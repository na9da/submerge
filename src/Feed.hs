module Feed
  ( Feed
  , newRSSFeed
  , mergeFeeds
  , fetchFeed
  , writeFeed
  , readFeed
  , discardFeedItemsBefore
  , entryCount
  ) where

import RIO
import Tracker

import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.XML.Types as XT
import Network.HTTP.Client (Request(..), Response(..))
import Network.HTTP.Simple (HttpException(..), httpLBS, parseRequest)
import Network.HTTP.Types (notModified304, ok200)
import RIO.Time (UTCTime)
import qualified Text.Feed.Constructor as Feed
import qualified Text.Feed.Export as Feed
import qualified Text.Feed.Import as Feed
import qualified Text.Feed.Query as Feed
import Text.Feed.Types (Feed, Item)
import qualified Text.XML as XC

newRSSFeed :: Feed
newRSSFeed = Feed.newFeed (Feed.RSSKind Nothing)

entryCount :: Feed -> Int
entryCount = length . Feed.feedItems

instance Eq Item where
  a == b =
    fromMaybe False $ do
      (_, ida) <- Feed.getItemId a
      (_, idb) <- Feed.getItemId b
      return $ ida == idb

mergeFeeds :: Feed -> [Feed] -> Feed
mergeFeeds target feeds =
  let items = foldr merge2 (Feed.feedItems target) feeds
   in Feed.withFeedItems (nub items) target
  where
    merge2 :: Feed -> [Item] -> [Item]
    merge2 src sink = foldr ((:) . castItem src) sink (Feed.feedItems src)
    castItem :: Feed -> Item -> Item
    castItem parentFeed item =
      Feed.withItemTitle (fromMaybe "" (Feed.getItemTitle item)) $
      Feed.withItemLink (fromMaybe "" (Feed.getItemLink item)) $
      Feed.withItemDate (fromMaybe "" (Feed.getItemPublishDateString item)) $
      uncurry Feed.withItemId (fromMaybe (False, "") (Feed.getItemId item)) $
      Feed.newItem (Feed.getFeedKind target)

fetchFeed ::
     (MonadIO m, MonadThrow m)
  => Text
  -> Tracker
  -> m (Either Text (Feed, Tracker))
fetchFeed url tracker = do
  req <- parseRequest $ "GET " <> T.unpack url
  res <- httpLBS $ req {requestHeaders = trackerToHttpHeaders tracker}
  let tracker' = trackerFromHttpHeaders (responseHeaders res)
  return $
    case responseStatus res of
      status
        | status == notModified304 -> Left "Feed not modified since last check"
        | status /= ok200 -> Left $ "Fetch failed with status " <> tshow status
        | otherwise ->
          case Feed.parseFeedSource (responseBody res) of
            Nothing -> Left "Failed to parse feed"
            Just feed -> Right (feed, tracker')

writeFeed :: MonadIO m => FilePath -> Feed -> m ()
writeFeed outFile feed = do
  case renderFeed feed of
    Nothing -> return ()
    Just text -> liftIO $ BL.writeFile outFile text

readFeed :: MonadIO m => FilePath -> m (Maybe Feed)
readFeed inFile =
  liftIO $
  catchIO (Just <$> Feed.parseFeedFromFile inFile) (const (return Nothing))

renderFeed :: Feed -> Maybe BL.ByteString
renderFeed feed =
  let e = Feed.xmlFeed feed
      d = elToDoc e
   in XC.renderLBS XC.def <$> d
  where
    elToDoc :: XT.Element -> Maybe XC.Document
    elToDoc el =
      let txd = XT.Document (XC.Prologue [] Nothing []) el []
          cxd = XC.fromXMLDocument txd
       in either (const Nothing) Just cxd

discardFeedItemsBefore :: UTCTime -> Feed -> Feed
discardFeedItemsBefore date feed = do
  let feedItems = filter (isLaterThan date) (Feed.feedItems feed)
   in Feed.withFeedItems feedItems feed
  where
    isLaterThan :: UTCTime -> Item -> Bool
    isLaterThan date item =
      fromMaybe True $ do
        itemDate <- join $ Feed.getItemPublishDate item
        return $ itemDate > date
