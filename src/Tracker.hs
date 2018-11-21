module Tracker
  ( Tracker(..)
  , emptyTracker
  , trackerToHttpHeaders
  , trackerFromHttpHeaders
  , readTracker
  , writeTracker
  ) where

import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, encodeFile)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Header)
import RIO

data Tracker = Tracker
  { trackerEtag :: Maybe Text
  , trackerLastModified :: Maybe Text
  } deriving (Show, Generic, FromJSON, ToJSON)

emptyTracker :: Tracker
emptyTracker = Tracker Nothing Nothing

trackerToHttpHeaders :: Tracker -> [Header]
trackerToHttpHeaders Tracker {..} =
  catMaybes
    [ header "ETag" <$> trackerEtag
    , header "If-Modified-Since" <$> trackerLastModified
    ]
  where
    header :: ByteString -> Text -> Header
    header name val = (CI.mk name, encodeUtf8 val)

trackerFromHttpHeaders :: [Header] -> Tracker
trackerFromHttpHeaders headers =
  Tracker (getHeader "ETag") (getHeader "Last-Modified")
  where
    getHeader :: ByteString -> Maybe Text
    getHeader headerName = decodeUtf8 <$> lookup (CI.mk headerName) headers

readTracker :: MonadIO m => FilePath -> Text -> m Tracker
readTracker file url = do
  mTrackers <- liftIO $ catchIO (decodeFileStrict file) (const mempty)
  return $
    fromMaybe emptyTracker $ do
      trackers <- mTrackers
      Map.lookup url trackers

writeTracker :: MonadIO m => FilePath -> Text -> Tracker -> m ()
writeTracker file url tracker = do
  mTrackers <- liftIO $ catchIO (decodeFileStrict file) (const mempty)
  let trackers = fromMaybe mempty mTrackers
  liftIO $ encodeFile file (Map.insert url tracker trackers)
