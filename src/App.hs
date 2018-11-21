module App where

import Feed
import RIO
import Tracker
import Types

import qualified Data.Text as T
import Data.Yaml (decodeFileThrow)
import qualified RIO.Time as Time
import System.FilePath ((</>), addExtension)
import System.IO.Error (IOError(..))

topicFilePath :: Text -> RIO App FilePath
topicFilePath topicName = do
  Config {..} <- asks appConfig
  return $ configOutputDir </> addExtension (T.unpack topicName) "xml"

discardOldEntries :: Feed -> RIO App Feed
discardOldEntries feed = do
  Config {..} <- asks appConfig
  case configDiscardEntriesOlderThan of
    Nothing -> return feed
    Just days -> do
      now <- Time.getCurrentTime
      let date = Time.addUTCTime (-days * Time.nominalDay) now
      return $ discardFeedItemsBefore date feed

fetchTopic :: Topic -> RIO App Feed
fetchTopic Topic {..} = do
  Config {..} <- asks appConfig
  feeds <-
    forM topicFeeds $ \url -> do
      logInfo $ "Fetching " <> display url
      tracker <- readTracker configTrackerFile url
      fetchFeed url tracker >>= \case
        Left e -> logError (display e) *> return Nothing
        Right (feed, tracker) -> do
          writeTracker configTrackerFile url tracker
          return (Just feed)
  topicFile <- topicFilePath topicName
  currentFeed <- fromMaybe newRSSFeed <$> readFeed topicFile
  discardOldEntries $ mergeFeeds currentFeed (catMaybes feeds)

writeTopic :: Topic -> Feed -> RIO App ()
writeTopic Topic {..} feed = do
  topicFile <- topicFilePath topicName
  writeFeed topicFile feed

appLoop :: RIO App ()
appLoop = do
  Config {..} <- asks appConfig
  forever $ do
    forM_ configTopics $ \topic@Topic {..} -> do
      logInfo $ "Fetching " <> display topicName <> " feeds"
      feed <- fetchTopic topic
      topicFile <- topicFilePath topicName
      logInfo $
        "Writing " <> display (entryCount feed) <> " entries to " <>
        displayShow topicFile
      writeTopic topic feed
    sleepSecs 3600
  where
    sleepSecs = threadDelay . (* 1000000)

main :: IO ()
main = do
  appConfig <- decodeFileThrow "config.yaml"
  let isVerbose = False
  logOptions' <- logOptionsHandle stdout isVerbose
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \lf -> do
    let app = App lf appConfig
    runRIO app $ do
      logInfo "Starting app"
      appLoop
