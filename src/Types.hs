module Types where

import Data.Aeson
import qualified Data.Text as T
import RIO
import RIO.Time (NominalDiffTime)

data App = App
  { appLogFunc :: LogFunc
  , appConfig :: Config
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

data Config = Config
  { configTopics :: [Topic]
  , configTrackerFile :: FilePath
  , configOutputDir :: FilePath
  , configDiscardEntriesOlderThan :: Maybe NominalDiffTime
  } deriving (Generic)

data Topic = Topic
  { topicName :: Text
  , topicFeeds :: [Url]
  } deriving (Generic)

type Url = Text

jsonDecodeOptions :: Text -> Options
jsonDecodeOptions prefix =
  defaultOptions {fieldLabelModifier = camelTo2 '-' . drop (T.length prefix)}

instance FromJSON Topic where
  parseJSON = genericParseJSON (jsonDecodeOptions "topic")

instance FromJSON Config where
  parseJSON = genericParseJSON (jsonDecodeOptions "config")
