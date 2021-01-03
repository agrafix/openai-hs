{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenAI.Resources
  ( -- * Core Types
    TimeStamp(..), OpenAIList(..)
    -- * Engine
  , EngineId(..), Engine(..)
    -- * Text completion
  , TextCompletionId(..), TextCompletionChoice(..), TextCompletion(..), TextCompletionCreate(..)
  , defaultTextCompletionCreate
    -- * Searching
  , SearchResult(..), SearchResultCreate(..)
  )
where

import OpenAI.Internal.Aeson

import Data.Time
import Data.Time.Clock.POSIX
import Servant.API
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp
  = TimeStamp { unTimeStamp :: UTCTime }
  deriving (Show, Eq)

instance A.ToJSON TimeStamp where
  toJSON = A.Number . fromRational . toRational . utcTimeToPOSIXSeconds . unTimeStamp

instance A.FromJSON TimeStamp where
  parseJSON =
    A.withScientific "unix timestamp" $ \sci ->
    pure $ TimeStamp $ posixSecondsToUTCTime (fromRational $ toRational sci)

instance ToHttpApiData TimeStamp where
  toUrlPiece x =
    let unix :: Int
        unix = round . utcTimeToPOSIXSeconds . unTimeStamp $ x
    in T.pack (show unix)

-- | A 'V.Vector' wrapper.
newtype OpenAIList a
  = OpenAIList
  { olData :: V.Vector a
  } deriving (Show, Eq, Functor)

instance Semigroup (OpenAIList a) where
 (<>) a b = OpenAIList (olData a <> olData b)

instance Monoid (OpenAIList a) where
  mempty = OpenAIList mempty

instance Applicative OpenAIList where
  pure = OpenAIList . pure
  (<*>) go x = OpenAIList (olData go <*> olData x)

newtype EngineId
  = EngineId { unEngineId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Engine
  = Engine
  { eId :: EngineId
  , eOwner :: T.Text
  , eReady :: Bool
  } deriving (Show, Eq)


newtype TextCompletionId
  = TextCompletionId { unTextCompletionId :: T.Text }
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data TextCompletionChoice
  = TextCompletionChoice
  { tccText :: T.Text
  , tccIndex :: Int
  , tccLogProps :: Maybe Int
  , tccFinishReason :: T.Text
  } deriving (Show, Eq)

data TextCompletion
  = TextCompletion
  { tcId :: TextCompletionId
  , tcCreated :: TimeStamp
  , tcModel :: T.Text
  , tcChoices :: V.Vector TextCompletionChoice
  } deriving (Show, Eq)

data TextCompletionCreate
  = TextCompletionCreate
  { tccrPrompt :: T.Text -- TODO: support lists of strings
  , tccrMaxTokens :: Maybe Int
  , tccrTemperature :: Maybe Double
  , tccrTopP :: Maybe Double
  , tccrN :: Maybe Int
  , tccrLogprobs :: Maybe Int
  , tccrEcho :: Maybe Bool
  , tccrStop :: Maybe (V.Vector T.Text)
  , tccrPresencePenalty :: Maybe Double
  , tccrFrequencyPenalty :: Maybe Double
  , tccrBestOf :: Maybe Int
  } deriving (Show, Eq)

-- | Applies API defaults, only passing a prompt.
defaultTextCompletionCreate :: T.Text -> TextCompletionCreate
defaultTextCompletionCreate prompt =
  TextCompletionCreate
  { tccrPrompt = prompt
  , tccrMaxTokens = Nothing
  , tccrTemperature = Nothing
  , tccrTopP = Nothing
  , tccrN = Nothing
  , tccrLogprobs = Nothing
  , tccrEcho = Nothing
  , tccrStop = Nothing
  , tccrPresencePenalty = Nothing
  , tccrFrequencyPenalty = Nothing
  , tccrBestOf = Nothing
  }

data SearchResult
  = SearchResult
  { srDocument :: Int
  , srScore :: Double
  } deriving (Show, Eq)

data SearchResultCreate
  = SearchResultCreate
  { sccrDocuments :: V.Vector T.Text
  , sccrQuery :: T.Text
  } deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''OpenAIList)
$(deriveJSON (jsonOpts 1) ''Engine)
$(deriveJSON (jsonOpts 2) ''TextCompletion)
$(deriveJSON (jsonOpts 3) ''TextCompletionChoice)
$(deriveJSON (jsonOpts 4) ''TextCompletionCreate)
$(deriveJSON (jsonOpts 2) ''SearchResult)
$(deriveJSON (jsonOpts 4) ''SearchResultCreate)
