{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAI.Resources
  ( -- * Core Types
    TimeStamp (..),
    OpenAIList (..),
    Usage (..),

    -- * Models
    Model (..),
    ModelId (..),

    -- * Completion
    CompletionCreate (..),
    CompletionChoice (..),
    CompletionResponse (..),
    defaultCompletionCreate,

    -- * Chat
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatChoice (..),
    ChatResponse (..),
    defaultChatCompletionRequest,

    -- * Edits
    EditCreate (..),
    EditChoice (..),
    EditResponse (..),
    defaultEditCreate,

    -- * Images
    ImageResponse (..),
    ImageResponseData (..),
    ImageCreate (..),
    ImageEditRequest (..),
    ImageVariationRequest (..),

    -- * Embeddings
    EmbeddingCreate (..),
    EmbeddingResponseData (..),
    EmbeddingUsage (..),
    EmbeddingResponse (..),

    -- * Audio
    AudioResponseData (..),
    AudioTranscriptionRequest (..),
    AudioTranslationRequest (..),

    -- * Fine tuning (out of date)
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),

    -- * File API (out of date)
    FileCreate (..),
    FileId (..),
    File (..),
    FileHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),

    -- * Engine (deprecated)
    EngineId (..),
    Engine (..),

    -- * Engine text completion (deprecated)
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultEngineTextCompletionCreate,

    -- * Engine Embeddings (deprecated)
    EngineEmbeddingCreate (..),
    EngineEmbedding (..),
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Network.Mime (defaultMimeLookup)
import OpenAI.Internal.Aeson
import Servant.API
import Servant.Multipart.API

-- | A 'UTCTime' wrapper that has unix timestamp JSON representation
newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
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
newtype OpenAIList a = OpenAIList
  { olData :: V.Vector a
  }
  deriving (Show, Eq, Functor)

instance Semigroup (OpenAIList a) where
  (<>) a b = OpenAIList (olData a <> olData b)

instance Monoid (OpenAIList a) where
  mempty = OpenAIList mempty

instance Applicative OpenAIList where
  pure = OpenAIList . pure
  (<*>) go x = OpenAIList (olData go <*> olData x)

$(deriveJSON (jsonOpts 2) ''OpenAIList)

data Usage = Usage
  { usPromptTokens :: Int,
    usCompletionTokens :: Int,
    usTotalTokens :: Int
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''Usage)

------------------------
------ Model API
------------------------

data Model = Model
  { mId :: ModelId,
    mObject :: T.Text,
    mOwnedBy :: T.Text,
    mPermission :: [A.Object] -- TODO 2023.03.22: Docs do not say what this is
  }
  deriving (Show, Eq)

newtype ModelId = ModelId {unModelId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

$(deriveJSON (jsonOpts 1) ''Model)

------------------------
------ Completions API
------------------------

data CompletionCreate = CompletionCreate
  { ccrModel :: ModelId,
    ccrPrompt :: Maybe T.Text,
    ccrSuffix :: Maybe T.Text,
    ccrMaxTokens :: Maybe Int,
    ccrTemperature :: Maybe Double,
    ccrTopP :: Maybe Double,
    ccrN :: Maybe Int,
    ccrStream :: Maybe Bool,
    ccrLogprobs :: Maybe Int,
    ccrEcho :: Maybe Bool,
    ccrStop :: Maybe (V.Vector T.Text),
    ccrPresencePenalty :: Maybe Double,
    ccrFrequencyPenalty :: Maybe Double,
    ccrBestOf :: Maybe Int,
    ccrLogitBias :: Maybe (V.Vector Double),
    ccrUser :: Maybe String
  }
  deriving (Show, Eq)

defaultCompletionCreate :: ModelId -> T.Text -> CompletionCreate
defaultCompletionCreate model prompt =
  CompletionCreate
    { ccrModel = model,
      ccrPrompt = Just prompt,
      ccrSuffix = Nothing,
      ccrMaxTokens = Nothing,
      ccrTemperature = Nothing,
      ccrTopP = Nothing,
      ccrN = Nothing,
      ccrStream = Nothing,
      ccrLogprobs = Nothing,
      ccrEcho = Nothing,
      ccrStop = Nothing,
      ccrPresencePenalty = Nothing,
      ccrFrequencyPenalty = Nothing,
      ccrBestOf = Nothing,
      ccrLogitBias = Nothing,
      ccrUser = Nothing
    }

data CompletionChoice = CompletionChoice
  { cchText :: T.Text,
    cchIndex :: Int,
    cchLogprobs :: Maybe (V.Vector Double),
    cchFinishReason :: Maybe T.Text
  }
  deriving (Show, Eq)

data CompletionResponse = CompletionResponse
  { crId :: T.Text,
    crObject :: T.Text,
    crCreated :: Int,
    crModel :: ModelId,
    crChoices :: [CompletionChoice],
    crUsage :: A.Object
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''CompletionCreate)
$(deriveJSON (jsonOpts 3) ''CompletionChoice)
$(deriveJSON (jsonOpts 2) ''CompletionResponse)

------------------------
------ Chat API
------------------------

data ChatFunctionCall = ChatFunctionCall
  { chfcName :: T.Text,
    chfcArguments :: A.Value
  }
  deriving (Eq, Show)

instance A.FromJSON ChatFunctionCall where
  parseJSON = A.withObject "ChatFunctionCall" $ \obj -> do
    name <- obj A..: "name"
    arguments <- obj A..: "arguments" >>= A.withEmbeddedJSON "Arguments" pure

    pure $ ChatFunctionCall {chfcName = name, chfcArguments = arguments}

instance A.ToJSON ChatFunctionCall where
  toJSON (ChatFunctionCall {chfcName = name, chfcArguments = arguments}) =
    A.object
      [ "name" A..= name,
        "arguments" A..= T.decodeUtf8 (BSL.toStrict (A.encode arguments))
      ]

data ChatMessage = ChatMessage
  { chmContent :: Maybe T.Text,
    chmRole :: T.Text,
    chmFunctionCall :: Maybe ChatFunctionCall,
    chmName :: Maybe T.Text
  }
  deriving (Show, Eq)

instance A.FromJSON ChatMessage where
  parseJSON = A.withObject "ChatMessage" $ \obj ->
    ChatMessage <$> obj A..:? "content"
                <*> obj A..: "role"
                <*> obj A..:? "function_call"
                <*> obj A..:? "name"

instance A.ToJSON ChatMessage where
  toJSON (ChatMessage {chmContent = content, chmRole = role, chmFunctionCall = functionCall, chmName = name}) =
    A.object $ 
      [ "content" A..= content,
        "role" A..= role
      ] ++ catMaybes
      [ ("function_call" A..=) <$> functionCall, 
        ("name" A..=) <$> name
      ]
      
data ChatFunction = ChatFunction
  { chfName :: T.Text,
    chfDescription :: T.Text,
    chfParameters :: A.Value
  }
  deriving (Show, Eq)

data ChatFunctionCallStrategy =
    CFCS_auto
  | CFCS_none
  | CFCS_name T.Text
  deriving (Show, Eq)

instance ToJSON ChatFunctionCallStrategy where
  toJSON = \case
    CFCS_auto              -> A.String "auto"
    CFCS_none              -> A.String "none"
    CFCS_name functionName -> A.object [ "name" A..= A.toJSON functionName ]

instance FromJSON ChatFunctionCallStrategy where
  parseJSON (A.String "auto") = pure CFCS_auto
  parseJSON (A.String "none") = pure CFCS_none
  parseJSON xs = flip (A.withObject "ChatFunctionCallStrategy") xs $ \o -> do
    functionName <- o A..: "name"
    pure $ CFCS_name functionName

data ChatCompletionRequest = ChatCompletionRequest
  { chcrModel :: ModelId,
    chcrMessages :: [ChatMessage],
    chcrFunctions :: Maybe [ChatFunction],
    chcrFunctionCall :: Maybe ChatFunctionCallStrategy,
    chcrTemperature :: Maybe Double,
    chcrTopP :: Maybe Double,
    chcrN :: Maybe Int,
    chcrStream :: Maybe Bool,
    chcrStop :: Maybe (V.Vector T.Text),
    chcrMaxTokens :: Maybe Int,
    chcrPresencePenalty :: Maybe Double,
    chcrFrequencyPenalty :: Maybe Double,
    chcrLogitBias :: Maybe (V.Vector Double),
    chcrUser :: Maybe String
  }
  deriving (Show, Eq)

defaultChatCompletionRequest :: ModelId -> [ChatMessage] -> ChatCompletionRequest
defaultChatCompletionRequest model messages =
  ChatCompletionRequest
    { chcrModel = model,
      chcrMessages = messages,
      chcrFunctions = Nothing,
      chcrFunctionCall = Nothing,
      chcrTemperature = Nothing,
      chcrTopP = Nothing,
      chcrN = Nothing,
      chcrStream = Nothing,
      chcrStop = Nothing,
      chcrMaxTokens = Nothing,
      chcrPresencePenalty = Nothing,
      chcrFrequencyPenalty = Nothing,
      chcrLogitBias = Nothing,
      chcrUser = Nothing
    }

data ChatChoice = ChatChoice
  { chchIndex :: Int,
    chchMessage :: ChatMessage,
    chchFinishReason :: Maybe T.Text
  }
  deriving (Show, Eq)

data ChatResponse = ChatResponse
  { chrId :: T.Text,
    chrObject :: T.Text,
    chrCreated :: Int,
    chrChoices :: [ChatChoice],
    chrUsage :: Usage
  }

$(deriveJSON (jsonOpts 3) ''ChatFunction)
$(deriveJSON (jsonOpts 4) ''ChatCompletionRequest)
$(deriveJSON (jsonOpts 4) ''ChatChoice)
$(deriveJSON (jsonOpts 3) ''ChatResponse)

------------------------
------ Edits API
------------------------

data EditCreate = EditCreate
  { edcrModel :: ModelId,
    edcrInput :: Maybe T.Text,
    edcrInstruction :: T.Text,
    edcrN :: Maybe Int,
    edcrTemperature :: Maybe Double,
    edcrTopP :: Maybe Double
  }
  deriving (Show, Eq)

defaultEditCreate :: ModelId -> T.Text -> T.Text -> EditCreate
defaultEditCreate model input instruction =
  EditCreate
    { edcrModel = model,
      edcrInput = Just input,
      edcrInstruction = instruction,
      edcrN = Nothing,
      edcrTemperature = Nothing,
      edcrTopP = Nothing
    }

data EditChoice = EditChoice
  { edchText :: T.Text,
    edchIndex :: Int
  }
  deriving (Show, Eq)

data EditResponse = EditResponse
  { edrObject :: T.Text,
    edrCreated :: Int,
    edrChoices :: [EditChoice],
    edrUsage :: Usage
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''EditCreate)
$(deriveJSON (jsonOpts 4) ''EditChoice)
$(deriveJSON (jsonOpts 3) ''EditResponse)

------------------------
------ Images API
------------------------

data ImageResponseData = ImageResponseData
  { irdUrl :: T.Text
  }
  deriving (Show, Eq)

data ImageResponse = ImageResponse
  { irCreated :: TimeStamp,
    irData :: [ImageResponseData]
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageResponseData)
$(deriveJSON (jsonOpts 2) ''ImageResponse)

-- | Image create API
data ImageCreate = ImageCreate
  { icPrompt :: T.Text,
    icN :: Maybe Int,
    icSize :: Maybe T.Text,
    icResponseFormat :: Maybe T.Text,
    icUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 2) ''ImageCreate)

-- | Image edit API
data ImageEditRequest = ImageEditRequest
  { ierImage :: T.Text,
    ierMask :: Maybe T.Text,
    ierPrompt :: T.Text,
    ierN :: Maybe Int,
    ierSize :: Maybe T.Text,
    ierResponseFormat :: Maybe T.Text,
    ierUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageEditRequest)

-- | Image variation API
data ImageVariationRequest = ImageVariationRequest
  { ivrImage :: T.Text,
    ivrN :: Maybe Int,
    ivrSize :: Maybe T.Text,
    ivrResponseFormat :: Maybe T.Text,
    ivrUser :: Maybe T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''ImageVariationRequest)

------------------------
------ Embeddings API
------------------------

data EmbeddingCreate = EmbeddingCreate
  { embcModel :: ModelId,
    embcInput :: T.Text, -- TODO (2023.02.23): Extend to allow taking in array of strings or token arrays
    embcUser :: Maybe T.Text
  }
  deriving (Show, Eq)

data EmbeddingResponseData = EmbeddingResponseData
  { embdObject :: T.Text,
    embdEmbedding :: V.Vector Double,
    embdIndex :: Int
  }
  deriving (Show, Eq)

data EmbeddingUsage = EmbeddingUsage
  { embuPromptTokens :: Int,
    embuTotalTokens :: Int
  }
  deriving (Show, Eq)

data EmbeddingResponse = EmbeddingResponse
  { embrObject :: T.Text,
    embrData :: [EmbeddingResponseData],
    embrModel :: ModelId,
    embrUsage :: EmbeddingUsage
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''EmbeddingCreate)
$(deriveJSON (jsonOpts 4) ''EmbeddingResponseData)
$(deriveJSON (jsonOpts 4) ''EmbeddingUsage)
$(deriveJSON (jsonOpts 4) ''EmbeddingResponse)

------------------------
------ Audio API
------------------------

data AudioResponseData = AudioResponseData
  { audrdText :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 5) ''AudioResponseData)

-- | Audio create API
data AudioTranscriptionRequest = AudioTranscriptionRequest
  { audtsrFile :: FilePath,
    audtsrModel :: ModelId,
    audtsrPrompt :: Maybe T.Text,
    audtsrResponseFormat :: Maybe T.Text,
    audtsrTemperature :: Maybe Double,
    audtsrLanguage :: Maybe T.Text
  }
  deriving (Show, Eq)

instance ToMultipart Tmp AudioTranscriptionRequest where
  toMultipart atr =
    MultipartData
      ( catMaybes
          [ Input "model" . unModelId <$> Just (audtsrModel atr),
            Input "prompt" <$> audtsrPrompt atr,
            Input "response_format" <$> audtsrResponseFormat atr,
            Input "temperature" . T.pack . show <$> audtsrTemperature atr,
            Input "language" <$> audtsrLanguage atr
          ]
      )
      [ FileData "file" (T.pack . audtsrFile $ atr) (T.decodeUtf8 . defaultMimeLookup . T.pack $ audtsrFile atr) (audtsrFile atr)
      ]

$(deriveJSON (jsonOpts 6) ''AudioTranscriptionRequest)

-- | Audio translation API
data AudioTranslationRequest = AudioTranslationRequest
  { audtlrFile :: FilePath,
    audtlrModel :: ModelId,
    audtlrPrompt :: Maybe T.Text,
    audtlrResponseFormat :: Maybe T.Text,
    audtlrTemperature :: Maybe Double
  }
  deriving (Show, Eq)

instance ToMultipart Tmp AudioTranslationRequest where
  toMultipart atr =
    MultipartData
      ( catMaybes
          [ Input "model" . unModelId <$> Just (audtlrModel atr),
            Input "prompt" <$> audtlrPrompt atr,
            Input "response_format" <$> audtlrResponseFormat atr,
            Input "temperature" . T.pack . show <$> audtlrTemperature atr
          ]
      )
      [ FileData "file" (T.pack . audtlrFile $ atr) (T.decodeUtf8 . defaultMimeLookup . T.pack $ audtlrFile atr) (audtlrFile atr)
      ]

$(deriveJSON (jsonOpts 6) ''AudioTranslationRequest)

------------------------
------ Files API
------------------------

data FineTuneHunk = FineTuneHunk
  { fthPrompt :: T.Text,
    fthCompletion :: T.Text
  }
  deriving (Show, Eq)

data FileHunk
  = FhFineTune FineTuneHunk
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''FineTuneHunk)

newtype FileId = FileId {unFileId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data File = File
  { fId :: FileId,
    fObject :: T.Text,
    fBytes :: Int,
    fCreatedAt :: TimeStamp,
    fFilename :: T.Text,
    fPurpose :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 1) ''File)

-- | File upload API
data FileCreate = FileCreate
  { fcPurpose :: T.Text,
    fcDocuments :: [FileHunk]
  }
  deriving (Show, Eq)

packDocuments :: [FileHunk] -> BSL.ByteString
packDocuments docs =
  BSL.intercalate "\n" $
    map
      ( \t -> A.encode $
          case t of
            FhFineTune x -> A.toJSON x
      )
      docs

instance ToMultipart Mem FileCreate where
  toMultipart rfc =
    MultipartData
      [ Input "purpose" (fcPurpose rfc)
      ]
      [ FileData "file" "data.jsonl" "application/json" (packDocuments $ fcDocuments rfc)
      ]

-- | File delete API
data FileDeleteConfirmation = FileDeleteConfirmation
  { fdcId :: FileId
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''FileDeleteConfirmation)

-- | File retrieve API
-- TODO

-- | File retrieve content API
-- TODO

------------------------
------ Engine API (deprecated)
------------------------

newtype EngineId = EngineId {unEngineId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data Engine = Engine
  { eId :: EngineId,
    eOwner :: T.Text,
    eReady :: Bool
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 1) ''Engine)

------------------------
------ Engine completions API (deprecated)
------------------------

newtype TextCompletionId = TextCompletionId {unTextCompletionId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data TextCompletionChoice = TextCompletionChoice
  { tccText :: T.Text,
    tccIndex :: Int,
    tccLogProps :: Maybe Int,
    tccFinishReason :: T.Text
  }
  deriving (Show, Eq)

data TextCompletion = TextCompletion
  { tcId :: TextCompletionId,
    tcCreated :: TimeStamp,
    tcModel :: T.Text,
    tcChoices :: V.Vector TextCompletionChoice
  }
  deriving (Show, Eq)

data TextCompletionCreate = TextCompletionCreate
  { tccrPrompt :: T.Text, -- TODO: support lists of strings
    tccrMaxTokens :: Maybe Int,
    tccrTemperature :: Maybe Double,
    tccrTopP :: Maybe Double,
    tccrN :: Maybe Int,
    tccrLogprobs :: Maybe Int,
    tccrEcho :: Maybe Bool,
    tccrStop :: Maybe (V.Vector T.Text),
    tccrPresencePenalty :: Maybe Double,
    tccrFrequencyPenalty :: Maybe Double,
    tccrBestOf :: Maybe Int
  }
  deriving (Show, Eq)

-- | Applies API defaults, only passing a prompt.
defaultEngineTextCompletionCreate :: T.Text -> TextCompletionCreate
defaultEngineTextCompletionCreate prompt =
  TextCompletionCreate
    { tccrPrompt = prompt,
      tccrMaxTokens = Nothing,
      tccrTemperature = Nothing,
      tccrTopP = Nothing,
      tccrN = Nothing,
      tccrLogprobs = Nothing,
      tccrEcho = Nothing,
      tccrStop = Nothing,
      tccrPresencePenalty = Nothing,
      tccrFrequencyPenalty = Nothing,
      tccrBestOf = Nothing
    }

$(deriveJSON (jsonOpts 3) ''TextCompletionChoice)
$(deriveJSON (jsonOpts 2) ''TextCompletion)
$(deriveJSON (jsonOpts 4) ''TextCompletionCreate)

------------------------
------ EngineEmbeddings API (deprecated)
------------------------

data EngineEmbeddingCreate = EngineEmbeddingCreate
  {enecInput :: T.Text}
  deriving (Show, Eq)

data EngineEmbedding = EngineEmbedding
  {eneEmbedding :: V.Vector Double, eneIndex :: Int}
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 4) ''EngineEmbeddingCreate)
$(deriveJSON (jsonOpts 3) ''EngineEmbedding)

------------------------
------ Old stuff; not touching
------ TODO 2023.03.22: Not touching this; unchanged since last year
------------------------

newtype FineTuneId = FineTuneId {unFineTuneId :: T.Text}
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData)

data FineTuneCreate = FineTuneCreate
  { ftcTrainingFile :: FileId,
    ftcValidationFile :: Maybe FileId,
    ftcModel :: Maybe T.Text,
    ftcBatchSize :: Maybe Int,
    ftcNEpochs :: Maybe T.Text,
    ftcLearningRateMultiplier :: Maybe Double,
    ftcPromptLossWeight :: Maybe Double,
    ftcComputeClassificationMetrics :: Maybe Bool,
    ftcClassificationNClasses :: Maybe Int,
    ftcClassificationPositiveClass :: Maybe T.Text
  }
  deriving (Show, Eq)

defaultFineTuneCreate :: FileId -> FineTuneCreate
defaultFineTuneCreate file =
  FineTuneCreate
    { ftcTrainingFile = file,
      ftcValidationFile = Nothing,
      ftcModel = Nothing,
      ftcBatchSize = Nothing,
      ftcNEpochs = Nothing,
      ftcLearningRateMultiplier = Nothing,
      ftcPromptLossWeight = Nothing,
      ftcComputeClassificationMetrics = Nothing,
      ftcClassificationNClasses = Nothing,
      ftcClassificationPositiveClass = Nothing
    }

data FineTuneEvent = FineTuneEvent
  { fteCreatedAt :: Int,
    fteLevel :: T.Text,
    fteMessage :: T.Text
  }
  deriving (Show, Eq)

data FineTune = FineTune
  { ftId :: FineTuneId,
    ftModel :: T.Text,
    ftCreatedAt :: Int,
    ftEvents :: V.Vector FineTuneEvent,
    ftTunedModel :: Maybe T.Text,
    ftStatus :: T.Text
  }
  deriving (Show, Eq)

$(deriveJSON (jsonOpts 3) ''FineTuneCreate)
$(deriveJSON (jsonOpts 3) ''FineTuneEvent)
$(deriveJSON (jsonOpts 2) ''FineTune)
