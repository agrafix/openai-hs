{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp #-}

module OpenAI.Client
  ( -- * Basics
    ApiKey,
    OpenAIClient,
    makeOpenAIClient,
    makeOpenAIClient',
    ClientError (..),

    -- * Helper types
    TimeStamp (..),
    OpenAIList (..),
    Usage (..),

    -- * Models
    Model (..),
    ModelId (..),
    listModels,
    getModel,

    -- * Completion
    CompletionCreate (..),
    CompletionChoice (..),
    CompletionResponse (..),
    defaultCompletionCreate,
    completeText,

    -- * Chat
    ChatFunction (..),
    ChatFunctionCall (..),
    ChatFunctionCallStrategy (..),
    ChatMessage (..),
    ChatCompletionRequest (..),
    ChatChoice (..),
    ChatResponse (..),
    defaultChatCompletionRequest,
    completeChat,

    -- * Edits
    EditCreate (..),
    EditChoice (..),
    EditResponse (..),
    createTextEdit,
    defaultEditCreate,

    -- * Images
    ImageResponse (..),
    ImageResponseData (..),
    ImageCreate (..),
    ImageEditRequest (..),
    ImageVariationRequest (..),
    generateImage,
    createImageEdit,
    createImageVariation,

    -- * Embeddings
    EmbeddingCreate (..),
    EmbeddingResponseData (..),
    EmbeddingUsage (..),
    EmbeddingResponse (..),
    createEmbedding,

    -- * Audio
    AudioResponseData (..),
    AudioTranscriptionRequest (..),
    AudioTranslationRequest (..),
    createTranscription,
    createAudioTranslation,

    -- * Engine (deprecated)
    EngineId (..),
    Engine (..),
    listEngines,
    getEngine,

    -- * Engine-based text completion (deprecated)
    TextCompletionId (..),
    TextCompletionChoice (..),
    TextCompletion (..),
    TextCompletionCreate (..),
    defaultEngineTextCompletionCreate,
    engineCompleteText,

    -- * Engine-based embeddings (deprecated)
    EngineEmbeddingCreate (..),
    EngineEmbedding (..),
    engineCreateEmbedding,

    -- * Fine tunes (out of date)
    FineTuneId (..),
    FineTuneCreate (..),
    defaultFineTuneCreate,
    FineTune (..),
    FineTuneEvent (..),
    createFineTune,
    listFineTunes,
    getFineTune,
    cancelFineTune,
    listFineTuneEvents,

    -- * File API (out of date)
    FileCreate (..),
    File (..),
    FileId (..),
    FileHunk (..),
    FineTuneHunk (..),
    FileDeleteConfirmation (..),
    createFile,
    deleteFile,
  )
where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client (Manager)
import OpenAI.Api
import OpenAI.Client.Internal.Helpers
import OpenAI.Resources
import Servant.API
import Servant.Auth.Client
import Servant.Client
import qualified Servant.Multipart.Client as MP

-- | Your OpenAI API key. Can be obtained from the OpenAI dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data OpenAIClient = OpenAIClient
  { scBaseUrl :: BaseUrl,
    scToken :: Token,
    scManager :: Manager,
    scMaxRetries :: Int
  }

-- | Construct a 'OpenAIClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeOpenAIClient' ::
  BaseUrl ->
  ApiKey ->
  Manager ->
  -- | Number of automatic retries the library should attempt.
  Int ->
  OpenAIClient
makeOpenAIClient' u k = OpenAIClient u (Token (T.encodeUtf8 k))

-- | method using default remote base url
makeOpenAIClient ::
  ApiKey ->
  Manager ->
  Int ->
  OpenAIClient
makeOpenAIClient = makeOpenAIClient' openaiBaseUrl

api :: Proxy OpenAIApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.openai.com" 443 ""

#define EP0(N, R) \
    N##' :: Token -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> m (Either ClientError R);\
    N sc = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc)) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP1(N, ARG, R) \
    N##' :: Token -> ARG -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> m (Either ClientError R);\
    N sc a = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a) (mkClientEnv (scManager sc) (scBaseUrl sc))

#define EP2(N, ARG, ARG2, R) \
    N##' :: Token -> ARG -> ARG2 -> ClientM R;\
    N :: MonadIO m => OpenAIClient -> ARG -> ARG2 -> m (Either ClientError R);\
    N sc a b = liftIO . runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scToken sc) a b) (mkClientEnv (scManager sc) (scBaseUrl sc))

EP0 (listModels, (OpenAIList Model))
EP1 (getModel, ModelId, Model)

EP1 (completeText, CompletionCreate, CompletionResponse)

EP1 (completeChat, ChatCompletionRequest, ChatResponse)

EP1 (createTextEdit, EditCreate, EditResponse)

EP1 (generateImage, ImageCreate, ImageResponse)
EP1 (createImageEdit, ImageEditRequest, ImageResponse)
EP1 (createImageVariation, ImageVariationRequest, ImageResponse)

EP1 (createEmbedding, EmbeddingCreate, EmbeddingResponse)

createTranscription :: MonadIO m => OpenAIClient -> AudioTranscriptionRequest -> m (Either ClientError AudioResponseData)
createTranscription sc atr =
  do
    bnd <- liftIO MP.genBoundary
    createTranscriptionInternal sc (bnd, atr)

createAudioTranslation :: MonadIO m => OpenAIClient -> AudioTranslationRequest -> m (Either ClientError AudioResponseData)
createAudioTranslation sc atr =
  do
    bnd <- liftIO MP.genBoundary
    createAudioTranslationInternal sc (bnd, atr)

EP1 (createTranscriptionInternal, (BSL.ByteString, AudioTranscriptionRequest), AudioResponseData)
EP1 (createAudioTranslationInternal, (BSL.ByteString, AudioTranslationRequest), AudioResponseData)

createFile :: MonadIO m => OpenAIClient -> FileCreate -> m (Either ClientError File)
createFile sc rfc =
  do
    bnd <- liftIO MP.genBoundary
    createFileInternal sc (bnd, rfc)

EP1 (createFileInternal, (BSL.ByteString, FileCreate), File)
EP1 (deleteFile, FileId, FileDeleteConfirmation)

EP1 (createFineTune, FineTuneCreate, FineTune)
EP0 (listFineTunes, (OpenAIList FineTune))
EP1 (getFineTune, FineTuneId, FineTune)
EP1 (cancelFineTune, FineTuneId, FineTune)
EP1 (listFineTuneEvents, FineTuneId, (OpenAIList FineTuneEvent))

EP0 (listEngines, (OpenAIList Engine))
EP1 (getEngine, EngineId, Engine)
EP2 (engineCompleteText, EngineId, TextCompletionCreate, TextCompletion)
EP2 (engineCreateEmbedding, EngineId, EngineEmbeddingCreate, (OpenAIList EngineEmbedding))

( ( listModels'
      :<|> getModel'
    )
    :<|> (completeText')
    :<|> (completeChat')
    :<|> (createTextEdit')
    :<|> ( generateImage'
             :<|> createImageEdit'
             :<|> createImageVariation'
           )
    :<|> (createEmbedding')
    :<|> ( createTranscriptionInternal'
             :<|> createAudioTranslationInternal'
           )
    :<|> (createFileInternal' :<|> deleteFile')
    :<|> ( createFineTune'
             :<|> listFineTunes'
             :<|> getFineTune'
             :<|> cancelFineTune'
             :<|> listFineTuneEvents'
           )
    :<|> ( listEngines'
             :<|> getEngine'
             :<|> engineCompleteText'
             :<|> engineCreateEmbedding'
           )
  ) =
    client api
