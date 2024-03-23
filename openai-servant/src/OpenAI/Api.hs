-- | The API
module OpenAI.Api where

import OpenAI.Resources
import Servant.API
import Servant.Auth
import Servant.Auth.Client
import Servant.Multipart.API

type OpenAIAuth = Auth '[Bearer] ()

type OpenAIApi =
  "v1" :> OpenAIApiInternal

type OpenAIApiInternal =
  "models" :> ModelsApi
    :<|> "completions" :> CompletionsApi
    :<|> "chat" :> ChatApi
    :<|> "images" :> ImagesApi
    :<|> "embeddings" :> EmbeddingsApi
    :<|> "audio" :> AudioApi
    :<|> "files" :> FilesApi
    :<|> FineTuneApi
    :<|> "engines" :> EnginesApi

type ModelsApi =
  OpenAIAuth :> Get '[JSON] (OpenAIList Model)
    :<|> OpenAIAuth :> Capture "model_id" ModelId :> Get '[JSON] Model

type CompletionsApi =
  OpenAIAuth :> ReqBody '[JSON] CompletionCreate :> Post '[JSON] CompletionResponse

type ChatApi =
  OpenAIAuth :> "completions" :> ReqBody '[JSON] ChatCompletionRequest :> Post '[JSON] ChatResponse

type ImagesApi =
  OpenAIAuth :> "generations" :> ReqBody '[JSON] ImageCreate :> Post '[JSON] ImageResponse
    :<|> OpenAIAuth :> "edits" :> ReqBody '[JSON] ImageEditRequest :> Post '[JSON] ImageResponse
    :<|> OpenAIAuth :> "variations" :> ReqBody '[JSON] ImageVariationRequest :> Post '[JSON] ImageResponse

type EmbeddingsApi =
  OpenAIAuth :> ReqBody '[JSON] EmbeddingCreate :> Post '[JSON] EmbeddingResponse

type AudioApi =
  OpenAIAuth :> "transcriptions" :> MultipartForm Tmp AudioTranscriptionRequest :> Post '[JSON] AudioResponseData
    :<|> OpenAIAuth :> "translations" :> MultipartForm Tmp AudioTranslationRequest :> Post '[JSON] AudioResponseData

type FilesApi =
  OpenAIAuth :> MultipartForm Mem FileCreate :> Post '[JSON] File
    :<|> OpenAIAuth :> Capture "file_id" FileId :> Delete '[JSON] FileDeleteConfirmation

type FineTuneApi =
  OpenAIAuth :> "fine-tunes" :> ReqBody '[JSON] FineTuneCreate :> Post '[JSON] FineTune
    :<|> OpenAIAuth :> "fine-tunes" :> Get '[JSON] (OpenAIList FineTune)
    :<|> OpenAIAuth :> "fine-tunes" :> Capture "fine_tune_id" FineTuneId :> Get '[JSON] FineTune
    :<|> OpenAIAuth :> "fine-tunes" :> Capture "fine_tune_id" FineTuneId :> "cancel" :> Post '[JSON] FineTune
    :<|> OpenAIAuth :> "fine-tunes" :> Capture "fine_tune_id" FineTuneId :> "events" :> Get '[JSON] (OpenAIList FineTuneEvent)

type EnginesApi =
  OpenAIAuth :> Get '[JSON] (OpenAIList Engine)
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> Get '[JSON] Engine
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "completions" :> ReqBody '[JSON] TextCompletionCreate :> Post '[JSON] TextCompletion
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "embeddings" :> ReqBody '[JSON] EngineEmbeddingCreate :> Post '[JSON] (OpenAIList EngineEmbedding)
