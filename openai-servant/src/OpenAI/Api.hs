-- | The API
module OpenAI.Api where

import OpenAI.Resources
import Servant.API
import Servant.Multipart.API

type OpenAIAuth = BasicAuth "OpenAI API" ()

type OpenAIApi =
  "v1" :> OpenAIApiInternal

type OpenAIApiInternal =
  "engines" :> EnginesApi
    :<|> "files" :> FilesApi
    :<|> AnswerApi

type FilesApi =
  OpenAIAuth :> MultipartForm Mem FileCreate :> Post '[JSON] File
    :<|> OpenAIAuth :> Capture "file_id" FileId :> Delete '[JSON] FileDeleteConfirmation

type AnswerApi =
  "answers" :> OpenAIAuth :> ReqBody '[JSON] AnswerReq :> Post '[JSON] AnswerResp

type EnginesApi =
  OpenAIAuth :> Get '[JSON] (OpenAIList Engine)
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> Get '[JSON] Engine
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "completions" :> ReqBody '[JSON] TextCompletionCreate :> Post '[JSON] TextCompletion
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "search" :> ReqBody '[JSON] SearchResultCreate :> Post '[JSON] (OpenAIList SearchResult)
    :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "embeddings" :> ReqBody '[JSON] EmbeddingCreate :> Post '[JSON] (OpenAIList Embedding)
