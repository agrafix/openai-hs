-- | The API

module OpenAI.Api where

import OpenAI.Resources

import Servant.API

type OpenAIAuth = BasicAuth "OpenAI API" ()

type OpenAIApi
  = "v1" :> OpenAIApiInternal

type OpenAIApiInternal
  = "engines" :> EnginesApi

type EnginesApi
  = OpenAIAuth :> Get '[JSON] (OpenAIList Engine)
  :<|> OpenAIAuth :> Capture "engine_id" EngineId :> Get '[JSON] Engine
  :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "completions" :> ReqBody '[JSON] TextCompletionCreate :> Post '[JSON] TextCompletion
  :<|> OpenAIAuth :> Capture "engine_id" EngineId :> "search" :> ReqBody '[JSON] SearchResultCreate :> Post '[JSON] (OpenAIList SearchResult)
