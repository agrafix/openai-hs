{-# OPTIONS_GHC -cpp -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}
module OpenAI.Client
  ( -- * Basics
    ApiKey, OpenAIClient, makeOpenAIClient, ClientError(..)
    -- * Helper types
  , TimeStamp(..), OpenAIList(..)
    -- * Engine
  , EngineId(..), Engine(..)
  , listEngines
  , getEngine
    -- * Text completion
  , TextCompletionId(..), TextCompletionChoice(..), TextCompletion(..), TextCompletionCreate(..)
  , defaultTextCompletionCreate
  , completeText
    -- * Searching
  , SearchResult(..), SearchResultCreate(..)
  , searchDocuments
  )
where

import OpenAI.Api
import OpenAI.Client.Internal.Helpers
import OpenAI.Resources

import Data.Proxy
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Your OpenAI API key. Can be obtained from the OpenAI dashboard. Format: @sk-<redacted>@
type ApiKey = T.Text

-- | Holds a 'Manager' and your API key.
data OpenAIClient
  = OpenAIClient
  { scBasicAuthData :: BasicAuthData
  , scManager :: Manager
  , scMaxRetries :: Int
  }

-- | Construct a 'OpenAIClient'. Note that the passed 'Manager' must support https (e.g. via @http-client-tls@)
makeOpenAIClient ::
  ApiKey
  -> Manager
  -> Int
  -- ^ Number of automatic retries the library should attempt.
  -> OpenAIClient
makeOpenAIClient k = OpenAIClient (BasicAuthData "" (T.encodeUtf8 k))

api :: Proxy OpenAIApi
api = Proxy

openaiBaseUrl :: BaseUrl
openaiBaseUrl = BaseUrl Https "api.openai.com" 443 ""

#define EP0(N, R) \
    N##' :: BasicAuthData -> ClientM R;\
    N :: OpenAIClient -> IO (Either ClientError R);\
    N sc = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc)) (mkClientEnv (scManager sc) openaiBaseUrl)

#define EP(N, ARG, R) \
    N##' :: BasicAuthData -> ARG -> ClientM R;\
    N :: OpenAIClient -> ARG -> IO (Either ClientError R);\
    N sc a = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a) (mkClientEnv (scManager sc) openaiBaseUrl)

#define EP2(N, ARG, ARG2, R) \
    N##' :: BasicAuthData -> ARG -> ARG2 -> ClientM R;\
    N :: OpenAIClient -> ARG -> ARG2 -> IO (Either ClientError R);\
    N sc a b = runRequest (scMaxRetries sc) 0 $ runClientM (N##' (scBasicAuthData sc) a b) (mkClientEnv (scManager sc) openaiBaseUrl)

EP2(completeText, EngineId, TextCompletionCreate, TextCompletion)
EP2(searchDocuments, EngineId, SearchResultCreate, (OpenAIList SearchResult))

EP0(listEngines, (OpenAIList Engine))
EP(getEngine, EngineId, Engine)

listEngines'
  :<|> getEngine'
  :<|> completeText'
  :<|> searchDocuments'
  = client api
