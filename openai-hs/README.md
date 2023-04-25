# openai-hs

Unofficial OpenAI SDK/client for Haskell. It's generated via `servant-client` from `openai-servant` with a small amount of hand-written code. Contributions are welcome!

## Install

``` sh
# stack
stack install openai-hs

# cabal
cabal install openai-hs
```

## Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import OpenAI.Client

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import qualified Data.Text as T

request :: ChatCompletionRequest
request = ChatCompletionRequest 
         { chcrModel = ModelId "gpt-3.5-turbo"
         , chcrMessages = 
            [ChatMessage { chmContent = "Write a hello world program in Haskell"
                         , chmRole = "user"
                         }
            ]
         , chcrTemperature = Nothing
         , chcrTopP = Nothing
         , chcrN = Nothing
         , chcrStream = Nothing
         , chcrStop = Nothing
         , chcrMaxTokens = Nothing
         , chcrPresencePenalty = Nothing
         , chcrFrequencyPenalty = Nothing
         , chcrLogitBias = Nothing
         , chcrUser = Nothing
         }

main :: IO ()
main =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "OPENAI_KEY"
     -- create a openai client that automatically retries up to 4 times on network
     -- errors
     let client = makeOpenAIClient apiKey manager 4
     result <- completeChat client request        
     case result of
       Left failure -> print failure
       Right success -> print $ chrChoices success
```

## Features

Supported actions:

* List engines
* Retrieve engine
* Create text completion
* Run semantic search

## Running the tests

You can run all tests with `stack test`. You'll need an OpenAI API Key assigned to the `OPENAI_KEY` environment variable.
