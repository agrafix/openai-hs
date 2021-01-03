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
import qualified Data.Vector as V

main :: IO ()
main =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "OPENAI_KEY"
     -- create a openai client that automatically retries up to 4 times on network
     -- errors
     let client = makeOpenAIClient apiKey manager 4
     result <-
         searchDocuments cli (eId firstEngine) $
         SearchResultCreate
         { sccrDocuments = V.fromList ["pool", "gym", "night club"]
         , sccrQuery = "swimmer"
         }
     print result
```

## Features

Supported actions:

* List engines
* Retrieve engine
* Create text completion
* Run semantic search

## Running the tests

You can run all tests with `stack test`. You'll need an OpenAI API Key assigned to the `OPENAI_KEY` environment variable.
