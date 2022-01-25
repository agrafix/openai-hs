{-# LANGUAGE BangPatterns #-}

-- | Private helper functions. Note that all contents of this module are excluded
-- from the versioning scheme.
module OpenAI.Client.Internal.Helpers where

import Network.HTTP.Types.Status
import Servant.Client

runRequest :: Int -> Int -> IO (Either ClientError a) -> IO (Either ClientError a)
runRequest maxRetries !retryCount makeRequest =
  do
    res <- makeRequest
    case res of
      Right ok -> pure (Right ok)
      Left err@(ConnectionError _) -> maybeRetry err
      Left err@(FailureResponse _ resp)
        | responseStatusCode resp == conflict409 -> maybeRetry err
        | statusCode (responseStatusCode resp) >= 500 -> maybeRetry err
        | otherwise -> pure (Left err)
      Left err -> pure (Left err)
  where
    maybeRetry err =
      if retryCount + 1 >= maxRetries
        then pure (Left err)
        else runRequest maxRetries (retryCount + 1) makeRequest
