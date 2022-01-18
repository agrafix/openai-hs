module HelperSpec (helperSpec) where

import Control.Exception.Base
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe
import qualified Data.Sequence as Seq
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import OpenAI.Client.Internal.Helpers
import Servant.Client
import Servant.Client.Core.Request
import System.Exit
import Test.Hspec

helperSpec :: Spec
helperSpec =
  do describe "retries" retryTests

makeFakeAction ::
  (Int -> Either ClientError a) ->
  IO (IO Int, IO (Either ClientError a))
makeFakeAction makeResult =
  do
    calls <- newIORef 0
    let action =
          do
            call <- atomicModifyIORef calls $ \i -> (i + 1, i)
            pure (makeResult call)
    pure (readIORef calls, action)

dummyReq :: RequestF () (BaseUrl, BS.ByteString)
dummyReq =
  defaultRequest
    { requestBody = Nothing,
      requestPath = (fromJust $ parseBaseUrl "api.example.com", "")
    }

retryAction ::
  Int ->
  Status ->
  Seq.Seq Header ->
  IO (ClientError, IO Int, IO (Either ClientError Bool))
retryAction n status headers =
  do
    let errResp =
          Response
            { responseStatusCode = status,
              responseHeaders = headers,
              responseHttpVersion = http11,
              responseBody = mempty
            }
        err = FailureResponse dummyReq errResp
    (getCalls, action) <-
      makeFakeAction $ \call ->
        if call < n
          then Left $ err
          else Right True
    pure (err, getCalls, action)

retryTests :: SpecWith ()
retryTests =
  do
    it "does not retry on success" $
      do
        (getCalls, action) <- makeFakeAction (const $ Right True)
        runRequest 10 0 action `shouldReturn` Right True
        getCalls `shouldReturn` 1
    it "retries on connection errors" $
      do
        (getCalls, action) <-
          makeFakeAction $ \call ->
            if call == 0
              then Left (ConnectionError $ toException ExitSuccess)
              else Right True
        runRequest 10 0 action `shouldReturn` Right True
        getCalls `shouldReturn` 2
    it "retries on 409 status code" $
      do
        (_, getCalls, action) <-
          retryAction 1 status409 mempty
        runRequest 10 0 action `shouldReturn` Right True
        getCalls `shouldReturn` 2
    it "retries on 500 status code" $
      do
        (_, getCalls, action) <-
          retryAction 1 status500 mempty
        runRequest 10 0 action `shouldReturn` Right True
        getCalls `shouldReturn` 2
    it "does not retry on status 500 if limit exceeded" $
      do
        (err, getCalls, action) <-
          retryAction 11 status500 mempty
        runRequest 10 0 action `shouldReturn` Left err
        getCalls `shouldReturn` 10
