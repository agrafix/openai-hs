module ApiSpec (apiSpec) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (getEnv)
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Vector as V

import OpenAI.Client

makeClient :: IO OpenAIClient
makeClient =
  do manager <- newManager tlsManagerSettings
     apiKey <- T.pack <$> getEnv "OPENAI_KEY"
     pure (makeOpenAIClient apiKey manager 2)

forceSuccess :: (MonadFail m, Show a) => m (Either a b) -> m b
forceSuccess req =
  req >>= \res ->
  case res of
    Left err -> fail (show err)
    Right ok -> pure ok

apiSpec :: Spec
apiSpec =
  describe "core api" apiTests

apiTests :: SpecWith ()
apiTests =
  beforeAll makeClient $
  do describe "engines" $
       do it "lists engines" $ \cli ->
            do res <- forceSuccess $ listEngines cli
               V.null (olData res) `shouldBe` False
          -- TODO: This doesn't work for some reason, even the cURL example from the docs fail.
          --it "retrieve engine" $ \cli ->
          --  do engineList <- forceSuccess $ listEngines cli
          --     print engineList
          --     let firstEngine = V.head (olData engineList)
          --     engine <- forceSuccess $ getEngine cli (eId firstEngine)
          --     engine `shouldBe` firstEngine
     describe "text completion" $
       do it "works (smoke test)" $ \cli ->
            do firstEngine <- V.head . olData <$> forceSuccess (listEngines cli)
               completionResults <-
                 forceSuccess $
                 completeText cli (eId firstEngine) $
                 (defaultTextCompletionCreate "Why is the house ")
                 { tccrMaxTokens = Just 2 }
               V.length (tcChoices completionResults) `shouldBe` 1
               T.length (tccText (V.head (tcChoices completionResults))) `shouldNotBe` 0
     describe "document search" $
       do it "works (smoke test)" $ \cli ->
            do firstEngine <- V.head . olData <$> forceSuccess (listEngines cli)
               searchResults <-
                 forceSuccess $
                 searchDocuments cli (eId firstEngine) $
                 SearchResultCreate
                 { sccrDocuments = V.fromList ["pool", "gym", "night club"]
                 , sccrQuery = "swimmer"
                 }
               V.length (olData searchResults) `shouldBe` 3
