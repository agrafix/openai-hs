module ApiSpec (apiSpec) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenAI.Client
import System.Environment (getEnv)
import Test.Hspec

makeClient :: IO OpenAIClient
makeClient =
  do
    manager <- newManager tlsManagerSettings
    apiKey <- T.pack <$> getEnv "OPENAI_KEY"
    pure (makeOpenAIClient apiKey manager 2)

forceSuccess :: (MonadFail m, Show a) => m (Either a b) -> m b
forceSuccess req =
  req >>= \res ->
    case res of
      Left err -> fail (show err)
      Right ok -> pure ok

apiSpec :: Spec
apiSpec = do
  describe "2022 core api" apiTests2022
  describe "March 2023 core API" apiTests2023

---------------------------------
------- 2023 API tests ----------
---------------------------------

apiTests2023 :: SpecWith ()
apiTests2023 =
  beforeAll makeClient $ do
    describe "models api" $ do
      it "list models" $ \cli -> do
        res <- forceSuccess $ listModels cli
        (V.length (olData res) > 5) `shouldBe` True
        let modelId = ModelId "text-embedding-3-small"
        case V.find (\m -> mId m == modelId) (olData res) of
          Nothing -> expectationFailure $ "could not find matching model in response " <> show modelId
          Just m -> mOwnedBy m `shouldBe` "system"

      it "retrieve model" $ \cli -> do
        model <- forceSuccess $ getModel cli (ModelId "text-embedding-3-small")
        mOwnedBy model `shouldBe` "system"

    describe "completions api" $ do
      it "create completion" $ \cli -> do
        let completion =
              (defaultCompletionCreate (ModelId "gpt-3.5-turbo-instruct") "The opposite of up is")
                { ccrMaxTokens = Just 1,
                  ccrTemperature = Just 0.1,
                  ccrN = Just 1
                }
        res <- forceSuccess $ completeText cli completion
        crChoices res `shouldNotBe` []
        cchText (head (crChoices res)) `shouldBe` " down"

    describe "chat api" $ do
      it "create chat completion" $ \cli -> do
        let completion =
              defaultChatCompletionRequest
                (ModelId "gpt-3.5-turbo")
                [ ChatMessage
                    { chmRole = "user",
                      chmContent = Just "What is the opposite of up? Answer in one word with no punctuation.",
                      chmFunctionCall = Nothing,
                      chmName = Nothing
                    }
                ]
        res <- forceSuccess $ completeChat cli completion
        chrChoices res `shouldNotBe` []
        chmContent (chchMessage (head (chrChoices res))) `shouldBe` Just "down"
      it "'content' is a required property" $ \cli -> do
        let completion =
              defaultChatCompletionRequest
                (ModelId "gpt-3.5-turbo")
                [ ChatMessage
                    { chmRole = "assistant",
                      chmContent = Nothing,
                      chmFunctionCall = Just $ ChatFunctionCall { chfcName = "f", chfcArguments = "{}" },
                      chmName = Nothing
                    },
                  ChatMessage
                    { chmRole = "function",
                      chmContent = Just "x",
                      chmFunctionCall = Nothing,
                      chmName = Just "f"
                    }
                ]
        res <- forceSuccess $ completeChat cli completion
        chrChoices res `shouldNotBe` []

    -- TODO (2023.03.22): Create tests for images, audio APIs

    describe "embeddings api" $ do
      it "create embeddings" $ \cli -> do
        let embedding = EmbeddingCreate {embcModel = ModelId "text-embedding-ada-002", embcInput = "Hello", embcUser = Nothing}
        res <- forceSuccess $ createEmbedding cli embedding
        embrData res `shouldNotBe` []
        V.length (embdEmbedding (head $ embrData res)) `shouldBe` 1536

---------------------------------
------- 2022 API tests ----------
---------------------------------

apiTests2022 :: SpecWith ()
apiTests2022 =
  beforeAll makeClient $
    do
      describe "embeddings" $ do
        it "computes embeddings" $ \cli -> do
          res <- forceSuccess $ engineCreateEmbedding cli (EngineId "babbage-similarity") (EngineEmbeddingCreate "This is nice")
          V.null (olData res) `shouldBe` False
          let embedding = V.head (olData res)
          V.length (eneEmbedding embedding) `shouldBe` 2048
      describe "fine tuning" $ do
        it "allows creating fine-tuning" $ \cli -> do
          let file =
                FileCreate
                  { fcPurpose = "fine-tune",
                    fcDocuments =
                      [ FhFineTune $ FineTuneHunk "So sad. Label:" "sad",
                        FhFineTune $ FineTuneHunk "So happy. Label:" "happy"
                      ]
                  }
          createRes <- forceSuccess $ createFile cli file
          let ftc = defaultFineTuneCreate (fId createRes)
          res <- forceSuccess $ createFineTune cli ftc
          ftStatus res `shouldBe` "pending"
      describe "engines" $
        do
          it "lists engines" $ \cli ->
            do
              res <- forceSuccess $ listEngines cli
              V.null (olData res) `shouldBe` False
          it "retrieve engine" $ \cli ->
            do
              engineList <- forceSuccess $ listEngines cli
              let firstEngine = V.head (olData engineList)
              engine <- forceSuccess $ getEngine cli (eId firstEngine)
              engine `shouldBe` firstEngine
      describe "text completion" $
        do
          it "works (smoke test)" $ \cli ->
            do
              completionResults <-
                forceSuccess $
                  engineCompleteText cli (EngineId "text-curie-001") $
                    (defaultEngineTextCompletionCreate "Why is the house ")
                      { tccrMaxTokens = Just 2
                      }
              V.length (tcChoices completionResults) `shouldBe` 1
              T.length (tccText (V.head (tcChoices completionResults))) `shouldNotBe` 0
