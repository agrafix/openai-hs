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
apiSpec =
  describe "core api" apiTests

apiTests :: SpecWith ()
apiTests =
  beforeAll makeClient $
    do
      describe "file api" $
        do
          it "allows creating one" $ \cli ->
            do
              let file =
                    FileCreate
                      { fcPurpose = "search",
                        fcDocuments = [FhSearch $ SearchHunk "Test 1" Nothing, FhSearch $ SearchHunk "text 2" (Just "foo")]
                      }
              _ <- forceSuccess $ createFile cli file
              pure ()
      describe "answer api" $
        do
          it "works" $ \cli ->
            do
              let file =
                    FileCreate
                      { fcPurpose = "search",
                        fcDocuments =
                          [ FhSearch $ SearchHunk "Cities in California: San Francisco, Los Angeles" (Just "cali"),
                            FhSearch $ SearchHunk "Tasty fruit: Apple, Orange" (Just "fruit"),
                            FhSearch $ SearchHunk "Cities in Germany: Freiburg, Berlin" (Just "germany")
                          ]
                      }
              res <- forceSuccess $ createFile cli file
              let searchReq =
                    AnswerReq
                      { arFile = Just (fId res),
                        arDocuments = Nothing,
                        arQuestion = "Where is San Francisco?",
                        arSearchModel = EngineId "babbage",
                        arModel = EngineId "davinci",
                        arExamplesContext = "Good programming languages: Haskell, PureScript",
                        arExamples = [["Is PHP a good programming language?", "No, sorry."]],
                        arReturnMetadata = True
                      }
              answerRes <- forceSuccess $ getAnswer cli searchReq
              T.unpack (head (arsAnswers answerRes)) `shouldContain` ("California" :: String)
              pure ()
      describe "embeddings" $ do
        it "computes embeddings" $ \cli -> do
          res <- forceSuccess $ createEmbedding cli (EngineId "babbage-similarity") (EmbeddingCreate "This is nice")
          V.null (olData res) `shouldBe` False
          let embedding = V.head (olData res)
          V.length (eEmbedding embedding) `shouldBe` 2048
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
              firstEngine <- V.head . olData <$> forceSuccess (listEngines cli)
              completionResults <-
                forceSuccess $
                  completeText cli (eId firstEngine) $
                    (defaultTextCompletionCreate "Why is the house ")
                      { tccrMaxTokens = Just 2
                      }
              V.length (tcChoices completionResults) `shouldBe` 1
              T.length (tccText (V.head (tcChoices completionResults))) `shouldNotBe` 0
      describe "document search" $
        do
          it "works (smoke test)" $ \cli ->
            do
              firstEngine <- V.head . olData <$> forceSuccess (listEngines cli)
              searchResults <-
                forceSuccess $
                  searchDocuments cli (eId firstEngine) $
                    SearchResultCreate
                      { sccrDocuments = Just $ V.fromList ["pool", "gym", "night club"],
                        sccrFile = Nothing,
                        sccrQuery = "swimmer",
                        sccrReturnMetadata = False
                      }
              V.length (olData searchResults) `shouldBe` 3
      describe "file based document search" $
        do
          it "works" $ \cli ->
            do
              let file =
                    FileCreate
                      { fcPurpose = "search",
                        fcDocuments =
                          [ FhSearch $ SearchHunk "pool" (Just "pool"),
                            FhSearch $ SearchHunk "gym" (Just "gym"),
                            FhSearch $ SearchHunk "night club" (Just "nc")
                          ]
                      }
              createRes <- forceSuccess $ createFile cli file
              let searchReq =
                    SearchResultCreate
                      { sccrFile = Just (fId createRes),
                        sccrDocuments = Nothing,
                        sccrQuery = "pool",
                        sccrReturnMetadata = True
                      }
              searchRes <- forceSuccess $ searchDocuments cli (EngineId "ada") searchReq
              let res = V.head (olData searchRes)
              srDocument res `shouldBe` 0 -- pool
              srMetadata res `shouldBe` Just "pool"
              pure ()
