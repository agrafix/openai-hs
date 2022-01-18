import ApiSpec
import HelperSpec
import Test.Hspec

main :: IO ()
main =
  hspec $
    do
      apiSpec
      helperSpec
