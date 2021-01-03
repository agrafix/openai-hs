import Test.Hspec

import ApiSpec
import HelperSpec

main :: IO ()
main =
  hspec $
  do apiSpec
     helperSpec
