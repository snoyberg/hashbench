import Test.Hspec
import Test.Hspec.QuickCheck
import Lib
import qualified Data.Text as T

main :: IO ()
main = hspec $ prop "C and Haskell match" $ \str -> do
  let t = T.pack str
  call hash_via_c t `shouldBe` call hash_via_haskell t
  let t1 = T.drop 1 t -- test offset
  call hash_via_c t1 `shouldBe` call hash_via_haskell t1
