{-
Some examples of Hspec and Property testing (this last one are much too simple, I am afraid)

Look at this stack overflow link on how to integrate the tests in the GHCI workflow
https://stackoverflow.com/questions/39938101/how-to-load-tests-in-ghci-with-stack

In short, start GHCI in this way:

stack ghci --ghci-options -isrc --ghci-options -itest exploring-testing:exploring-testing-test
-}

import Archiver

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec


main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs [specTests]
  defaultMain (testGroup "All tests" [
                  testGroup "Specs" specs
                -- , testGroup "Properties" props
              ])

specTests :: SpecWith ()
specTests = 
  describe "Hspec tests" $ do
    it "always succeeds" $
      isTrue `shouldBe` True

isTrue = True