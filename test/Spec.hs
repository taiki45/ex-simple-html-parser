module Main (main, spec)
where

import Test.Hspec
import Control.Arrow (left)
import Text.SimpleHtmlParser
import Data.SimpleHtmlTag

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "SimpleHtmlParser" $ do
            context "with one tag" $ do
                it "parses" $ do
                    (left show $ parseSimpleHtml "Spec" "<html></html>") `shouldBe` (Right $ Tag "html" "" [])
