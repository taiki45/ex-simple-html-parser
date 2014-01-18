module Main (main, spec)
where

import Test.Hspec
import Control.Arrow (left)
import Text.SimpleHtmlParser
import Data.SimpleHtmlTag

main :: IO ()
main = hspec spec

parseTest :: String -> Either String Tag
parseTest input = (left show $ parseSimpleHtml "Spec" input)

spec :: Spec
spec = do
        describe "SimpleHtmlParser" $ do
            context "with one tag" $ do
                it "parses" $ do
                    parseTest "<html></html>" `shouldBe` (Right $ Tag "html" "" [])
            context "with nested tag" $ do
                it "parses" $ do
                    parseTest "<html><body></body></html>"
                        `shouldBe` (Right $ Tag "html" "" [Tag "body" "" [] ])
