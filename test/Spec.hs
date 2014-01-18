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
            context "with content" $ do
                it "parses" $ do
                    parseTest "<html>AAA</html>" `shouldBe` (Right $ Tag "html" "AAA" [])
            context "with nested tag" $ do
                it "parses" $ do
                    parseTest "<html><body></body></html>"
                        `shouldBe` (Right $ Tag "html" "" [Tag "body" "" [] ])
            context "with nested tag and content" $ do
                it "parses" $ do
                    parseTest "<html><body>AAA</body></html>"
                        `shouldBe` (Right $ Tag "html" "" [Tag "body" "AAA" [] ])
            context "with nested contents" $ do
                it "parses" $ do
                    parseTest "<html><body>AAA</body>BBB</html>"
                        `shouldBe` (Right $ Tag "html" "BBB" [Tag "body" "AAA" [] ])
            context "with invalid nested contents" $ do
                it "parses" $ do
                    parseTest "<html>CCC<body>AAA</body>BBB</html>"
                        `shouldBe` (Left "\"Spec\" (line 1, column 10):\nunexpected \"b\"\nexpecting \"</\"")
                        -- TODO: pass this case with:
                        --   `shouldBe` (Right $ Tag "html" "CCCBBB" [Tag "body" "AAA" [] ])
