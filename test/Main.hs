import System.Environment
import Text.SimpleHtmlParser

main :: IO ()
main = do args <- getArgs
          if length args == 0
              then
                 do result <- getContents >>= return . parseSimpleHtml "STDIN"
                    putStrLn . (either show show) $ result
              else
                 do result <- (readFile $ args !! 0) >>= return . (parseSimpleHtml $ args !! 0)
                    putStrLn . (either show show) $ result
