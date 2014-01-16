import System.Environment
import SimpleHtmlParser

main :: IO ()
main = do args <- getArgs
          if length args == 0
              then
                 do result <- getContents >>= return . readHtml "STDIN"
                    putStrLn . (either show show) $ result
              else
                 do result <- (readFile $ args !! 0) >>= return . (readHtml $ args !! 0)
                    putStrLn . (either show show) $ result
