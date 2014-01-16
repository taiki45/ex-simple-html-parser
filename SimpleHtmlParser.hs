module SimpleHtmlParser
    (readHtml)
where

import Text.ParserCombinators.Parsec

data Tag = Tag {
                _name :: String,
                _content :: String,
                _children :: [Tag]
              } deriving Show

openB :: Parser Char
openB = char '<'

openBEnd :: Parser String
openBEnd = string "</"

closeB :: Parser Char
closeB = char '>'

parseSTag :: Parser String
parseSTag = between openB closeB (many1 alphaNum)

parseETag :: String -> Parser String
parseETag tagName = between openBEnd closeB (string tagName)

parseContent :: Parser String
parseContent = many $ noneOf "<"

parseEnnerTag :: Parser [Tag]
parseEnnerTag = option [] $ many (try $ parseTag)

parseTag :: Parser Tag
parseTag = do spaces
              name <- parseSTag
              children <- parseEnnerTag
              spaces
              content <- try (do contents <- parseContent `manyTill` parseETag name
                                 return $ foldr (++) [] contents)
              return $ Tag {_children = children, _name = name, _content = content}

readHtml :: String -> String -> Either ParseError Tag
readHtml name input = parse parseTag name input
