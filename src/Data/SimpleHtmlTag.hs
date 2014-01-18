module Data.SimpleHtmlTag (Tag (Tag) ) where

data Tag = Tag
            { name :: String
            , content :: String
            , children :: [Tag]
            } deriving (Show, Eq)
