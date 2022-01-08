module Utils where

surroundParens :: String -> String
surroundParens = surround "(" ")"

surround :: String -> String -> String -> String
surround l r s = l ++ s ++ r