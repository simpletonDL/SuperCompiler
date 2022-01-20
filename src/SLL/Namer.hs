module SLL.Namer where

import Control.Monad.Trans.State.Lazy
import Control.Monad (replicateM)
import Data.Maybe
import Data.List (find)

nextName' :: String -> [String] -> State Int String
nextName' start reversedNames = do {
  i <- get;
  put $ i + 1;
  let name = start ++ show i in
  if isNothing $ find (== name) reversedNames then
    return name
  else
    nextName reversedNames
}

nextName :: [String] -> State Int String
nextName = nextName' "x"

nextNames :: [String] -> Int -> [String]
nextNames reversedNames count =
  let st = replicateM count (nextName reversedNames) in
    evalState st 0
