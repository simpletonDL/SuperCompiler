module SLL.ProcessTree.Common where

import SLL.Lang
  
data GCase = GCase {
  gCaseVar :: Var,
  gCasePtrn :: Pattern
} deriving Eq
