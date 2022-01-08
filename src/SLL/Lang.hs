{-# LANGUAGE InstanceSigs #-}

module SLL.Lang where

import Data.List
import Control.Monad (guard)
import Data.Maybe

type Var = String
type CName = String
type FName = String
type GName = String


-- * Expressions
data Expr =
    V Var
  | C CName [Expr]
  | F FName [Expr]
  | G GName [Expr] -- expr list not empty
  deriving (Show, Eq)

class Named a where
  getName :: a -> String

-- ** Expression functions
collectVars :: Expr -> [Var]
collectVars (V v) = [v]
collectVars (C _ cargs) = foldMap (\e -> collectVars e) cargs
collectVars (F _ fargs) = foldMap (\e -> collectVars e) fargs
collectVars (G _ gargs) = foldMap (\e -> collectVars e) gargs

-- Definitions

-- f-function
data FDef = FDef FName [Var] Expr
  deriving (Eq, Show)

-- g-function
data CaseDef = CaseDef {
  pattern :: Pattern,
  restVars :: [Var],
  body :: Expr
} deriving (Eq, Show)

-- C (v1, ..., vn)
data Pattern = Pattern {
  consName :: CName,
  consVars :: [Var]
} deriving (Eq, Show)

data GDef = GDef GName [CaseDef] -- g [(p1, v1, ..., vn) = e, ...]
  deriving (Eq, Show)

instance Named FDef where
  getName (FDef name _ _) = name

instance Named GDef where
  getName (GDef name _ ) = name

data SLLProg = SLLProg {
  main :: Expr,
  fDefs :: [FDef],
  gDefs :: [GDef]
} deriving (Eq, Show)

findFDef :: SLLProg -> FName -> Maybe FDef
findFDef prog name = find (\def -> getName def == name) $ fDefs prog

findGDefCase :: SLLProg -> (GName, CName) -> Maybe CaseDef
findGDefCase prog (gName, cName) = do {
    GDef _ caseDefs <- find (\def -> getName def == gName) $ gDefs prog;
    find (\caseDef -> (consName $ pattern caseDef) == cName) caseDefs;
  }

findGDefAllCases :: SLLProg -> GName -> [CaseDef]
findGDefAllCases p gName = fromMaybe [] $ do {
  GDef _ caseDefs <- find (\def -> getName def == gName) $ gDefs p;
  return $ caseDefs
}