module SLL.Decomposition where

import SLL.Lang

data Context =
    Hole
  | GIntro GName Context [Expr] -- g(ctx, e1, ..., en)
  deriving (Show, Eq)

data Redex =
    RedexF FName [Expr] -- f(e1, ..., en)
  | RedexG GName (CName, [Expr]) [Expr] -- g(C(e1, ..., en), ... ei ...)
  | RedexGFail GName Var [Expr] -- g(v, e1, ..., en)
  deriving (Show, Eq)

data Observable =
    ObsC CName [Expr]
  | ObsV Var
  deriving (Show, Eq)

data Decomposition =
    DecObservable Observable
  | DecRedex (Context, Redex)
  deriving (Show, Eq)

fillHole :: Context -> Expr -> Expr
fillHole Hole e = e
fillHole (GIntro g ctx restArgs) e = G g $ fillHole ctx e : restArgs

decompose :: Expr -> Decomposition
decompose (V v) = DecObservable $ ObsV v
decompose (C c args) = DecObservable $ ObsC c args
decompose (F f args) = DecRedex $ (Hole, RedexF f args)
decompose (G g args) =
  case head args of
    V v -> DecRedex $ (Hole, RedexGFail g v $ tail args)
    C c cargs -> DecRedex $ (Hole, RedexG g (c, cargs) $ tail args)
    F f fargs -> DecRedex $ (GIntro g Hole $ tail args, RedexF f fargs)
    G g' gargs ->
      let DecRedex (ctx, redex) = decompose (G g' gargs) in
        DecRedex $ (GIntro g ctx $ tail args, redex)
