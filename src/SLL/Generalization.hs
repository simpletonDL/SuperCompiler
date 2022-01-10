module SLL.Generalization where

import SLL.Lang
import Control.Monad (guard)
import Data.List

type Permutation = [(Var, Var)]

alphaEqualityExprList :: [Expr] -> [Expr] -> Permutation -> Maybe Permutation
alphaEqualityExprList [] [] p =
  let gs = groupBy (\x y -> fst x == fst y) $ sort p in
    if all (== True) $ map (\g -> let x = (snd $ head g) in all (\(_, y) -> x == y) g) gs
    then Just $ nub p else Nothing
alphaEqualityExprList (x : xs) (y : ys) p = do {
  perm <- alphaEquality x y;
  alphaEqualityExprList xs ys (perm ++ p)
}
alphaEqualityExprList _ _ _ = Nothing

alphaEquality :: Expr -> Expr -> Maybe Permutation
alphaEquality (V v1) (V v2) = Just [(v1, v2)]
alphaEquality (C c1 args1) (C c2 args2) = do {
  guard $ c1 == c2;
  alphaEqualityExprList args1 args2 []
}
alphaEquality (F f1 args1) (F f2 args2) = do {
  guard $ f1 == f2;
  alphaEqualityExprList args1 args2 []
}
alphaEquality (G g1 args1) (G g2 args2) = do {
  guard $ g1 == g2;
  alphaEqualityExprList args1 args2 []
}
alphaEquality _ _ = Nothing


-- f(x1, x2, x3)
-- f(x1, x2, x1)

-- v               { v = f(x1, x1, x3) }           { v =  f(x1, x1, x1) }
-- f(v1, v2, v3)   { v1 = x1, v2 = x1, v3 = x3 }   { v1 = x1, v2 = x1, v3 = x1 }

-- f(v, v, v3)   { v = x1, v3 = x3 }   { v = x1, v3 = x1 }

