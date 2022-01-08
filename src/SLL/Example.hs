module SLL.Example where

import SLL.Lang

-- List
nil :: Expr
nil = C "Nil" []

cons :: (Expr, Expr) -> Expr
cons (x, xs) = C "Cons" [x, xs]

buildList :: [Var] -> Expr
buildList [] = nil
buildList (x : xs) = cons (V x, buildList xs)

-- Append
append :: (Expr, Expr) -> Expr
append (xs, ys) = G "app" [xs, ys]

-- app (y : app(xs, ys), zs)
-- app (x : xs, ys) = x : app(xs, ys)
-- x -> y
-- xs -> app(xs, ys)
-- ys -> zs

appendDef :: GDef
appendDef = GDef "app"
  [CaseDef { pattern = Pattern { consName = "Nil", consVars = [] },
             restVars = ["ys"],
             body = V "ys" },
   CaseDef { pattern = Pattern {consName = "Cons", consVars = ["x", "xs"]},
             restVars = ["ys"],
             body = cons (V "x", G "app" [V "xs", V "ys"])
            }
  ]

appendCase1Expr :: Expr
appendCase1Expr = append(append (V "xs", V "ys"), V "zs")

appendCase1Prog :: SLLProg
appendCase1Prog = SLLProg { main = appendCase1Expr, gDefs = [appendDef], fDefs = [] }

appendCase2Expr :: Expr
appendCase2Expr = append(append (cons (V "x", V "xs"), V "ys"), V "zs")

appendCase2Prog :: SLLProg
appendCase2Prog = SLLProg { main = appendCase2Expr, gDefs = [appendDef], fDefs = [] }

-- f-functions
z :: Expr
z = C "Z" []

s :: Expr -> Expr
s n = C "S" [n]

suc :: Expr -> Expr
suc n = F "suc" [n]

sucDef :: FDef
sucDef = FDef "suc" ["x"] $ s (V "x")
