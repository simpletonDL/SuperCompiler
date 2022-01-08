module SLL.ProcessTree where

import SLL.Lang
import SLL.Decomposition
import SLL.Evaluation
import SLL.Namer

data GCase = GCase {
  gCaseVar :: Var,
  gCasePtrn :: Pattern
} deriving (Eq, Show)

data PTNode =
    CaseAnalysis Expr [(GCase, PTNode)]
  | Leaf Expr
  deriving (Eq, Show)

patternToExpr :: Pattern -> Expr
patternToExpr p = C (consName p) $ map V (consVars p)

replaceExpr :: SLLProg -> Expr -> SLLProg
replaceExpr p e = SLLProg { main = e, fDefs = fDefs p, gDefs = gDefs p }

buildProcessTree :: SLLProg -> PTNode
buildProcessTree prog =
  let e = main prog in
  case decompose e of
    DecRedex (ctx, redex) ->
      case redex of
        RedexGFail g var _ ->
          let patterns = map pattern $ findGDefAllCases prog g in
          let children = map (\ptrn ->
                let freshVars = nextNames (collectVars e) (length $ consVars ptrn) in
                let insteadVar = C (consName ptrn) $ map V freshVars in
                let gCase = GCase { gCaseVar = var, 
                                    gCasePtrn = Pattern { consName = consName ptrn, consVars = freshVars }} in
                let ptNode = buildProcessTree $ replaceExpr prog $ subst var insteadVar e in
                  (gCase, ptNode)) patterns in
          CaseAnalysis (main prog) children
        -- TODO
        RedexF f vars -> undefined
        RedexG _ _ _ -> 
          let Just nextExpr = evalStep prog in buildProcessTree $ replaceExpr prog nextExpr
    _ -> Leaf e

-- append(append (xs, ys), zs)
-- ** xs = x0 : x1
--    append (append (x0 : x1, ys), zs)
--    append (x0 : append (x1, ys), zs)
--    x0 : append (append (x1, ys), zs)
-- ** xs = nil
--    append (append (nil, ys), zs)
--    append (ys, zs)
--    ** ys = nil
--       append (nil, zs)
--       zs
--    ** ys = x0 : x1
--       append (x0 : x1, zs)
--       x0 : append(x1, zs)

x = CaseAnalysis (G "app" [G "app" [V "xs",V "ys"],V "zs"]) 
    [(GCase {gCaseVar = "xs", gCasePtrn = Pattern {consName = "Nil", consVars = []}},
      CaseAnalysis (G "app" [V "ys",V "zs"]) 
         [(GCase {gCaseVar = "ys", gCasePtrn = Pattern {consName = "Nil", consVars = []}},
           Leaf (V "zs")),
          
          (GCase {gCaseVar = "ys", gCasePtrn = Pattern {consName = "Cons", consVars = ["x0","x1"]}},
           Leaf (C "Cons" [V "x0",G "app" [V "x1",V "zs"]]))]),
     
     (GCase {gCaseVar = "xs", gCasePtrn = Pattern {consName = "Cons", consVars = ["x0","x1"]}},
      Leaf (C "Cons" [V "x0",G "app" [G "app" [V "x1",V "ys"],V "zs"]]))]
