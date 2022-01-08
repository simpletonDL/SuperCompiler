module SLL.Evaluation where

import SLL.Lang
import SLL.Decomposition
import Data.List (find)

-- [x -> term] e
subst :: Var -> Expr -> Expr -> Expr
subst x term e@(V y) = if x == y then term else e
subst x term (C c cargs) = C c $ map (subst x term) cargs
subst x term (F f fargs) = F f $ map (subst x term) fargs
subst x term (G g gargs) = G g $ map (subst x term) gargs

substAll :: [(Var, Expr)] -> Expr -> Expr
substAll sbs e@(V v) = let sb = find (\(var, _) -> var == v) sbs in maybe e (\(_, expr) -> expr) sb
substAll sbs (C c cargs) = C c $ map (substAll sbs) cargs
substAll sbs (F f fargs) = F f $ map (substAll sbs) fargs
substAll sbs (G g gargs) = G g $ map (substAll sbs) gargs

--substAll :: [Var] -> [Expr] -> Expr -> Expr
--substAll [] [] e = e
--substAll (v : vs) (t : ts) e = subst v t $ substAll vs ts e
--substAll _ _ _ = error "Different var and term count"

evalStep :: SLLProg -> Maybe Expr
evalStep p = case decompose $ main p of
  (DecObservable _) -> Nothing
  (DecRedex (ctx, redex)) -> case redex of
    RedexF f args -> case findFDef p f of
      Just (FDef _ vars body) ->
        let unfold = substAll (zip vars args) body in Just $ fillHole ctx unfold
      Nothing -> error $ "f-function " ++ f ++ " is not defined"
    RedexG g (cName, cArgs) restArgs -> case findGDefCase p (g, cName) of
      Just caseDef ->
        let vars = consVars (pattern caseDef) ++ restVars caseDef in
        let args = cArgs ++ restArgs in
        let unfold = substAll (zip vars args) (body caseDef) in
--        let substConsArgs = substAll (zip (consVars $ pattern caseDef) cArgs) (body caseDef) in
--        let substRestArgs = substAll (zip (restVars caseDef) restArgs) substConsArgs in
        Just $ fillHole ctx unfold
      Nothing -> error $ "g-function " ++ g ++ " with pattern " ++ cName ++ " is not defined"
    RedexGFail _ _ _ -> error "gfail"

eval :: SLLProg -> Expr
eval p = case evalStep p of
  Just e -> eval $ SLLProg { main = e, gDefs = gDefs p, fDefs = fDefs p }
  Nothing -> main p













