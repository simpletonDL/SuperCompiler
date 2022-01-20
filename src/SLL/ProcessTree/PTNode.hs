module SLL.ProcessTree.PTNode where

import SLL.Lang
import SLL.Decomposition
import SLL.Evaluation
import SLL.Namer
import Data.List
import SLL.ProcessTree.Common

data PTNode =
    PTCaseAnalysis Expr [(GCase, PTNode)]
  | PTCons CName Expr [PTNode]
  | PTVar Var
  | PTLeaf Expr -- only for prune tree
  deriving Eq

class HasExpr a where
  getExpr :: a -> Expr

instance HasExpr PTNode where
  getExpr (PTCaseAnalysis e _ ) = e
  getExpr (PTCons _ e _) = e
  getExpr (PTVar v) = V v
  getExpr (PTLeaf e) = e

patternToExpr :: Pattern -> Expr
patternToExpr p = C (consName p) $ map V (consVars p)

replaceExpr :: SLLProg -> Expr -> SLLProg
replaceExpr p e = SLLProg { mainExpr = e, fDefs = fDefs p, gDefs = gDefs p }

buildProcessTree :: SLLProg -> PTNode
buildProcessTree prog =
  let e = mainExpr prog in
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
          PTCaseAnalysis (mainExpr prog) children
        _ ->
          let Just nextExpr = evalStep prog in buildProcessTree $ replaceExpr prog nextExpr
    DecObservable obs -> case obs of
      ObsC c cargs -> PTCons c (mainExpr prog) $ map (buildProcessTree . replaceExpr prog) cargs
      ObsV v -> PTVar v

-- Pruning process tree
toLeaf :: PTNode -> PTNode
toLeaf (PTCaseAnalysis e _) = PTLeaf e
toLeaf (PTCons c e _) = PTLeaf e
toLeaf l@(PTVar v) = l
toLeaf l@(PTLeaf _) = l

pruneHelper :: Int -> Int -> PTNode -> PTNode
pruneHelper depth maxDepth node = 
  if (depth == maxDepth) then toLeaf node else
    case node of
      PTCaseAnalysis e cases -> 
        let nodes = map (pruneHelper (depth + 1) maxDepth . snd) cases in
        PTCaseAnalysis e $ zip (map fst cases) nodes
      PTCons c e children -> PTCons c e $ map (pruneHelper (depth + 1) maxDepth) children
      leaf -> leaf
      
prunePTNode :: Int -> PTNode -> PTNode
prunePTNode depth = pruneHelper 0 depth

-- append(append (xs, ys), zs)
-- * xs = x0 : x1
--    append (append (x0 : x1, ys), zs)
--    append (x0 : append (x1, ys), zs)
--    x0 : append (append (x1, ys), zs)
-- * xs = nil
--    append (append (nil, ys), zs)
--    append (ys, zs)
--    * ys = nil
--       append (nil, zs)
--       zs
--    * ys = x0 : x1
--       append (x0 : x1, zs)
--       x0 : append(x1, zs)

-- Printer
makeIndent :: Int -> String -> String
makeIndent x s = replicate (2*x) ' ' ++ s

processTreeToString :: Int -> PTNode -> String
processTreeToString depth node =
  case node of
    PTCaseAnalysis e children ->
      let head =  makeIndent depth $ "Case Analisys: " ++ show e in
      let childrenStrs = map (\(gCase, child) -> (makeIndent depth $ "* " ++ show gCase) ++ "\n" ++ processTreeToString (depth + 1) child) children in
        head ++ "\n" ++ intercalate "\n" childrenStrs
    PTCons c e children ->
      let childrenStrs = map (processTreeToString $ depth + 1) children in
      makeIndent depth $ "Constructor: " ++ show e ++ "\n" ++ intercalate "\n" childrenStrs
    PTVar v ->  makeIndent depth $ "Var: " ++ v
    PTLeaf e -> makeIndent depth $ "Leaf: " ++ show e

instance Show PTNode where
   show node = processTreeToString 0 node

instance Show GCase where
  show gCase = gCaseVar gCase ++ " = " ++ show (gCasePtrn gCase)