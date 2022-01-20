module SLL.ProcessTree.PartialPTNode where

import SLL.Lang
import SLL.ProcessTree.Common
import SLL.ProcessTree.PTNode
import SLL.Generalization
import SLL.Namer
import Data.Maybe
import Data.List
import Control.Monad.Trans.State.Lazy

data Signature = Signature (GName, [Var])

instance Show Signature where
  show (Signature (gName, vars)) = gName ++ "(" ++ intercalate "," vars ++ ")"

data PartialPTNode =
     PartialPTCaseAnalysis Signature Expr [(GCase, PartialPTNode)]
  |  PartialPTCons CName Expr [PartialPTNode]
  |  PartialFoldNode Signature Permutation Expr
  |  PartialPTVar Var
  |  PartialPTLeaf Expr

foldPTNodeHelper :: [(Signature, Expr)] -> PTNode -> State Int PartialPTNode
foldPTNodeHelper _ (PTVar v) = return $ PartialPTVar v
foldPTNodeHelper parents (PTCaseAnalysis expr cases) = 
  let foldNode = [(s, perm) | (s, e) <- parents, Just perm <- [alphaEquality e expr]] in
  case foldNode of
    [] -> 
      do 
        gName <- nextName' "f" []
        let caseVar = case cases of { (gCase, _) : _ -> gCaseVar gCase; _ -> error "empty gCases"}
        let vars = caseVar : filter (/= caseVar) (collectVars expr)
        let signature = Signature (gName, vars)
        let newParents = (signature, expr) : parents
        newChildren <- mapM (foldPTNodeHelper newParents . snd) cases
        return $ PartialPTCaseAnalysis signature expr $ zip (map fst cases) newChildren
    (s, perm) : _ -> return $ PartialFoldNode s perm expr
foldPTNodeHelper parents (PTCons cName expr children) =
  do
    newChildren <- mapM (foldPTNodeHelper parents) children
    return $ PartialPTCons cName expr newChildren

foldPTNodeHelper _ _ = error "not implemented"

foldPTNode :: PTNode -> PartialPTNode
foldPTNode node = evalState (foldPTNodeHelper [] node) 0

partialProcessTreeToString :: Int -> PartialPTNode -> String
partialProcessTreeToString depth node =
  case node of
    PartialPTCaseAnalysis signature e children ->
      let head =  makeIndent depth $ "Case Analisys: " ++ show signature ++ " = " ++ show e in
      let childrenStrs = map (\(gCase, child) -> (makeIndent depth $ "* " ++ show gCase) ++ "\n" ++ partialProcessTreeToString (depth + 1) child) children in
        head ++ "\n" ++ intercalate "\n" childrenStrs
    PartialPTCons _ e children ->
      let head = makeIndent depth $ "Constructor: " ++ show e in
      let childrenStrs = map (partialProcessTreeToString $ depth + 1) children in
      head ++ "\n" ++ intercalate "\n" childrenStrs
    PartialPTVar v ->  makeIndent depth $ "Var: " ++ v
    PartialFoldNode signature permutation expr -> makeIndent depth $ "Folding(" ++ show expr ++ "): " ++ show signature ++ " <- " ++ show permutation
    PartialPTLeaf e -> makeIndent depth $ "Leaf: " ++ show e

instance Show PartialPTNode where
   show node = partialProcessTreeToString 0 node
