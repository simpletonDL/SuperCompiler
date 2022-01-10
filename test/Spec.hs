import Test.HUnit

--import Examples
import SLL.Lang
import SLL.Decomposition
import SLL.Evaluation
import SLL.Example
import SLL.Generalization

testDecomposition :: Test
testDecomposition = TestCase $
  let e = G "g1" [G "g2" [G "g3" [V "x"]]] in
  let (DecRedex actual) = decompose e in
  let expeted = (GIntro "g1" (GIntro "g2" Hole []) [], RedexGFail "g3" "x" []) in
  assertEqual ":(" expeted actual


testFindDef :: Test
testFindDef = TestCase $
  let p = SLLProg {mainExpr = V "x", fDefs = [FDef "f" ["x"] $ V "x", FDef "g" ["y"] $ V "y"], gDefs = []} in
  let actual = findFDef p "f" in
  let expected = Just $ FDef "f" ["x"] $ V "x" in
  assertEqual ":(" expected actual

testEval1 :: Test
testEval1 = TestCase $
  let actual = evalStep $ SLLProg { mainExpr = suc z, fDefs = [sucDef], gDefs = [] } in
  let expexted = Just $ s z in
  assertEqual ":(" expexted actual

-- Alpha-equality
testAlphaEq1 :: Test
testAlphaEq1 = TestCase $
  let actual = alphaEquality (V "x") (V "y") in
  let expected = Just [("x", "y")] in
  assertEqual ":(" expected actual

testAlphaEq2 :: Test
testAlphaEq2 = TestCase $
  let actual = alphaEquality (F "f" [G "g" [V "x", V "y"], V "x", F "f1" [V "y"]])
                             (F "f" [G "g" [V "x1", V "y1"], V "x1", F "f1" [V "y1"]]) in
  let expected = Just [("y", "y1"), ("x", "x1")] in
  assertEqual ":(" expected actual

testAlphaEq3 :: Test
testAlphaEq3 = TestCase $
  let actual = alphaEquality (F "f" [G "g" [V "x", V "y"], V "x", F "f1" [V "y"]])
                             (F "f" [G "g" [V "x1", V "y1"], V "x1", F "f1" [V "y2"]]) in
  let expected = Nothing in
  assertEqual ":(" expected actual


--testFindDef :: Test
--testFindDef = TestCase $
--  let prog = SLLProg {expr = _, defs = _} in _

--import Semantic
--
--test1 :: Test
--test1 = TestCase $
--  let act = substitution "y" expr2 expr1 in
--  let exp = Lam "x0" $ Var "x0" :@ (Var "x" :@ Var "u") in
--  assertEqual ":(" exp act
--
--test2 :: Test
--test2 = TestCase $
--  let act =  substitution "t" expr2 exprCase in
--  let exp = Case (Var "x" :@ Var "u")
--             [(Pattern "C1" ["x0", "y"], Var "x0" :@ (Var "x" :@ Var "u")), 
--              (Pattern "C2" ["z"], Lam "x1" $ Var "x1" :@ (Var "x" :@ Var "u"))] in
--  assertEqual ":(" exp act
--
--  
----test2 = TestCase (assertEqual "for (foo 3)," (1,2) (1, 2))
--
tests :: Test
tests = TestList
  [TestLabel "decompose" testDecomposition,
   TestLabel "findDef" testFindDef,
   TestLabel "testEval1" testEval1,
   TestLabel "testAlphaEq1" testAlphaEq1,
   TestLabel "testAlphaEq2" testAlphaEq2,
   TestLabel "testAlphaEq3" testAlphaEq3
   ]

main :: IO Counts
main = runTestTT tests
