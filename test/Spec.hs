import           Control.Exception
import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Control.Monad
import           Parser.Parser
import           Parser.TypeDefs
import           Parser.Utils
import           Text.Megaparsec       (parse, runParser)


main :: IO ()
main =
  hspec $
  describe "Parser" $ do
    describe "arithmetic parser" $ do
      it "parses integer literals" $ do
        parse program "" "type :: a =3;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 3)]
        parse program "" "type :: a =31234;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 31234)]
        parse program "" "type :: a= 145674653;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 145674653)]
        parse program "" "type ::a = 1234112343423413;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 1234112343423413)]
        parse program "" "type:: a = 123412343;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 123412343)]
        parse program "" "type :: a = 31234153423451243;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 31234153423451243)]
        parse program "" "type::a=8567643737876983;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 8567643737876983)]
        parse program "" "type::a=00000002137;" `shouldParse` AST [] [FunDecl TInt "a" [] (EInt 2137)]
      it "parses addition" $ do
        parse program "" "type :: a=aAaAaAaA+bBbBbBbB;" `shouldParse` AST [] [FunDecl TInt "a" [] (EAdd (EVar "aAaAaAaA") (EVar "bBbBbBbB"))]
        parse program "" "type :: a=123412341243+aadfawevxz4231313245;" `shouldParse` AST [] [FunDecl TInt "a" [] (EAdd (EInt 123412341243) (EVar "aadfawevxz4231313245"))]
        parse program "" "type :: a=31423+a12341234;" `shouldParse` AST [] [FunDecl TInt "a" [] (EAdd (EInt 31423) (EVar "a12341234"))]
        parse program "" "type :: a=0039679887+a0000000kkkk;" `shouldParse` AST [] [FunDecl TInt "a" [] (EAdd (EInt 39679887) (EVar "a0000000kkkk"))]
        parse program "" "type :: a=0+0+3+9+67+a+0+k+k+k+k;" `shouldParse` AST [] [FunDecl TInt "a" [] (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EAdd (EInt 0) (EInt 0)) (EInt 3)) (EInt 9)) (EInt 67)) (EVar "a")) (EInt 0)) (EVar "k")) (EVar "k")) (EVar "k")) (EVar "k")) ]
        parse program "" "type :: a=2137+(-a0000000kkkk);" `shouldParse` AST [] [FunDecl TInt "a" [] (EAdd (EInt 2137) (ENeg(EVar "a0000000kkkk")))]
      it "parses subtraction" $ do
        parse program "" "type :: a=3-a;" `shouldParse` AST [] [FunDecl TInt "a" [] (ESub (EInt 3) (EVar "a"))]
        parse program "" "type :: a=3-a - 2137;" `shouldParse` AST [] [FunDecl TInt "a" [] (ESub (ESub (EInt 3) (EVar "a")) (EInt 2137))]
      it "parses multiplication" $
        parse program "" "type :: a=3*a;" `shouldParse` AST [] [FunDecl TInt "a" [] (EMul (EInt 3) (EVar "a"))]
      it "parses complex arithmetic expressions" $
        parse program "" "type :: a = ((b + 1) * 3);" `shouldParse` AST [] [FunDecl TInt "a" [] (EMul (EAdd (EVar "b") (EInt 1)) (EInt 3))]
    describe "function application parser" $ do
      it "parses simple expression application" $ do
        parse program "" "type::a=a b;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EVar "a") (EVar "b")]
        parse program "" "type::a=a 1;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EVar "a") (EInt 1)]
        parse program "" "type::a=3 1;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EInt 3) (EInt 1)]
        parse program "" "type::a=1234 1;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EInt 1234) (EInt 1)]
        parse program "" "type::a=a a12341;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EVar "a") (EVar "a12341")]
      it "parses complex application" $ do
        parse program "" "type::a= f g h;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EVar "f") (EVar "g")) (EVar "h")]
        parse program "" "type::a= a b c d e f g h;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EApply (EApply (EApply (EApply (EApply (EVar "a") (EVar "b")) (EVar "c")) (EVar "d")) (EVar "e")) (EVar "f")) (EVar "g")) (EVar "h")]
        parse program "" "type::a= f (2137 + g) h;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EVar "f") (EAdd (EInt 2137) (EVar "g"))) (EVar "h")]
        parse program "" "type :: functionName   a b c d e f =   functionName 1 (functionName 2) (cuś) (functionName 4);" `shouldParse` AST [] [FunDecl TInt "functionName" (map EVar ["a", "b", "c", "d", "e", "f"]) $
          EApply (EApply (EApply (EApply (EVar "functionName") (EInt 1)) (EApply (EVar "functionName") (EInt 2))) (EVar "cuś")) (EApply (EVar "functionName") (EInt 4))]
      it "parses lambdas" $ do
        parse program "" "type::a=fn x . x + 2;" `shouldParse` AST [] [FunDecl TInt "a" [] $ ELambda [EVar "x"] $ EAdd (EVar "x") (EInt 2)]
        parse program "" "type::a=(fn x . x + 2) 2;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (ELambda [EVar "x"] $ EAdd (EVar "x") (EInt 2)) (EInt 2)]
        parse program "" "type::a=map (fn x . x + 2) list;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EVar "map") (ELambda [EVar "x"] $ EAdd (EVar "x") (EInt 2))) (EVar "list")]
        parse program "" "type :: a = fnnnn;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EVar "fnnnn"]
        parse program "" "type :: a = kafafafn;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EVar "kafafafn"]
    describe "list parser" $ do
      it "parses list literals" $ do
        parse program "" "type::a=[1, 2, 3, 4];" `shouldParse` AST [] [FunDecl TInt "a" [] $ EListLiteral $ map EInt [1..4]]
        parse program "" "type::a=[a, b, c, d];" `shouldParse` AST [] [FunDecl TInt "a" [] $ EListLiteral $ map EVar ["a", "b", "c", "d"]]
        parse program "" "type::a=[[], [], [], []];" `shouldParse` AST [] [FunDecl TInt "a" [] $ EListLiteral $ map EListLiteral [[], [], [], []]]
        parse program "" "type::a=[[1, 2, 3], [[[]]], [[1], [2], [3]], []];" `shouldParse` AST [] [FunDecl TInt "a" [] $ EListLiteral $ map EListLiteral [map EInt [1, 2, 3], [EListLiteral [EListLiteral []]], map (EListLiteral . (:[]) . EInt) [1, 2, 3], []]]
      it "parses cons expressions" $ do
        parse program "" "type::a=x:xs;`" `shouldParse` AST [] [FunDecl TInt "a" [] $ ECons (EVar "x") (EVar "xs")]
        parse program "" "type::a b=x + b : xs;" `shouldParse` AST [] [FunDecl TInt "a" [EVar "b"] $ ECons (EAdd (EVar "x") (EVar "b")) (EVar "xs")]
        parse program "" "type::a b=x * y : concat xs;" `shouldParse` AST [] [FunDecl TInt "a" [EVar "b"] $ ECons (EMul (EVar "x") (EVar "y")) (EApply (EVar "concat") $ EVar "xs")]
      it "parses list concatenation" $ do
        parse program "" "type :: a = xs ++ ys;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EConcat (EVar "xs") (EVar "ys")]
        parse program "" "type :: a = x : xs ++ ys;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EConcat (ECons (EVar "x") (EVar "xs")) (EVar "ys")]
        parse program "" "type :: a = xs ++ ys ++ zs;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EConcat (EConcat (EVar "xs") (EVar "ys")) (EVar "zs")]
        parse program "" "type :: a = x : xs ++ y : ys ++ z : zs;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EConcat (EConcat (ECons (EVar "x") (EVar "xs")) (ECons (EVar "y") (EVar "ys"))) (ECons (EVar "z") (EVar "zs"))]
    describe "boolean parser" $ do
      it "parses negation" $ do -- TODO maybe define negation using conjunction and disjunction if possible?
        parse program "" "type :: a = not b;" `shouldParse` AST [] [FunDecl TInt "a" [] $ ENot $ EVar "b"]
        parse program "" "type :: a = not (b + 2);" `shouldParse` AST [] [FunDecl TInt "a" [] $ ENot (EAdd (EVar "b") (EInt 2))]
      it "parses conjunction" $ do
        parse program "" "type :: a = x and y;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EAnd (EVar "x") (EVar "y")]
        parse program "" "type :: a = 1 + x and 2 - y;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EAnd (EAdd (EInt 1) (EVar "x")) (ESub (EInt 2) (EVar "y"))]
        parse program "" "type :: a = xand;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EVar "xand"]
        parse program "" "type :: a = andx;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EVar "andx"]
        parse program "" "type :: a = 1 + x and 2 - y and 3 * z;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EAnd (EAnd (EAdd (EInt 1) (EVar "x")) (ESub (EInt 2) (EVar "y"))) (EMul (EInt 3) (EVar "z"))]
      it "parses disjunction" $ do
        parse program "" "type :: a = a or a;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EOr (EVar "a") (EVar "a")]
        parse program "" "type :: a = 1 + x or 2 - y;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EOr (EAdd (EInt 1) (EVar "x")) (ESub (EInt 2) (EVar "y"))]
        parse program "" "type::a= xor;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EVar "xor"]
        parse program "" "type::a= oreo;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EVar "oreo"]
        parse program "" "type :: a = 1 and x or 2 and y;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EOr (EAnd (EInt 1) (EVar "x")) (EAnd (EInt 2) (EVar "y"))]
      it "parses equality" $ do
        parse program "" "type :: a = a == a; " `shouldParse` AST [] [FunDecl TInt "a" [] $ EEq (EVar "a") (EVar "a")]
        parse program "" "type :: a = a + 2 == a * 2; " `shouldParse` AST [] [FunDecl TInt "a" [] $ EEq (EAdd (EVar "a") (EInt 2)) (EMul (EVar "a") (EInt 2))]
        parse program "" "type :: a = a == a and b == b; " `shouldParse` AST [] [FunDecl TInt "a" [] $ EAnd (EEq (EVar "a") (EVar "a")) (EEq (EVar "b") (EVar "b"))]
        parse program "" "type :: a = a == a; " `shouldParse` AST [] [FunDecl TInt "a" [] $ EEq (EVar "a") (EVar "a")]
      it "parses less than or equal" $ do
        parse program "" "type :: a = a <= a; " `shouldParse` AST [] [FunDecl TInt "a" [] $ ELeq (EVar "a") (EVar "a")]
        parse program "" "type :: a = a + 2 <= a * 2; " `shouldParse` AST [] [FunDecl TInt "a" [] $ ELeq (EAdd (EVar "a") (EInt 2)) (EMul (EVar "a") (EInt 2))]
        parse program "" "type :: a = a <= a and b <= b; " `shouldParse` AST [] [FunDecl TInt "a" [] $ EAnd (ELeq (EVar "a") (EVar "a")) (ELeq (EVar "b") (EVar "b"))]
        parse program "" "type :: a = a <= a; " `shouldParse` AST [] [FunDecl TInt "a" [] $ ELeq (EVar "a") (EVar "a")]

{-

data Test a = Assert String a | ExpectError String


parserTests :: [Test AST]
parserTests =
  [ Assert "type :: a =3;" (AST [] [FunDecl TInt "a" [] (EInt 3)])
  , Assert "type :: a = 3+  a;" (AST [] [FunDecl TInt "a" [] (EAdd (EInt 3) (EVar "a"))])
  , Assert "type :: a = ((b + 1) * 3) 2137;" (AST [] [FunDecl TInt "a" [] (EApply (EMul (EAdd (EVar "b") (EInt 1)) (EInt 3)) (EInt 2137))])
  , Assert "type :: a = fn x y . y x;" (AST [] [FunDecl TInt "a" [] (ELambda ["x","y"] (EApply (EVar "y") (EVar "x")))])
  , ExpectError "type :: "
  , ExpectError "type :: a"
  , ExpectError "type :: a ="
  , ExpectError "type :: a =;"
  ]

runTest :: (Eq a, Show a) => Parser a -> Test a -> IO ()
runTest p (Assert str x) = either print verify (runParser p "Test" str)
  where
    verify res =
      if res == x
        then return ()
        else putStrLn $ "Error when parsing \"" ++ str ++ "\": expected " ++ show x ++ ", got " ++ show res
runTest p (ExpectError str) = either (const $ return ()) (putStrLn . ("Expected an error, got: " ++) . show) (runParser p "Test" str)


runTests :: (Eq a, Show a) => Parser a -> [Test a] -> IO ()
runTests parser suite = putStrLn "\nStarting tests..." >> mapM_ (runTest parser) suite
-}
