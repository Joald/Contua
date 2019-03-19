import           Control.Exception
import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Control.Monad
import           Parser.Parser
import           Parser.Expr
import           Parser.TypeDefs
import           Parser.TypeDecls
import           Parser.Utils
import           Text.Megaparsec       (parse, runParser, eof)
import Text.Megaparsec.Error.Builder (ET)

shouldParseExpr :: String -> Expr -> Expectation
shouldParseExpr = shouldParse . parse expr ""

shouldParseType :: String -> Type -> Expectation
shouldParseType = shouldParse . parse type_ ""

shouldParseTypeDecl :: String -> TypeDecl -> Expectation
shouldParseTypeDecl = shouldParse . parse typeDecl ""

shouldParseFunDecl :: String -> FunDecl -> Expectation
shouldParseFunDecl = shouldParse . parse funDecl ""

shouldParseProgram :: String -> AST -> Expectation
shouldParseProgram = shouldParse . parse program ""

main :: IO ()
main =
  hspec $ do
  describe "Expression parser" $ do
    describe "arithmetic parser" $ do
      it "parses integer literals" $ do
        shouldParseExpr "3" $ EInt 3
        shouldParseExpr "31234" $ EInt 31234
        shouldParseExpr "145674653" $ EInt 145674653
        shouldParseExpr "1234112343423413" $ EInt 1234112343423413
        shouldParseExpr "123412343" $ EInt 123412343
        shouldParseExpr "31234153423451243" $ EInt 31234153423451243
        shouldParseExpr "8567643737876983" $ EInt 8567643737876983
        shouldParseExpr "00000002137" $ EInt 2137
      it "parses addition" $ do
        shouldParseExpr "aAaAaAaA+bBbBbBbB" $ EVar "aAaAaAaA" ^+^ EVar "bBbBbBbB"
        shouldParseExpr "123412341243+aadfawevxz4231313245" $ EInt 123412341243 ^+^ EVar "aadfawevxz4231313245"
        shouldParseExpr "31423+a12341234" $ EInt 31423 ^+^ EVar "a12341234"
        shouldParseExpr "0039679887+a0000000kkkk" $ EInt 39679887 ^+^ EVar "a0000000kkkk"
        shouldParseExpr "0+0+3+9+67+a+0+k+k+k+k" $ (((((((((EInt 0 ^+^ EInt 0) ^+^ EInt 3) ^+^ EInt 9) ^+^ EInt 67) ^+^ EVar "a") ^+^ EInt 0) ^+^ EVar "k") ^+^ EVar "k") ^+^ EVar "k") ^+^ EVar "k"
        shouldParseExpr "2137+(-a0000000kkkk)" $ EInt 2137 ^+^ (ENeg $ EVar "a0000000kkkk")
      it "parses subtraction" $ do
        shouldParseExpr "3-a" $ EInt 3 ^-^ EVar "a"
        shouldParseExpr "3-a - 2137" $ (EInt 3 ^-^ EVar "a") ^-^ EInt 2137
      it "parses multiplication" $
        shouldParseExpr "3*a" $ EInt 3 ^*^ EVar "a"
      it "parses complex arithmetic expressions" $
        shouldParseExpr "((b + 1) * 3)" $ (EVar "b" ^+^ EInt 1) ^*^ EInt 3
    describe "function application parser" $ do
      it "parses simple expression application" $ do
        shouldParseExpr "a b" $ EVar "a" ^$^ EVar "b"
        shouldParseExpr "a 1" $ EVar "a" ^$^ EInt 1
        shouldParseExpr "3 1" $ EInt 3 ^$^ EInt 1
        shouldParseExpr "1234 1" $ EInt 1234 ^$^ EInt 1
        shouldParseExpr "a a12341" $ EVar "a" ^$^ EVar "a12341"
      it "parses complex application" $ do
        shouldParseExpr "f g h" $ (EVar "f" ^$^ EVar "g") ^$^ EVar "h"
        shouldParseExpr "a b c d e f g h" $ EApply (EApply (EApply (EApply (EApply (EApply (EApply (EVar "a") (EVar "b")) (EVar "c")) (EVar "d")) (EVar "e")) (EVar "f")) (EVar "g")) (EVar "h")
        shouldParseExpr "f (2137 + g) h" $ EApply (EApply (EVar "f") (EAdd (EInt 2137) (EVar "g"))) (EVar "h")
        shouldParseExpr "functionName 1 (functionName 2) (cuś) (functionName 4)" $ (((EVar "functionName" ^$^ EInt 1) ^$^ (EVar "functionName" ^$^ EInt 2)) ^$^ EVar "cuś") ^$^ (EVar "functionName" ^$^ EInt 4)
      it "parses lambdas" $ do
        shouldParseExpr "fn x . x + 2" $ ELambda [EVar "x"] $ EVar "x" ^+^ EInt 2
        shouldParseExpr "(fn x . x + 2) 2" $ ELambda [EVar "x"] (EVar "x" ^+^ EInt 2) ^$^ EInt 2
        shouldParseExpr "map (fn x . x + 2) list" $ (EVar "map" ^$^ ELambda [EVar "x"] (EVar "x" ^+^ EInt 2)) ^$^ EVar "list"
        shouldParseExpr "fnnnn" $ EVar "fnnnn"
        shouldParseExpr "kafafafn" $ EVar "kafafafn"
    describe "list parser" $ do
      it "parses list literals" $ do
        shouldParseExpr "[1, 2, 3, 4]" $ EListLiteral $ map EInt [1..4]
        shouldParseExpr "[a, b, c, d]" $ EListLiteral $ map EVar ["a", "b", "c", "d"]
        shouldParseExpr "[[], [], [], []]" $ EListLiteral $ map EListLiteral [[], [], [], []]
        shouldParseExpr "[[1, 2, 3], [[[]]], [[1], [2], [3]], []]" $ EListLiteral $ map EListLiteral [map EInt [1, 2, 3], [EListLiteral [EListLiteral []]], map (EListLiteral . (:[]) . EInt) [1, 2, 3], []]
      it "parses cons expressions" $ do
        shouldParseExpr "x:xs`" $ EVar "x" ^:^ EVar "xs"
        shouldParseExpr "x + b : xs" $ (EVar "x" ^+^ EVar "b") ^:^ EVar "xs"
        shouldParseExpr "x * y : concat xs" $ (EVar "x" ^*^ EVar "y") ^:^ (EVar "concat" ^$^ EVar "xs")
      it "parses list concatenation" $ do
        shouldParseExpr "xs ++ ys" $ EConcat (EVar "xs") (EVar "ys")
        shouldParseExpr "x : xs ++ ys" $ EConcat (ECons (EVar "x") (EVar "xs")) (EVar "ys")
        shouldParseExpr "xs ++ ys ++ zs" $ EConcat (EConcat (EVar "xs") (EVar "ys")) (EVar "zs")
        shouldParseExpr "x : xs ++ y : ys ++ z : zs" $ EConcat (EConcat (ECons (EVar "x") (EVar "xs")) (ECons (EVar "y") (EVar "ys"))) (ECons (EVar "z") (EVar "zs"))
    describe "boolean parser" $ do
      it "parses negation" $ do
        shouldParseExpr "not b" $ ENot (EVar "b")
        shouldParseExpr "not (b + 2)" $ ENot (EVar "b" ^+^ EInt 2)
      it "parses conjunction" $ do
        shouldParseExpr "x and y" $ EVar "x" ^&&^ EVar "y"
        shouldParseExpr "1 + x and 2 - y" $ (EInt 1 ^+^ EVar "x") ^&&^ (EInt 2 ^-^ EVar "y")
        shouldParseExpr "xand" $ EVar "xand"
        shouldParseExpr "andx" $ EVar "andx"
        shouldParseExpr "1 + x and 2 - y and 3 * z" $ ((EInt 1 ^+^ EVar "x") ^&&^ (EInt 2 ^-^ EVar "y")) ^&&^ (EInt 3 ^*^ EVar "z")
      it "parses disjunction" $ do
        shouldParseExpr "a or a" $ EVar "a" ^||^ EVar "a"
        shouldParseExpr "1 + x or 2 - y" $ (EInt 1 ^+^ EVar "x") ^||^ (EInt 2 ^-^ EVar "y")
        shouldParseExpr "xor" $ EVar "xor"
        shouldParseExpr "oreo" $ EVar "oreo"
        shouldParseExpr "1 and x or 2 and y" $ (EInt 1 ^&&^ EVar "x") ^||^ (EInt 2 ^&&^ EVar "y")
      it "parses equality" $ do
        shouldParseExpr "a == a " $ EVar "a" ^==^ EVar "a"
        shouldParseExpr "a + 2 == a * 2 " $ (EVar "a" ^+^ EInt 2) ^==^ (EVar "a" ^*^ EInt 2)
        shouldParseExpr "a == a and b == b " $ (EVar "a" ^==^ EVar "a") ^&&^ (EVar "b" ^==^ EVar "b")
        shouldParseExpr "a == a " $ EVar "a" ^==^ EVar "a"
      it "parses less than or equal" $ do
        shouldParseExpr "a <= a " $ EVar "a" ^<=^ EVar "a"
        shouldParseExpr "a + 2 <= a * 2 " $ (EVar "a" ^+^ EInt 2) ^<=^ (EVar "a" ^*^ EInt 2)
        shouldParseExpr "a <= a and b <= b " $ (EVar "a" ^<=^ EVar "a") ^&&^ (EVar "b" ^<=^ EVar "b")
        shouldParseExpr "a <= a " $ EVar "a" ^<=^ EVar "a"
    describe "control flow expressions parser" $ do
      it "parses if expressions" $ do
        shouldParseExpr "if a then a else a" $ EIf (EVar "a") (EVar "a") (EVar "a")
        shouldParseExpr "if2137then2else3" $ EVar "if2137then2else3"
        shouldParseExpr "if 2137 then 2 else 3" $ EIf (EInt 2137) (EInt 2) (EInt 3)
        shouldParseExpr "if a and b then fn . 2137 else 3 + 2" $ EIf (EVar "a" ^&&^ EVar "b") (ELambda [] $ EInt 2137) (EInt 3 ^+^ EInt 2)
      it "parses let .. in expressions" $ do
        shouldParseExpr "let x = 1 in x" $ ELet (EVar "x") (EInt 1) (EVar "x")
        shouldParseExpr "let x + 1 = 2137 in x + 2" $ ELet (EVar "x" ^+^ EInt 1) (EInt 2137) (EVar "x" ^+^ EInt 2)
        shouldParseExpr "let x = if a then b else c in fn z . x + 1" $ ELet (EVar "x") (EIf (EVar "a") (EVar "b") (EVar "c")) (ELambda [EVar "z"] $ EVar "x" ^+^ EInt 1)
        shouldParseExpr "letx+12137+ainx2" $ EVar "letx" ^+^ EInt 12137 ^+^ EVar "ainx2"
      it "parses match expressions" $ do
        shouldParseExpr "match x with | 2137 => 69420" $ EMatch (EVar "x") [(EInt 2137, EInt 69420)]
        shouldParseExpr "match x with | 2137 => 69420 | x : xs => 2137" $ EMatch (EVar "x") [(EInt 2137, EInt 69420), (EVar "x" ^:^ EVar "xs", EInt 2137)]
        shouldParseExpr "match x with | 2137 => 69420 | 2137 => 69420| 2137 => 69420| 2137 => 69420| 2137 => 69420| 2137 => 69420" $ EMatch (EVar "x") $ map (const (EInt 2137, EInt 69420)) [1..6]
  describe "Type parser" $ do
    it "parses type constructors" $ do
      shouldParseType "Abcds" $ TCtor "Abcds"
      shouldParseType "A123123bsdf324fds325" $ TCtor "A123123bsdf324fds325"
      shouldParseType "AAAAAAAAAAAAAAAAAAAAAAAA" $ TCtor "AAAAAAAAAAAAAAAAAAAAAAAA"
    it "parses function types" $ do
      shouldParseType "x -> y" $ TAbstract "x" ^->^ TAbstract "y"
      shouldParseType "x -> y -> y" $ TAbstract "x" ^->^ TAbstract "y" ^->^ TAbstract "y"
    it "parses type application" $ do
      shouldParseType "x a" $ TAbstract "x" ^$$^ TAbstract "a"
      shouldParseType "a b c d e " $ TAbstract "a" ^$$^ TAbstract "b" ^$$^ TAbstract "c" ^$$^ TAbstract "d" ^$$^ TAbstract "e"
      shouldParseType "x a -> Int" $ TAbstract "x" ^$$^ TAbstract "a" ^->^ TCtor "Int"
    it "parses different type parenthesizations" $ do
      shouldParseType "(a -> b) -> (b -> c) -> a -> c" $ (TAbstract "a" ^->^ TAbstract "b") ^->^ (TAbstract "b" ^->^ TAbstract "c") ^->^ TAbstract "a" ^->^ TAbstract "c"
      shouldParseType "m a -> m (a -> m b) -> m b" $ TAbstract "m" ^$$^ TAbstract "a" ^->^ TAbstract "m" ^$$^ (TAbstract "a" ^->^ TAbstract "m" ^$$^ TAbstract "b") ^->^ TAbstract "m" ^$$^ TAbstract "b"
  describe "Declaration parser" $ do
    it "parses type declarations" $ do
      shouldParseTypeDecl "type R = R Rational;" $ TypeDecl "R" [] [TypeVariant "R" [TCtor "Rational"]]
      shouldParseTypeDecl "type Maybe a = Just a | Nothing;" $ TypeDecl "Maybe" [TAbstract "a"] [TypeVariant "Just" [TAbstract "a"], TypeVariant "Nothing" []]
      shouldParseTypeDecl "type Either e a = Left e | Right a;" $ TypeDecl "Either" [TAbstract "e", TAbstract "a"] [TypeVariant "Left" [TAbstract "e"], TypeVariant "Right" [TAbstract "a"]]
      shouldParseTypeDecl "type Expr = EVar Name | EInt Int | ETypeName TypeName | EAdd Expr Expr | ENeg Expr | ESub Expr Expr | EMul Expr Expr | EApply Expr Expr;" $ TypeDecl "Expr" [] [TypeVariant "EVar" [TCtor "Name"], TypeVariant "EInt" [TCtor "Int"], TypeVariant "ETypeName" [TCtor "TypeName"], TypeVariant "EAdd" [TCtor "Expr", TCtor "Expr"], TypeVariant "ENeg" [TCtor "Expr"], TypeVariant "ESub" [TCtor "Expr", TCtor "Expr"], TypeVariant "EMul" [TCtor "Expr", TCtor "Expr"], TypeVariant "EApply" [TCtor "Expr", TCtor "Expr"]]
      shouldParseTypeDecl "type X a b = Y (a -> b) | Z (b -> a);" $ TypeDecl "X" [TAbstract "a", TAbstract "b"] [TypeVariant "Y" [TAbstract "a" ^->^ TAbstract "b"], TypeVariant "Z" [TAbstract "b" ^->^ TAbstract "a"]]
      parse typeDecl "" `shouldFailOn` "type X a b = Y a -> b;" --err 18 (utok '-' <> etok '(' <> etok '|' <> eeof <> etoks "identifier" <> "typename")
    it "parses function declarations" $ do
      shouldParseFunDecl "(b -> c) -> (b -> c) :: f a b = a b;" $ FunDecl ((TAbstract "b" ^->^ TAbstract "c") ^->^ TAbstract "b" ^->^ TAbstract "c") "f" (map EVar ["a", "b"]) $ EVar "a" ^$^ EVar "b"
      shouldParseFunDecl "m (b -> c) -> m b -> Evald (m c) :: f x y = Evald ((getOut x) y);" $ FunDecl (TAbstract "m" ^$$^ (TAbstract "b" ^->^ TAbstract "c") ^->^ TAbstract "m" ^$$^ TAbstract "b" ^->^ TCtor "Evald" ^$$^ (TAbstract "m" ^$$^ TAbstract "c")) "f" (map EVar ["x", "y"]) $ ETypeName "Evald" ^$^ ((EVar "getOut" ^$^ EVar "x") ^$^ EVar "y")
    it "parses programs" $
      shouldParseProgram "type R = R Rational;\n\
       \type Maybe a = Just a | Nothing;\n\
       \type Either e a = Left e | Right a;\n\
       \type Expr = EVar Name | EInt Int | ETypeName TypeName | EAdd Expr Expr | ENeg Expr | ESub Expr Expr | EMul Expr Expr | EApply Expr Expr;\n\
       \type X a b = Y (a -> b) | Z (b -> a);\n\
       \(b -> c) -> (b -> c) :: f a b = a b;\n\
       \m (b -> c) -> m b -> Evald (m c) :: f x y = Evald ((getOut x) y);\n" $
      AST [TypeDecl "R" [] [TypeVariant "R" [TCtor "Rational"]], TypeDecl "Maybe" [TAbstract "a"] [TypeVariant "Just" [TAbstract "a"], TypeVariant "Nothing" []], TypeDecl "Either" [TAbstract "e", TAbstract "a"] [TypeVariant "Left" [TAbstract "e"], TypeVariant "Right" [TAbstract "a"]], TypeDecl "Expr" [] [TypeVariant "EVar" [TCtor "Name"], TypeVariant "EInt" [TCtor "Int"], TypeVariant "ETypeName" [TCtor "TypeName"], TypeVariant "EAdd" [TCtor "Expr", TCtor "Expr"], TypeVariant "ENeg" [TCtor "Expr"], TypeVariant "ESub" [TCtor "Expr", TCtor "Expr"], TypeVariant "EMul" [TCtor "Expr", TCtor "Expr"], TypeVariant "EApply" [TCtor "Expr", TCtor "Expr"]], TypeDecl "X" [TAbstract "a", TAbstract "b"] [TypeVariant "Y" [TAbstract "a" ^->^ TAbstract "b"], TypeVariant "Z" [TAbstract "b" ^->^ TAbstract "a"]]]
      [FunDecl ((TAbstract "b" ^->^ TAbstract "c") ^->^ TAbstract "b" ^->^ TAbstract "c") "f" (map EVar ["a", "b"]) $ EVar "a" ^$^ EVar "b", FunDecl (TAbstract "m" ^$$^ (TAbstract "b" ^->^ TAbstract "c") ^->^ TAbstract "m" ^$$^ TAbstract "b" ^->^ TCtor "Evald" ^$$^ (TAbstract "m" ^$$^ TAbstract "c")) "f" (map EVar ["x", "y"]) $ ETypeName "Evald" ^$^ ((EVar "getOut" ^$^ EVar "x") ^$^ EVar "y")]