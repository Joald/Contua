import Control.Exception
import Test.Hspec
import Test.Hspec.Megaparsec

import Parser.Utils
import Parser.TypeDefs
import Parser.Parser
import Control.Monad
import Text.Megaparsec (runParser, parse)


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
      it "parses subtraction" $
        parse program "" "type :: a=3-a;" `shouldParse` AST [] [FunDecl TInt "a" [] (ESub (EInt 3) (EVar "a"))]
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
      it "parser complex application" $ do
        parse program "" "type::a= f g h;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EVar "f") (EVar "g")) (EVar "h")]
        parse program "" "type::a= a b c d e f g h;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EApply (EApply (EApply (EApply (EApply (EVar "a") (EVar "b")) (EVar "c")) (EVar "d")) (EVar "e")) (EVar "f")) (EVar "g")) (EVar "h")]
        parse program "" "type::a= f (2137 + g) h;" `shouldParse` AST [] [FunDecl TInt "a" [] $ EApply (EApply (EVar "f") (EAdd (EInt 2137) (EVar "g"))) (EVar "h")]


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