module TypeSystem.KindCheckerSpec where

import qualified Data.Map as Map
import Data.Map (Map)

import Test.Hspec

import TestUtils
import TypeSystem.KindChecker
import TypeSystem.TypeDefs
import TypeSystem.Preprocessor
import Parser.TypeDefs
import Parser.Parser
import Parser.ParserSpec
import Text.Megaparsec
import Parser.Utils
import TypeSystem.Substitutable
import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Control.Monad.Except (Except)

spec :: Spec
spec = do
  kindParserTest
  kindCheckerTest

defaultEnv :: Map Name Kind
defaultEnv = Map.fromList [("Int", KStar)]

kindCheckWithDefaultEnv :: KindState -> KindCheck a -> Except KindError a
kindCheckWithDefaultEnv = runKindCheck defaultEnv

kindCheckDefault :: KindCheck a -> Except KindError a
kindCheckDefault = kindCheckWithDefaultEnv initState

kindCheckFailedMessage :: String
kindCheckFailedMessage = "kind check failed"

kindCheckTypeDeclarationTest :: String -> Kind
kindCheckTypeDeclarationTest = extract (KUnknown kindCheckFailedMessage) . kindCheckDefault . kindCheckTypeDecl . parseTypeDecl
kindCheckTypeDeclarationsTest :: String -> Map Name Kind
kindCheckTypeDeclarationsTest = extract (Map.singleton kindCheckFailedMessage KStar) . kindCheckDefault . kindCheckTypeDecls . parseTypeDecls

kindCheckTypeTest :: String -> (Kind, KindSubst)
kindCheckTypeTest = extract (KUnknown kindCheckFailedMessage, nullSubst) . kindCheckDefault . kindCheckType . parseType

kindCheckIASTTest :: String -> Map Name Kind
kindCheckIASTTest = extract' (`Map.singleton` KStar) . kindCheckIAST . preprocess . parseAST

kindOperatorTable :: [[Operator Parser Kind]]
kindOperatorTable = [[binaryR "->" KArrow]]

kindTerm :: Parser Kind
kindTerm = choice
  [ try $ parens kindParser
  , try $ symbol "*" >> return KStar
  ]

kindParser :: Parser Kind
kindParser = makeExprParser kindTerm kindOperatorTable

kparse :: String -> Kind
kparse = parseHelper (KUnknown "parse failed") kindParser

infixr -->
(-->) :: Kind -> Kind -> Kind
(-->) = KArrow

fooKind1str, fooKind1'str, stateTKindStr :: String
fooKind1str = "(* -> *) -> ((* -> *) -> *) -> *"
fooKind1'str = "((* -> *) -> *) -> (* -> *) -> *"
stateTKindStr =  "* -> (* -> *) -> * -> *"

kindShouldBe :: Kind -> Kind -> Expectation
k1 `kindShouldBe` k2 = monomorphise k1 `shouldBe` monomorphise k2

kindParseTest :: String -> Expectation
kindParseTest s = shouldBe s $ show $ kparse s

kindParserTest :: Spec
kindParserTest = describe "Kind parser" $
  it "parses kinds" $ do
    kindParseTest fooKind1str
    kindParseTest fooKind1'str
    kindParseTest stateTKindStr

kindCheckerTest :: Spec
kindCheckerTest = describe "Kind checker" $ do
  it "checks kinds of type declarations" $ do
    kindCheckTypeDeclarationTest fooProgram1 `kindShouldBe` kparse fooKind1str
    kindCheckTypeDeclarationTest fooProgram1' `kindShouldBe` kparse fooKind1'str
    kindCheckTypeDeclarationTest stateTProgram `kindShouldBe` kparse stateTKindStr
  it "checks kinds of lists of type declarations" $
    monomorphise' (kindCheckTypeDeclarationsTest fooProgram1) `shouldBe` Map.singleton "Foo" (kparse fooKind1str) <> defaultEnv
  it "checks kinds of programs" $ do
    kindCheckIASTTest fooProgram1 `shouldBe` Map.singleton "Foo" (kparse fooKind1str)
    kindCheckIASTTest fooProgram1' `shouldBe` Map.singleton "Foo" (kparse fooKind1'str)
    kindCheckIASTTest (fooProgram1 ++ stateTProgram) `shouldBe` Map.fromList (zip ["Foo", "StateT"] [kparse fooKind1str, kparse stateTKindStr])
    kindCheckIASTTest fooProgram2 `shouldBe` Map.fromList (zip ["Foo", "StateT", "Barium"] [kparse fooKind1str, kparse stateTKindStr, kparse "* -> * -> * -> (* -> *) -> * -> * -> *"])
