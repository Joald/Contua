{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module TypeSystem.PatternChecker where

import qualified Data.Map as Map
import Data.Map (Map)


import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe (isNothing, fromJust)
import Data.List ((\\), intercalate)

import Parser.TypeDefs
import Debug.Trace (traceM)


type PatternError = String

checkPatterns :: AST -> CoverageCheck ()
checkPatterns (AST _ fns) = mapM_ (checkPatternsExp . fnBody) fns

checkPatternsExp :: Expr -> CoverageCheck ()
checkPatternsExp (EAdd e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (ESub e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EMul e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EApply e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (ELambda _ e) = checkPatternsExp e
checkPatternsExp (EListLiteral list) = mapM_ checkPatternsExp list
checkPatternsExp (ECons e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EConcat e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EAnd e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EOr e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EEq e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (ELeq e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (ENeg e) = checkPatternsExp e
checkPatternsExp (ENot e) = checkPatternsExp e
checkPatternsExp (ELet _ e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EIf _ e1 e2) = checkPatternsExp e1 >> checkPatternsExp e2
checkPatternsExp (EMatch e pats) = checkPatternsExp e >> checkExhaustiveness e (map fst pats)
checkPatternsExp _ = return ()

checkExhaustiveness :: Expr -> [Expr] -> CoverageCheck ()
checkExhaustiveness _ [] = throwError "An empty pattern list is non-exhaustive."
checkExhaustiveness e list = mapM toCoverage list >>= foldM (><) NoCoverage >>= \res -> when (res /= Anything) $ throwError $ "Pattern group for expression " ++ show e ++ " is non-exhaustive."

toCoverage :: Expr -> CoverageCheck Coverage
toCoverage (EVar _) = return Anything
toCoverage (EListLiteral list) = foldM (\a x -> flip SpecificFirstElem a <$> toCoverage x) EmptyList list
toCoverage (ECons x xs) = toCoverage x >>= \case
                                             Anything -> AnyFirstElem <$> toCoverage xs
                                             res -> SpecificFirstElem res <$> toCoverage xs
toCoverage e@(EApply e1 _) | ETypeName name <- leftmost e1 =
  do
    tName <- findType name
    variantArgs <- mapM toCoverage $ gatherArgs e
    return $ Ctors tName $ Map.fromList [(name, variantArgs)]

toCoverage (EInt _) = return ExactInt
toCoverage (ETypeName name) = Ctors <$> findType name <*> pure (Map.singleton name [])
toCoverage e = throwError $ "Cannot use expression " ++ show e ++ " in pattern matching."

findType :: Name -> CoverageCheck Name
findType ctorName =
  do
    mt <- asks $ Map.filter (any ((ctorName ==) . tvName) . tdVariants)
    when (Map.null mt) . throwError $ "Cannot find a type with constructor " ++ ctorName
    when (Map.size mt > 1) . throwError $ "Ambiguous reference to type constructor " ++ ctorName ++ ", it matches types: " ++ intercalate ", " (Map.keys mt)
    return . head $ Map.keys mt


type CoverageEnv = Map Name TypeDecl

type CoverageCheck a = ReaderT CoverageEnv (Except PatternError) a

runCoverageCheck :: CoverageEnv -> CoverageCheck a -> Either PatternError a
runCoverageCheck env cc = runExcept $ runReaderT cc env

data Coverage =
    NoCoverage
  | Anything
  | EmptyList
  | AnyFirstElem Coverage
  | AnyFirstElemOrEmpty Coverage
  | SpecificFirstElem Coverage Coverage
  | SpecificFirstElemOrEmpty Coverage Coverage
  | Ctors Name (Map Name [Coverage])
  | ExactInt
  deriving (Eq, Show)


(><) :: Coverage -> Coverage -> CoverageCheck Coverage
-- No coverage
NoCoverage >< x = return x
x >< NoCoverage = return x

-- Anything
Anything >< _ = return Anything
_ >< Anything = return Anything

-- Empty list
EmptyList >< EmptyList = return EmptyList

EmptyList >< AnyFirstElem Anything = return Anything
AnyFirstElem Anything >< EmptyList = return Anything

EmptyList >< AnyFirstElem x = return $ AnyFirstElemOrEmpty x
AnyFirstElem x >< EmptyList = return $ AnyFirstElemOrEmpty x

EmptyList >< r@(AnyFirstElemOrEmpty _) = return r
l@(AnyFirstElemOrEmpty _) >< EmptyList = return l

EmptyList >< (SpecificFirstElem x y) = return $ SpecificFirstElemOrEmpty x y
(SpecificFirstElem x y) >< EmptyList = return $ SpecificFirstElemOrEmpty x y

EmptyList >< r@(SpecificFirstElemOrEmpty _ _) = return r
l@(SpecificFirstElemOrEmpty _ _) >< EmptyList = return l

-- Any first element
AnyFirstElem x >< AnyFirstElem y = AnyFirstElem <$> x >< y

AnyFirstElemOrEmpty x >< AnyFirstElem y = (\case
                                             Anything -> Anything
                                             z -> AnyFirstElemOrEmpty z) <$> x >< y
l@(AnyFirstElem _) >< r@(AnyFirstElemOrEmpty _) = r >< l

l@(AnyFirstElem _) >< SpecificFirstElem _ y = l >< AnyFirstElem y
SpecificFirstElem _ y >< r@(AnyFirstElem _) = r >< AnyFirstElem y

l@(AnyFirstElem _) >< SpecificFirstElemOrEmpty _ y = l >< AnyFirstElemOrEmpty y
SpecificFirstElemOrEmpty _ y >< r@(AnyFirstElem _) = r >< AnyFirstElemOrEmpty y

-- Any first element or empty
AnyFirstElemOrEmpty x >< AnyFirstElemOrEmpty y = AnyFirstElemOrEmpty <$> x >< y

AnyFirstElemOrEmpty x >< (SpecificFirstElem _ y) = AnyFirstElemOrEmpty <$> x >< y
(SpecificFirstElem _ y) >< AnyFirstElemOrEmpty x = AnyFirstElemOrEmpty <$> x >< y

AnyFirstElemOrEmpty x >< (SpecificFirstElemOrEmpty _ y) = AnyFirstElemOrEmpty <$> x >< y
(SpecificFirstElemOrEmpty _ y) >< AnyFirstElemOrEmpty x = AnyFirstElemOrEmpty <$> x >< y

-- Specific first element
SpecificFirstElem l x >< SpecificFirstElem r y = SpecificFirstElem <$> l >< r <*> x >< y

SpecificFirstElem l x >< SpecificFirstElemOrEmpty r y = SpecificFirstElemOrEmpty <$> l >< r <*> x >< y
SpecificFirstElemOrEmpty l x >< SpecificFirstElem r y = SpecificFirstElemOrEmpty <$> l >< r <*> x >< y

-- Specific first element or empty
SpecificFirstElemOrEmpty l x >< SpecificFirstElemOrEmpty r y = SpecificFirstElemOrEmpty <$> l >< r <*> x >< y

-- Constructors
Ctors lName lMap >< Ctors rName rMap =
  do when (lName /= rName) . throwError $ "Cannot match type " ++ lName ++ " with type " ++ rName ++ "."
     newMap <- Map.fromList <$> mergeVariants (Map.toList lMap) (Map.toList rMap)
     env <- ask
     let mtDecl = Map.lookup lName env
     when (isNothing mtDecl) . throwError $ "Unbound type name: " ++ lName
     let (TypeDecl _ _ variants) = fromJust mtDecl
     traceM $ "Merging " ++ show lMap ++ " with " ++ show rMap
     traceM $ "merged variants are: " ++ show newMap
     traceM $ "variants: " ++ show variants
     return $ if all (== Anything) (concat $ Map.elems newMap) && haveSameElements (map tvName variants) (Map.keys newMap)
                then Anything
                else Ctors lName newMap

-- Integer literal.
ExactInt >< ExactInt = return ExactInt

-- otherwise
c1 >< c2 = throwError $ "Cannot merge coverage " ++ show c1 ++ " with " ++ show c2

haveSameElements :: Eq a => [a] -> [a] -> Bool
haveSameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

mergeVariants :: [(Name, [Coverage])] -> [(Name, [Coverage])] -> CoverageCheck [(Name, [Coverage])]
mergeVariants [] [] = return []
mergeVariants [] xs = return xs
mergeVariants xs [] = return xs
mergeVariants x@((lName, lCoverage):xs) y@((rName, rCoverage):ys)
  | lName == rName = (:) <$> ((lName, ) <$> zipWithM (><) lCoverage rCoverage) <*> mergeVariants xs ys
  | lName < rName = ((lName, lCoverage):) <$> mergeVariants xs y
  | lName > rName = ((rName, rCoverage):) <$> mergeVariants x ys
  | otherwise = error "this will never happen as other patterns are exhaustive yet ghc claims otherwise."
