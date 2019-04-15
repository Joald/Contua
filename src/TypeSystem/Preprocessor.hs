module TypeSystem.Preprocessor where

import TypeSystem.TypeDefs
import Semantics.Builtins
import Parser.TypeDefs
import Data.Bifunctor (first, second)

preprocess :: AST -> IAST
preprocess (AST types fns) = IAST types $ map convertFn fns
  where
    convertFn (FunDecl _fnContType _fnType _fnName _fnArgs _fnBody) = IFn _fnContType _fnType _fnName _fnArgs $ desugar _fnBody

infixl 9 ^^$
(^^$) :: IExpr -> IExpr -> IExpr
(^^$) = IEApply

desugar :: Expr -> IExpr
desugar (EVar x) = IEVar x
desugar (EInt x) = ILit $ LInt x
desugar (ETypeName x) = IEVar x
desugar (EAdd e1 e2) = IEVar addName ^^$ desugar e1 ^^$ desugar e2
desugar (ESub e1 e2) = IEVar subName ^^$ desugar e1 ^^$ desugar e2
desugar (EMul e1 e2) = IEVar mulName ^^$ desugar e1 ^^$ desugar e2
desugar (EApply e1 e2) = IEApply (desugar e1) $ desugar e2
desugar (ELambda args body) = foldr IEAbstract (desugar body) args
desugar (EListLiteral list) = foldr (IEApply . IEApply (IEVar consName) . desugar) (ILit LEmptyList) list
desugar (ECons e1 e2) = IEVar consName ^^$ desugar e1 ^^$ desugar e2
desugar (EConcat e1 e2) = IEVar concName ^^$ desugar e1 ^^$ desugar e2
desugar (EAnd e1 e2) = IEVar andName ^^$ desugar e1 ^^$ desugar e2
desugar (EOr e1 e2) = IEVar orName ^^$ desugar e1 ^^$ desugar e2
desugar (EEq e1 e2) = IEVar eqName ^^$ desugar e1 ^^$ desugar e2
desugar (ELeq e1 e2) = IEVar leqName ^^$ desugar e1 ^^$ desugar e2
desugar (ENeg e) = IEVar negName ^^$ desugar e
desugar (ENot e) = IEVar notName ^^$ desugar e
desugar (ELet x e1 e2) = IELet x (desugar e1) $ desugar e2
desugar (EIf b e1 e2) = IEApply (IEApply (IEVar ifteName ^^$ desugar b) $ desugar e1) $ desugar e2
desugar (EMatch e pats) = desugarMatch e pats

desugarMatch :: Expr -> [(Expr, Expr)] -> IExpr
desugarMatch e xs = uncurry (flip IMatch (desugar e)) (unzip $ map (first toPattern . second desugar) xs)

toPattern :: Expr -> Pattern
toPattern (EListLiteral l) = foldr (\x a -> PCons (toPattern x) a) (PLit LEmptyList) l
toPattern (ECons x xs) = PCons (toPattern x) (toPattern xs)
toPattern (EVar name) = PVar name
toPattern e@(EApply e1 _) | ETypeName name <- leftmost e1 =
  let args = gatherArgs e
    in PTVariant name $ map toPattern args
toPattern (EInt x) = PLit (LInt x)
toPattern (ETypeName name) = PTVariant name []
toPattern _ = error "This should never happen: it isn't your fault."