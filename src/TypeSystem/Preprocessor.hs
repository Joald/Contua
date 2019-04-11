module TypeSystem.Preprocessor where

import TypeSystem.TypeDefs
import Semantics.Builtins
import Parser.TypeDefs

-- TODO: check patterns in match before desugaring

preprocess :: AST -> IAST
preprocess (AST types fns) = IAST types $ map convertFn fns
  where
    convertFn (FunDecl fnType fnName fnArgs fnBody) = IFn fnType fnName fnArgs $ desugar fnBody

infixl 9 ^^$
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
desugar (EMatch e pats) =
  let e' = desugar e
    in foldr (\(pat, expr) -> IEApply $ IEVar ifteName ^^$ (IEVar matchesName ^^$ e' ^^$ IPat (desugar pat)) ^^$ desugar expr) (ILit LError) pats
