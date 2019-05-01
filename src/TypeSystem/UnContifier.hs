{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module TypeSystem.UnContifier where

import Control.Monad.Except
import Control.Applicative (liftA2)

import TypeSystem.TypeDefs
import Parser.TypeDefs
import Utils

type UnCont a = Except String a

unContFn :: IFnDecl -> UnCont IFnDecl
unContFn fn @ IFnDecl {ifnContType, ifnType, .. } = liftA2 (\ct t -> fn {
  ifnContType = ct, ifnType = t
}) (mapM unContType ifnContType) (mapM unContType ifnType)

unContTypes :: TypeDecl -> UnCont ITypeDecl
unContTypes td @ TypeDecl {tdVariants, ..} = (\vs -> td { tdVariants = vs }) <$> mapM unContTypeVariant tdVariants

unContTypeVariant :: TypeVariant -> UnCont TypeVariant
unContTypeVariant (TypeVariant name args) = TypeVariant name <$> mapM unContType args

unContType :: Type -> UnCont Type
unContType (TCont (Just tc) t) = do
  let (args, body) = pap (typeArgs, typeBody) t
  when (null args) . throwError $ "Cannot add continuation to non-function type " ++ show t
  return $ foldr1 (^->^) $ args ++ [body ^->^ tc, tc]
unContType (TArrow t1 t2) = liftA2 TArrow (unContType t1) (unContType t2)
unContType (TApply t1 t2) = liftA2 TApply (unContType t1) (unContType t2)
unContType (TList t) = fmap TList (unContType t)
unContType t = return t
