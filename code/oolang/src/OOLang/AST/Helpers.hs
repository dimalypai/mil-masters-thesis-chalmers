-- | Helper functions for working with AST.
module OOLang.AST.Helpers where

import OOLang.AST

-- * Getters

getFunParams :: FunType s -> [VarBinder s]
getFunParams (FunType _ varBinders _) = varBinders

getFunReturnType :: FunType s -> TypeS s
getFunReturnType (FunType _ _ retType) = retType

getInitOpS :: Init t s -> InitOpS s
getInitOpS (Init _ initOpS _) = initOpS

getDeclVarName :: Declaration t s -> VarS s
getDeclVarName (Decl _ _ varBinder _) = getBinderVar varBinder

getDeclVarType :: Declaration t s -> TypeS s
getDeclVarType (Decl _ _ varBinder _) = getBinderType varBinder

getBinderVar :: VarBinder s -> VarS s
getBinderVar (VarBinder _ srcVar _ ) = srcVar

getBinderType :: VarBinder s -> TypeS s
getBinderType (VarBinder _ _ srcType) = srcType

-- | Returns an underlying type. For Mutable A it is A, for everything else it
-- is just the type itself. Used with assignment type checking.
-- Mutable is more modifier-like than a type-like, comparing to Ref, for
-- example, that's why we have this special treatment.
getUnderType :: Type -> Type
getUnderType (TyMutable t) = t
getUnderType            t  = t

-- | Removes 'TyPure' on the top-level if there is one.
stripPureType :: Type -> Type
stripPureType (TyPure t) = t
stripPureType         t  = t

-- | Implementation is stolen from 'Data.List.partition'. This is a specialised
-- version of it.
partitionClassMembers :: [MemberDecl t s] -> ([FieldDecl t s], [MethodDecl t s])
partitionClassMembers = foldr selectClassMember ([], [])
  where selectClassMember member ~(fs, ms) | FieldMemberDecl fd  <- member = (fd:fs, ms)
                                           | MethodMemberDecl md <- member = (fs, md:ms)

-- * Setters

setVarBinderAnn :: VarBinder s -> s -> VarBinder s
setVarBinderAnn (VarBinder _ v t) s = VarBinder s v t

-- * Predicates

isAssignRef :: AssignOp -> Bool
isAssignRef AssignRef {} = True
isAssignRef            _ = False

-- * Synonyms

getClassName :: ClassNameS s -> ClassName
getClassName = fst

getFunName :: FunNameS s -> FunName
getFunName = fst

getMemberName :: MemberNameS s -> MemberName
getMemberName = fst

getVar :: VarS s -> Var
getVar = fst

getBinOp :: BinOpS s -> BinOp
getBinOp = fst

getAssignOp :: AssignOpS s -> AssignOp
getAssignOp = fst

getInitOp :: InitOpS s -> InitOp
getInitOp = fst

-- * Conversions

varToFunName :: Var -> FunName
varToFunName (Var varName) = FunName varName

memberNameToVar :: MemberName -> Var
memberNameToVar (MemberName nameStr) = Var nameStr

memberNameToFunName :: MemberName -> FunName
memberNameToFunName (MemberName nameStr) = FunName nameStr

varToMemberName :: Var -> MemberName
varToMemberName (Var nameStr) = MemberName nameStr

funNameToMemberName :: FunName -> MemberName
funNameToMemberName (FunName nameStr) = MemberName nameStr

-- * Constructors

-- | Adds 'TyPure' on top of the given type if the flag is True.
mkTypeWithPurity :: Bool -> Type -> Type
mkTypeWithPurity shouldBePure t =
  if shouldBePure
    then TyPure t
    else t

-- * Type predicates

-- | Entity of this type is either an already computed value or a global
-- function without arguments.
--
-- For example, function parameter of type Unit has value type (it can be only
-- unit value, since we are in a strict language) and global function main :
-- Unit has value type, but it denotes a computation returning unit, not an
-- already computed value.
isValueType :: Type -> Bool
isValueType TyArrow {} = False
isValueType _ = True

-- | It can be either, for example Pure Int (it doesn't have arguments and
-- purely computes a value of type Int) or a function type with like Int ->
-- Float -> Pure Int, which takes arguments and its return type signals that it
-- is a pure function that delivers and integer value.
isPureType :: Type -> Bool
isPureType (TyPure _) = True
isPureType (TyArrow _ t2) = isPureType t2
isPureType _ = False

isMutableType :: Type -> Bool
isMutableType TyMutable {} = True
isMutableType            _ = False

-- | By immutable type we mean anything else than Mutable and Ref.
--
-- Note: 'isMutable' means only Mutable, not Ref.
isImmutableType :: Type -> Bool
isImmutableType TyMutable {} = False
isImmutableType TyRef     {} = False
isImmutableType            _ = True

isRefType :: Type -> Bool
isRefType TyRef {} = True
isRefType        _ = False

isMaybeType :: Type -> Bool
isMaybeType TyMaybe {} = True
isMaybeType          _ = False

-- | Checks whether it is Maybe, Mutable Maybe or Ref Maybe type.
-- Variables of these types can be uninitialised and have Nothing as a value.
hasMaybeType :: Type -> Bool
hasMaybeType TyMaybe {} = True
hasMaybeType (TyMutable (TyMaybe {})) = True
hasMaybeType (TyRef     (TyMaybe {})) = True
hasMaybeType _ = False

isAtomicType :: Type -> Bool
isAtomicType TyArrow   {} = False
isAtomicType TyPure    {} = False
isAtomicType TyMaybe   {} = False
isAtomicType TyMutable {} = False
isAtomicType TyRef     {} = False
isAtomicType            _ = True

-- * Type precedences

getTypePrec :: Type -> Int
getTypePrec TyUnit       = 3
getTypePrec TyBool       = 3
getTypePrec TyInt        = 3
getTypePrec TyFloat      = 3
getTypePrec TyString     = 3
getTypePrec TyClass   {} = 3
getTypePrec TyArrow   {} = 1
getTypePrec TyPure    {} = 2
getTypePrec TyMaybe   {} = 2
getTypePrec TyMutable {} = 2
getTypePrec TyRef     {} = 2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
--
-- Note: It is reflexive: t `typeHasLowerPrec` t ==> True
typeHasLowerPrec :: Type -> Type -> Bool
typeHasLowerPrec t1 t2 = getTypePrec t1 <= getTypePrec t2

-- | Returns whether the first type operator has a lower precedence than the
-- second one. Convenient to use in infix form.
-- This version can be used with associative type operators, for example:
-- arrow, type application. See "OOLang.AST.PrettyPrinter".
--
-- Note: It is *not* reflexive: t `typeHasLowerPrecAssoc` t ==> False
typeHasLowerPrecAssoc :: Type -> Type -> Bool
typeHasLowerPrecAssoc t1 t2 = getTypePrec t1 < getTypePrec t2

-- * Parsing helpers

isClassDef :: TopDef t s -> Bool
isClassDef (TopClassDef _) = True
isClassDef               _ = False

isFunDef :: TopDef t s -> Bool
isFunDef (TopFunDef _) = True
isFunDef             _ = False

