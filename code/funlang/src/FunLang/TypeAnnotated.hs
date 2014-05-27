-- | Module that defines an interface for working with type annotations of the
-- AST.
module FunLang.TypeAnnotated where

import FunLang.AST

-- | AST nodes for which it makes sense to have a type should be an instance of
-- the 'TypeAnnotated' type class.
--
-- Note the explicit 'Type' type argument. This is done to ensure that we query
-- only the type checked nodes.
class TypeAnnotated ast where
  getTypeOf :: ast Type s -> Type

