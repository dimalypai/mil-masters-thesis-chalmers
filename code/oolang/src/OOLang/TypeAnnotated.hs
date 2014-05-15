-- | Module that defines an interface for working with type annotations of the
-- AST.
module OOLang.TypeAnnotated where

-- | AST nodes which contain fields of type t should be an instance of the
-- 'TypeAnnotated' type class.
class TypeAnnotated ast where
  getTypeOf :: ast t s -> t

