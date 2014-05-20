-- | Module that defines an interface for working with purity annotations of
-- the AST.
module OOLang.PureAnnotated where

import OOLang.AST (Type)

-- | AST nodes for which it makes sense to have a purity indicator should be an
-- instance of the 'PureAnnotated' type class.
--
-- Note the explicit 'Type' type argument. This is done to ensure that we query
-- only the type checked nodes (since before the type checking purity
-- indicators are always False).
class PureAnnotated ast where
  getPurityOf :: ast Type s -> Bool

