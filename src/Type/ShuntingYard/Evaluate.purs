-- | A simple reimplementation of `typelevel-eval`
-- | making use of kind polymorphism.
module Type.ShuntingYard.Evaluate where

import Type.Proxy (Proxy(..))


data TypeExpr :: forall k. k -> Type
data TypeExpr r


class Evaluate :: forall k. TypeExpr k -> k -> Constraint
class Evaluate e r | e -> r


evaluate :: forall e r. Evaluate e r => Proxy e -> Proxy r
evaluate _ = Proxy


foreign import data Compose
  :: forall a b c
   . (b -> TypeExpr c)
  -> (a -> TypeExpr b)
  -> a
  -> TypeExpr c

instance evaluateCompose
  :: ( Evaluate (g a) b
     , Evaluate (f b) c
     )
  => Evaluate (Compose f g a) c
