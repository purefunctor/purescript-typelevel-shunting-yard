module Type.ShuntingYard.Lexer where

import Type.Data.Boolean (class And)
import Type.Data.List (type (:>), Cons', List', Nil')
import Type.Proxy (Proxy(..))
import Type.ShuntingYard.Evaluate (class Evaluate, TypeExpr, evaluate)
import Type.ShuntingYard.Prelude (class ChopT, class FilterT, class GroupByT, class MapT, IsNatural, Join, NotEquals)


foreign import data GroupingPredicate :: Symbol -> Symbol -> TypeExpr Boolean

instance groupingPredicateEvaluate
  :: ( Evaluate (IsNatural k) k'
     , Evaluate (IsNatural l) l'
     , And k' l' r
     )
  => Evaluate (GroupingPredicate k l) r


foreign import data Tokenize
  :: Symbol
  -> TypeExpr (List' Symbol)

instance tokenizeEvaluate
  :: ( ChopT ws xs
     , GroupByT GroupingPredicate xs ys
     , FilterT (NotEquals (" " :> Nil')) ys zs
     , MapT Join zs zs'
     )
  => Evaluate (Tokenize ws) zs'


f :: Proxy (Cons' "(" (Cons' "1234" (Cons' "+" (Cons' "1234" (Cons' ")" Nil')))))
f = evaluate (Proxy :: Proxy (Tokenize "(1234 + 1234)"))
