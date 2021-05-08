module Type.ShuntingYard.Lexer where

import Type.Data.Boolean (class And)
import Type.Data.List (type (:>), Nil')
import Type.Data.Peano as Peano
import Type.Proxy (Proxy(..))
import Type.ShuntingYard.Evaluate (class Evaluate, Compose, TypeExpr, evaluate)
import Type.ShuntingYard.Prelude (class ChopT, class FilterT, class GroupByT, class MapT, IsNatural, Join, NotEquals)
import Type.ShuntingYard.Types (LeftP, NaturalToken, OperatorToken, ParenToken, Plus, ReadToken, RightP, Tokens)


foreign import data NaturalGrouper :: Symbol -> Symbol -> TypeExpr Boolean

instance naturalGrouperEvaluate
  :: ( Evaluate (IsNatural k) k'
     , Evaluate (IsNatural l) l'
     , And k' l' r
     )
  => Evaluate (NaturalGrouper k l) r


foreign import data Tokenize :: Symbol -> TypeExpr Tokens

instance tokenizeEvaluate
  :: ( ChopT ws xs
     , GroupByT NaturalGrouper xs ys
     , FilterT (NotEquals (" " :> Nil')) ys zs
     , MapT (Compose ReadToken Join) zs zs'
     )
  => Evaluate (Tokenize ws) zs'


f :: Proxy
   ( ParenToken LeftP :>
     NaturalToken Peano.D50 :>
     OperatorToken Plus :>
     NaturalToken Peano.D50 :>
     ParenToken RightP :>
     Nil'
   )
f = evaluate (Proxy :: Proxy (Tokenize "(50 + 50)"))
