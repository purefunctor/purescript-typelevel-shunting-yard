module Type.ShuntingYard.Lexer where

import Type.Data.Boolean (class And)
import Type.Data.List (type (:>), Nil')
import Type.Data.Peano as Peano
import Type.Proxy (Proxy(..))
import Type.ShuntingYard.Evaluate (class Evaluate, Compose, TypeExpr, evaluate)
import Type.ShuntingYard.Prelude (class ChopT, class FilterT, class GroupByT, class MapT, IsNatural, Join, NotEquals)
import Type.ShuntingYard.Types (LeftP, NaturalToken, OperatorToken, ParenToken, Plus, ReadToken, RightP, Tokens)

foreign import data NaturalGrouper ∷ Symbol → Symbol → TypeExpr Boolean

instance naturalGrouperEvaluate ∷
  ( Evaluate (IsNatural k) k'
  , Evaluate (IsNatural l) l'
  , And k' l' r
  ) ⇒
  Evaluate (NaturalGrouper k l) r

class TokenizeT :: Symbol → Tokens → Constraint
class TokenizeT text tokens | text → tokens

instance
  ( ChopT ws xs
  , GroupByT NaturalGrouper xs ys
  , FilterT (NotEquals (" " :> Nil')) ys zs
  , MapT (Compose ReadToken Join) zs zs'
  ) ⇒
  TokenizeT ws zs'

foreign import data Tokenize ∷ Symbol → TypeExpr Tokens

instance tokenizeEvaluate ∷ TokenizeT ws zs ⇒ Evaluate (Tokenize ws) zs
