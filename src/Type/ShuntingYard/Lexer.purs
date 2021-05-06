module Type.ShuntingYard.Lexer where

import Type.Data.Boolean (class And)
import Type.Data.List (type (:>), List', Nil')
import Type.Data.Peano as Peano
import Type.Proxy (Proxy(..))
import Type.ShuntingYard.Evaluate (class Evaluate, Compose, TypeExpr, evaluate)
import Type.ShuntingYard.Prelude (class ChopT, class FilterT, class GroupByT, class MapT, IsNatural, Join, NotEquals)


data TokenK

foreign import data Operator :: Symbol -> TokenK
foreign import data Natural :: Peano.Nat -> TokenK


class ReadTokenT ( s :: Symbol ) ( t :: TokenK ) | s -> t

instance readTokenTLP :: ReadTokenT "(" (Operator "(")
else instance readTokenTRP :: ReadTokenT ")" (Operator ")")
else instance readTokenTPlus :: ReadTokenT "+" (Operator "+")
else instance readTokenTMinus :: ReadTokenT "-" (Operator "-")
else instance readTokenTTimes :: ReadTokenT "*" (Operator "*")
else instance readTokenTDivide :: ReadTokenT "/" (Operator "/")
else instance readTokenTNatural :: (Peano.ParseNat x n) => ReadTokenT x (Natural n)

foreign import data ReadToken :: Symbol -> TypeExpr TokenK

instance readTokenEvaluate :: ReadTokenT t r => Evaluate (ReadToken t) r


foreign import data GroupingPredicate :: Symbol -> Symbol -> TypeExpr Boolean

instance groupingPredicateEvaluate
  :: ( Evaluate (IsNatural k) k'
     , Evaluate (IsNatural l) l'
     , And k' l' r
     )
  => Evaluate (GroupingPredicate k l) r


foreign import data Tokenize
  :: Symbol
  -> TypeExpr (List' TokenK)

instance tokenizeEvaluate
  :: ( ChopT ws xs
     , GroupByT GroupingPredicate xs ys
     , FilterT (NotEquals (" " :> Nil')) ys zs
     , MapT (Compose ReadToken Join) zs zs'
     )
  => Evaluate (Tokenize ws) zs'


f :: Proxy
  ( Operator "(" :>
    Natural Peano.D50 :>
    Operator "+" :>
    Natural Peano.D50 :>
    Operator ")" :>
    Nil'
  )
f = evaluate (Proxy :: Proxy (Tokenize "(50 + 50)"))
