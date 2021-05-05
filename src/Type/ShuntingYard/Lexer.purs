module Type.ShuntingYard.Lexer where

import Type.Data.Boolean (class If)
import Type.Data.List (class IsMember, type (:>), List', Nil')
import Type.Data.Peano as Peano
-- import Type.Proxy (Proxy(..))
-- import Type.ShuntingYard.Prelude (class Chop, class Join, KindPair)


data Assoc

foreign import data LeftAssoc ∷ Assoc
foreign import data RightAssoc ∷ Assoc


data Token

foreign import data OperatorToken ∷ Symbol → Peano.Nat → Assoc → Token
foreign import data NumberToken ∷ Symbol → Token
foreign import data LeftParenToken ∷ Token
foreign import data RightParenToken ∷ Token


class Tokenize ( symbol ∷ List' Symbol ) ( tokens ∷ List' Token )
  | symbol → tokens

instance tokenizeEnd ∷ Tokenize Nil' n

else instance tokenizeLeftParen
  ∷ Tokenize n m
  ⇒ Tokenize ( "(" :> n ) ( LeftParenToken :> m )

else instance tokenizeRightParen
  ∷ Tokenize n m
  ⇒ Tokenize ( ")" :> n ) ( RightParenToken :> m )

else instance tokenizeOperator
  ∷ Tokenize n m
  ⇒ Tokenize ( "+" :> n ) ( OperatorToken "+" Peano.D1 LeftAssoc :> m )

else instance tokenizeSpace
  :: Tokenize n m
  => Tokenize ( " " :> n ) m

-- | WILL LEAD TO INFINITE LOOPS.
else instance tokenizeNatural
  ∷ ( SpanIsNatural v (KindPair vl vr)
    , Join vl vl'
    , Tokenize vr m
    )
  ⇒ Tokenize v ( NumberToken vl' :> m )


-- f ∷ ∀ r r'. Chop "(00 + 00 + 10 + 10)" r ⇒ Tokenize r r' ⇒ Proxy r'
-- f = Proxy

-- g ∷ Proxy _
-- g = f


type Naturals =
  "0" :> "1" :> "2" :> "3" :> "4" :>
  "5" :> "6" :> "7" :> "8" :> "9" :> Nil'

class SpanIsNatural ( c ∷ List' Symbol ) ( r ∷ Type )
  | c → r

instance spanIsNaturalNil ∷ SpanIsNatural Nil' (KindPair Nil' Nil')

else instance spanIsNaturalRec
  ∷ ( IsMember x Naturals r
    , SpanIsNatural xs' (KindPair ys zs)
    , If r (KindPair (x :> ys) zs) (KindPair Nil' (x :> xs')) z
    )
  ⇒ SpanIsNatural (x :> xs') z
