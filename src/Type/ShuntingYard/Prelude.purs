-- | Some extra type class definitions not present
-- | in dependencies for handling type-level stuff.
module Type.ShuntingYard.Prelude where

import Prim.Symbol as Symbol
import Type.Data.List (type (:>), List', Nil')


foreign import data KindPair ∷ ∀ k l. k → l → Type


class SpanEq ∷ ∀ k. k → List' k → Type → Constraint
class SpanEq x xs r | xs → r

instance spanNil ∷ SpanEq e Nil' (KindPair Nil' Nil')

else instance spanFound
  ∷ ( SpanEq x xs' (KindPair ys zs) )
  ⇒ SpanEq x (x :> xs') (KindPair (x :> ys) zs)

else instance spanNotFound
  ∷ SpanEq e xs (KindPair Nil' xs)


class BreakEq ∷ ∀ k. k → List' k → Type → Constraint
class BreakEq x xs r | xs → r

instance breakNil ∷ BreakEq e Nil' (KindPair Nil' Nil')

else instance breakFound
  ∷ BreakEq e (e :> es) (KindPair Nil' (e :> es))

else instance breakNotFound
  ∷ ( BreakEq e xs' (KindPair ys zs) )
  ⇒ BreakEq e (x :> xs') (KindPair (x :> ys) zs)


class DropWhileEq ∷ ∀ k. k → List' k → List' k → Constraint
class DropWhileEq e xs ys | e xs → ys

instance dropWhileImpl ∷ SpanEq e xs (KindPair ys zs) ⇒ DropWhileEq e xs zs


class Chop ( symbol ∷ Symbol ) ( chars ∷ List' Symbol )
  | symbol → chars

instance chopBase ∷ Chop "" Nil'

else instance chopRec
  ∷ ( Symbol.Cons h t s
    , Chop t t'
    )
  ⇒ Chop s (h :> t')


class Join ( chars ∷ List' Symbol ) ( symbol ∷ Symbol )
  | chars → symbol

instance joinNil ∷ Join Nil' ""

else instance joinRec
  ∷ ( Join xs t
    , Symbol.Cons h t r
    )
  ⇒ Join (h :> xs) r


class Words ( sentence ∷ Symbol ) ( words ∷ List' Symbol )
  | sentence → words

instance wordsNil ∷ Words "" Nil'

else instance wordsRec
  ∷ ( Chop sentence chars
    , DropWhileEq " " chars clean
    , BreakEq " " clean (KindPair wordL restL)
    , Join wordL word
    , Join restL rest_
    , Words rest_ rest
    )
  ⇒ Words sentence (word :> rest)
