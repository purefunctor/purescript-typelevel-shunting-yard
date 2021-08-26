module Type.ShuntingYard.Types where

import Prim.Boolean (False, True)
import Type.Data.List (List')
import Type.Data.Peano as P
import Type.Data.Peano as Peano
import Type.ShuntingYard.Evaluate (class Evaluate, TypeExpr)

data TokenK

foreign import data OperatorToken ∷ OperatorK → TokenK
foreign import data NaturalToken ∷ Peano.Nat → TokenK
foreign import data ParenToken ∷ ParenK → TokenK

type Tokens = List' TokenK

data OperatorK

foreign import data Plus ∷ OperatorK
foreign import data Minus ∷ OperatorK
foreign import data Times ∷ OperatorK
foreign import data Divide ∷ OperatorK
foreign import data Exponent ∷ OperatorK

data ParenK

foreign import data LeftP ∷ ParenK
foreign import data RightP ∷ ParenK

class
  ReadTokenT (s ∷ Symbol) (t ∷ TokenK)
  | s → t

instance readTokenTLP ∷ ReadTokenT "(" (ParenToken LeftP)
else instance readTokenTRP ∷ ReadTokenT ")" (ParenToken RightP)
else instance readTokenTPL ∷ ReadTokenT "+" (OperatorToken Plus)
else instance readTokenTMN ∷ ReadTokenT "-" (OperatorToken Minus)
else instance readTokenTTM ∷ ReadTokenT "*" (OperatorToken Times)
else instance readTokenTDV ∷ ReadTokenT "/" (OperatorToken Divide)
else instance readTokenTEX ∷ ReadTokenT "^" (OperatorToken Exponent)
else instance readTokenTNT ∷ (Peano.ParseNat x n) ⇒ ReadTokenT x (NaturalToken n)

foreign import data ReadToken ∷ Symbol → TypeExpr TokenK

instance readTokenEvaluate ∷ ReadTokenT s t ⇒ Evaluate (ReadToken s) t

class
  OperatorPrecedenceT (t ∷ OperatorK) (n ∷ Peano.Nat)
  | t → n

instance operatorPrecedenceTPL ∷ OperatorPrecedenceT Plus P.D2
instance operatorPrecedenceTMN ∷ OperatorPrecedenceT Minus P.D2
instance operatorPrecedenceTTM ∷ OperatorPrecedenceT Times P.D3
instance operatorPrecedenceTDV ∷ OperatorPrecedenceT Divide P.D3
instance OperatorPrecedenceT Exponent P.D4

class LeftAssocT (t ∷ OperatorK) (b ∷ Boolean) | t → b

instance LeftAssocT Plus True
instance LeftAssocT Minus True
instance LeftAssocT Times True
instance LeftAssocT Divide True
instance LeftAssocT Exponent False

data PairK ∷ ∀ k_ l_. k_ → l_ → Type
data PairK k l

foreign import data Pair ∷ ∀ k l. k → l → PairK k l

infixr 0 type Pair as \/
infixr 0 type PairK as \/\/

data MaybeK ∷ ∀ k_. k_ → Type
data MaybeK k

foreign import data JustK ∷ ∀ k. k → MaybeK k
foreign import data NothingK ∷ ∀ k. MaybeK k
