module Type.ShuntingYard.Types where

import Type.Data.List (List')
import Type.Data.Peano as P
import Type.Data.Peano as Peano
import Type.ShuntingYard.Evaluate (class Evaluate, TypeExpr)


data TokenK

foreign import data OperatorToken :: OperatorK -> TokenK
foreign import data NaturalToken :: Peano.Nat -> TokenK
foreign import data ParenToken :: ParenK -> TokenK


type Tokens = List' TokenK


data OperatorK

foreign import data Plus :: OperatorK
foreign import data Minus :: OperatorK
foreign import data Times :: OperatorK
foreign import data Divide :: OperatorK


data ParenK

foreign import data LeftP :: ParenK
foreign import data RightP :: ParenK


class ReadTokenT (s :: Symbol) (t :: TokenK)
  | s -> t

instance readTokenTLP :: ReadTokenT "(" (ParenToken LeftP)
else instance readTokenTRP :: ReadTokenT ")" (ParenToken RightP)
else instance readTokenTPL :: ReadTokenT "+" (OperatorToken Plus)
else instance readTokenTMN :: ReadTokenT "-" (OperatorToken Minus)
else instance readTokenTTM :: ReadTokenT "*" (OperatorToken Times)
else instance readTokenTDV :: ReadTokenT "/" (OperatorToken Divide)
else instance readTokenTNT :: (Peano.ParseNat x n) => ReadTokenT x (NaturalToken n)

foreign import data ReadToken :: Symbol -> TypeExpr TokenK

instance readTokenEvaluate :: ReadTokenT s t => Evaluate (ReadToken s) t


class OperatorPrecedenceT (t :: OperatorK) (n :: Peano.Nat)
  | t -> n

instance operatorPrecedenceTPL :: OperatorPrecedenceT Plus P.D2
instance operatorPrecedenceTMN :: OperatorPrecedenceT Minus P.D2
instance operatorPrecedenceTTM :: OperatorPrecedenceT Times P.D3
instance operatorPrecedenceTDV :: OperatorPrecedenceT Divide P.D3
