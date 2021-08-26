module Type.ShuntingYard where

import Prim.Ordering (EQ, GT)
import Prim.TypeError (class Fail, Text)
import Type.Data.Boolean (class And, class If, class Not, class Or)
import Type.Data.List (type (:>), Nil')
import Type.Data.Peano as Peano
import Type.Data.Peano.Nat (class CompareNat)
import Type.Proxy (Proxy(..))
import Type.ShuntingYard.Lexer (class TokenizeT)
import Type.ShuntingYard.Prelude (class TypeEqualsTF)
import Type.ShuntingYard.Types (class LeftAssocT, class OperatorPrecedenceT, type (\/), type (\/\/), LeftP, NaturalToken, OperatorK, OperatorToken, ParenK, ParenToken, Plus, RightP, Times, Tokens, Minus, Exponent, Divide)

foreign import data ParenthesisOperator ∷ ParenK → OperatorK

instance OperatorPrecedenceT (ParenthesisOperator p) Peano.D0

type YardState = Tokens \/\/ Tokens

class ParseAndShunt ∷ Symbol → Tokens → Constraint
class ParseAndShunt text tokens | text → tokens

instance
  ( TokenizeT text tokens
  , Shunt (Nil' \/ Nil') tokens shunted
  , Finish shunted (finished \/ _operator)
  , Reverse finished reversed
  ) ⇒
  ParseAndShunt text reversed

class Shunt ∷ YardState → Tokens → YardState → Constraint
class Shunt currentState currentTokens nextState | currentState currentTokens → nextState

instance Shunt currentState Nil' currentState

else instance Shunt (NaturalToken token :> output \/ operator) tokens result ⇒ Shunt (output \/ operator) (NaturalToken token :> tokens) result

else instance
  ( ClearOperatorStack currentState operator (output \/ operators)
  , Shunt (output \/ (OperatorToken operator) :> operators) tokens result
  ) ⇒
  Shunt currentState (OperatorToken operator :> tokens) result

else instance Shunt (output \/ OperatorToken (ParenthesisOperator LeftP) :> operator) tokens result ⇒ Shunt (output \/ operator) (ParenToken LeftP :> tokens) result

else instance
  ( ClearNonLeftP (output \/ operator) nextResult
  , Shunt nextResult tokens futureResult
  ) ⇒
  Shunt (output \/ operator) (ParenToken RightP :> tokens) futureResult

class Finish ∷ YardState → YardState → Constraint
class Finish start final | start → final

instance Finish (output \/ Nil') (output \/ Nil')

else instance Fail (Text "unterminated left parenthesis") ⇒ Finish (output \/ (OperatorToken (ParenthesisOperator LeftP)) :> operator) terminate

else instance Finish (top :> output \/ operator) result ⇒ Finish (output \/ top :> operator) result

class Reverse ∷ Tokens → Tokens → Constraint
class Reverse start final | start → final

instance ReverseImpl Nil' start final ⇒ Reverse start final

class ReverseImpl ∷ Tokens → Tokens → Tokens → Constraint
class ReverseImpl acc start final | acc start → final

instance ReverseImpl acc Nil' acc

else instance ReverseImpl (x :> acc) xs next ⇒ ReverseImpl acc (x :> xs) next

class ClearOperatorStack ∷ YardState → OperatorK → YardState → Constraint
class ClearOperatorStack currentState operator nextState | currentState operator → nextState

instance ClearOperatorStack (output \/ Nil') o1 (output \/ Nil')

else instance
  ( OperatorPrecedenceT o1 p1
  , OperatorPrecedenceT o2 p2

  , TypeEqualsTF o2 (ParenthesisOperator LeftP) isLeftParenthesis
  , Not isLeftParenthesis isNotLeftParenthesis

  , CompareNat p2 p1 pn
  , TypeEqualsTF pn GT hasGreaterPrecedence
  , TypeEqualsTF pn EQ hasEqualPrecedence
  , LeftAssocT o1 isLeftAssociative

  , And hasEqualPrecedence isLeftAssociative leftAssoc
  , Or hasGreaterPrecedence leftAssoc isPoppable

  , And isNotLeftParenthesis isPoppable popOperator

  , ClearOperatorStack (OperatorToken o2 :> output \/ operator) o1 clearSomeMore
  , If popOperator clearSomeMore (output \/ (OperatorToken o2) :> operator) result
  ) ⇒
  ClearOperatorStack (output \/ (OperatorToken o2) :> operator) o1 result

class ClearNonLeftP ∷ YardState → YardState → Constraint
class ClearNonLeftP start final | start → final

instance Fail (Text "unterminated parenthesis") ⇒ ClearNonLeftP (output \/ Nil') terminate

else instance ClearNonLeftP (output \/ (OperatorToken (ParenthesisOperator LeftP)) :> operator) (output \/ operator)

else instance ClearNonLeftP (top :> output \/ operator) result ⇒ ClearNonLeftP (output \/ top :> operator) result

type Z' =
  ( NaturalToken Peano.D3
      :> NaturalToken Peano.D4
      :> NaturalToken Peano.D2
      :> OperatorToken Times
      :> NaturalToken Peano.D1
      :> NaturalToken Peano.D5
      :> OperatorToken Minus
      :> NaturalToken Peano.D2
      :> NaturalToken Peano.D3
      :> OperatorToken Exponent
      :> OperatorToken Exponent
      :> OperatorToken Divide
      :> OperatorToken Plus
      :> Nil'
  )

z ∷ ∀ r. ParseAndShunt "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3" r ⇒ Proxy r
z = Proxy

z' ∷ Proxy Z'
z' = z
