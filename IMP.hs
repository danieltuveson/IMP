module IMP where 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type ProgramState = Map String Number 
type Number = Double 

newtype Loc = Loc String 
  deriving(Eq, Show)
  
data Aexp 
  = Num Number 
  | LocAexp Loc
  | Add Aexp Aexp 
  | Sub Aexp Aexp 
  | Mult Aexp Aexp
  deriving(Eq, Show)

data Bexp 
  = T
  | F
  | Eq Aexp Aexp 
  | Leq Aexp Aexp
  | Not Bexp 
  | Or Bexp Bexp
  | And Bexp Bexp
  deriving(Eq, Show)

data Sexp 
  = Empty 
  | Cons Char Sexp 
  deriving(Eq, Show)

data Com 
  = Skip
  | Set Loc Aexp
  | Seq Com Com 
  | If Bexp Com Com 
  | While Bexp Com 
  deriving(Eq, Show)

newtype Return = Return Aexp
  deriving(Eq, Show)

evalA :: Aexp -> ProgramState -> Number 
evalA a s = 
  case a of 
    Num n    -> n 
    LocAexp (Loc l) -> s Map.! l
    Add n m  -> evalA n s + evalA m s
    Sub n m  -> evalA n s - evalA m s
    Mult n m -> evalA n s * evalA m s

evalB :: Bexp -> ProgramState -> Bool
evalB b s = 
  case b of 
    T         -> True 
    F         -> False 
    Eq a1 a2  -> evalA a1 s == evalA a2 s
    Leq a1 a2 -> evalA a1 s <= evalA a2 s
    Not b'    -> not $ evalB b' s
    Or b1 b2  -> evalB b1 s || evalB b2 s
    And b1 b2 -> evalB b1 s && evalB b2 s

evalS :: Sexp -> String 
evalS s = 
  case s of 
    Empty      -> ""
    Cons c str -> c : evalS str

evalC :: Com -> ProgramState -> ProgramState
evalC c s = 
  case c of 
    Skip          -> s 
    Set (Loc l) a -> Map.insert l (evalA a s) s
    Seq c1 c2     -> evalC c2 (evalC c1 s)
    If b c1 c2    -> if evalB b s then evalC c1 s else evalC c2 s
    While b c     
      | evalB b s -> evalC (Seq c $ While b c) s
      | otherwise -> s

evalR :: Return -> ProgramState -> Number
evalR (Return a) s = evalA a s


-- some utility functions
add x y  = Add (Num x) (Num y)
sub x y  = Sub (Num x) (Num y)
mult x y = Mult (Num x) (Num y)
loc      = LocAexp . Loc