module Test where 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import IMP 

data Test a 
  = Test String a a 
  deriving(Eq)


main = 
  let tests   = map test testAexps ++ map test testBexps
      failing = map fst (filter (not . snd) tests)
      passing = map fst (filter snd tests)
  in  case failing of 
        [] -> putStrLn "All tests passing!"
        _  -> mapM_ putStrLn failing

-- Utility functions for testing:
test :: Test -> (String, Bool)
test t = 
  case t of 
    TestA s aexp n -> testAny s (evalA aexp) n 
    TestB s bexp b -> testAny s (evalB bexp) b
    TestC s com ps -> testAny s (evalC com) ps 
  where testAny testName actual expected = 
          let msg = "Test " ++ testName ++ ": " ++ 
                if actual == expected then 
                  "passing" 
                else "\nfailed: expected " ++ show expected ++ " but got " ++ show actual
          in (msg, actual == expected)

-- Program states used for testing
e = Map.empty
s1 = Map.singleton "x" (Num 11)

-- Tests for eval functions in IMP
testAexps = 
  [ TestA "add: 5 + 6 = 11"             (evalA (add 5 6) e) 11
  , TestA "subtraction: 5 - 6 = -1"     (evalA (sub 5 6) s1) (-1)
  , TestA "multiplication: 5 * 6 = 30"  (evalA (mult 5 6) e) 30
  , TestA "loc: x = 11; 2 * x - 1 = 21" (evalA (Sub (Mult (Num 2) (loc "x")) (Num 1)) s1) 21
  ]

testBexps = 
  [ TestB "equality: x = 11; x == (1 + 10)" (evalB (Eq (loc "x") (add 1 10)) s1) True
  -- , 
  ]


    -- T         -> True 
    -- F         -> False 
    -- Eq a1 a2  -> evalA a1 s == evalA a2 s
    -- Leq a1 a2 -> evalA a1 s <= evalA a2 s
    -- Not b'    -> not $ evalB b' s
    -- Or b1 b2  -> evalB b1 s || evalB b2 s
    -- And b1 b2 -> evalB b1 s && evalB b2 s