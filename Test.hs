module Test where 

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import IMP 
import TestUtils
import IMPParser

main = runTests tests

tests :: [Maybe String]
tests = 
  evalTests testAexps ++ 
  evalTests testBexps ++ 
  evalTests testCommands ++
  evalTests testAexpsParsers ++
  evalTests testBexpsParsers ++
  evalTests testCommandParsers

-- Program states used for testing
e  = Map.empty
s1 = Map.singleton "x" 11
s2 = Map.singleton "x" 5
s3 = Map.insert "y" 8 s2
s4 = Map.singleton "y" 8

c1 = Set (Loc "y") (Num 8)
c2 = Set (Loc "x") (Num 5)
c3 = Set (Loc "x") (Sub (LocAexp (Loc "x")) (Num 6))
c4 = Set (Loc "x") (Sub (LocAexp (Loc "x")) (Num 1))

-- Tests for eval functions in IMP
testAexps = 
  [ Test "add: 5 + 6 = 11"             (evalA (add 5 6) e) 11
  , Test "subtraction: 5 - 6 = -1"     (evalA (sub 5 6) s1) (-1)
  , Test "multiplication: 5 * 6 = 30"  (evalA (mult 5 6) e) 30
  , Test "loc: x = 11; 2 * x - 1 = 21" (evalA (Sub (Mult (Num 2) (loc "x")) (Num 1)) s1) 21
  ]

testBexps = 
  [ Test "equality: x = 11; x == (1 + 10)" (evalB (Eq (loc "x") (add 1 10)) s1) True
  , Test "less than: x = 11; x <= 5" (evalB (Leq (loc "x") (Num 5)) s1) False
  , Test "not: Not T -> False" (evalB (Not T) s1) False
  , Test "or: T or F -> T" (evalB (Or T F) e) True 
  , Test "or: F or F -> F" (evalB (Or F F) e) False 
  , Test "and: T and F -> F" (evalB (And T F) e) False
  , Test "and: T and T -> T" (evalB (And T T) e) True
  ]

testCommands = 
  [ Test "skip: skip in program with some state" (evalC Skip s1) s1
  , Test "set: change the value of x from 11 to 5" (evalC (Set (Loc "x") (Num 5)) s1) s2
  , Test "set: x = 11; x = x - 6" (evalC c3 s1) s2
  , Test "sequence: " (evalC (Seq c1 c2) s1) s3
  , Test "if: test if with false arugment" (evalC (If F c1 c2) e) s2
  , Test "if: test if with true arugment" (evalC (If T c1 c2) e) s4
  , Test "while:" (evalC (While (Not (Eq (LocAexp (Loc "x")) (Num 5))) c4) s1) s2
  ]

-- test parsers
testAexpsParsers = 
  [ Test "parseNumber" (parse parseNumber "" "-5") (Right (Num (-5)))
  , Test "parseLocAexp" (parse parseLocAexp "" "x41") (Right (LocAexp (Loc "x41")))
  , Test "parseMathOp" (parse parseMathOp "" "x41 - 1 + 5") (Right (Add (Sub (LocAexp (Loc "x41")) (Num 1)) (Num 5)))
  , Test "parseAexp" (parse parseAexp "" "x41  -  1   +   5") (Right (Add (Sub (LocAexp (Loc "x41")) (Num 1)) (Num 5)))
  , Test "parseAexp" (parse parseAexp "" "5") (Right (Num 5))
  ]

testBexpsParsers = 
  [ Test "parseBoolean" (parse parseBoolean "" "True") (Right T)
  , Test "parseEq" (parse parseEq "" "5  ==  x41") (Right (Eq (Num 5) (LocAexp (Loc "x41"))))
  , Test "parseLeq" (parse parseLeq "" "5  <=  x41") (Right (Leq (Num 5) (LocAexp (Loc "x41"))))
  , Test "parseBoolOp" (parse parseBoolOp "" "True && False ||  False") (Right (Or (And T F) F))
  , Test "parseBexp" (parse parseBexp "" "True") (Right T)
  , Test "parseBexp" (parse parseBexp "" "True   &&  False  ||  False") (Right (Or (And T F) F))
  , Test "parseBexp" (parse parseBexp "" "hi == 5 &&  False ||  False") (Right (Or (And (Eq (LocAexp $ Loc "hi") (Num 5)) F) F))
  ]

testCommandParsers = 
  [ Test "parseSkip" (parse parseSkip "" "\n") (Right Skip)
  , Test "parseSet" (parse parseSet "" "hello  =  5 - 6 + 1") (Right (Set (Loc "hello") (Add (Sub (Num 5) (Num 6)) (Num 1))))
  , Test "parseIf" (parse parseIf "" "If hello  ==  5 || False Then\n hello = 4\nElse\n hello = 5\n End If") (Right (If (Or (Eq (LocAexp $ Loc "hello") (Num 5)) F) (Set (Loc "hello") (Num 4)) (Set (Loc "hello") (Num 5))))
  , Test "parseWhile" (parse parseWhile "" "While hello  ==  5 Do hello = hello + 2 End While") (Right (While (Eq (LocAexp $ Loc "hello") (Num 5)) (Set (Loc "hello") (Add (LocAexp $ Loc "hello") (Num 2)))))
  , Test "parseSeq" (parse parseSeq "" "x = 5\n\nWhile 1 <= x Do\n\tx = x - 1 End While") (Right (Seq (Set (Loc "x") (Num 5)) (While (Leq (Num 1) (LocAexp $ Loc "x")) (Set (Loc "x") (Sub (LocAexp $ Loc "x") (Num 1))))))
  ]