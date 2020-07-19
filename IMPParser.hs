module IMPParser where 

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Data.Maybe(isJust)
import Data.Map.Strict (toList, empty)
import qualified Data.Map.Strict as Map

import IMP 

parser :: GenParser Char st Number
parser = 
  do  c <- parseCommands
      r <- parseReturn
      return $ evalR r (evalC c empty) 



-- Parses an infix operator, e.g. x + y, returns a tuple of the parsed arguments
parseInfixOp :: GenParser Char st a -> String -> GenParser Char st (a, a)
parseInfixOp p str = 
  do  x1 <- p 
      spaces 
      string str 
      spaces
      x2 <- p 
      return (x1, x2)


-- Parsers for Aexpressions: 
parseAexp :: GenParser Char st Aexp
parseAexp = spaces >> (parseNumber <|> parseOperator <|> parseLocAexp)

parseNumber :: GenParser Char st Aexp
parseNumber = -- many $ oneOf "0123456789"
  do  negative    <- optionMaybe $ oneOf "-"
      beforePoint <- many1 $ digit
      -- point       <- oneOf "."
      -- afterPoint  <- many $ digit
      -- let num = (if isJust negative then "-" else "") ++ beforePoint ++ '.' : afterPoint
      let num = (if isJust negative then "-" else "") ++ beforePoint
      return $ Num (read num :: Double) 

parseLocAexp :: GenParser Char st Aexp
parseLocAexp = many alphaNum >>= return . LocAexp . Loc

parseOperator :: GenParser Char st Aexp
parseOperator = 
  do  num      <- (parseNumber <|> parseLocAexp)
      spaces
      operator <- oneOf "+-*"
      spaces
      rest     <- (parseOperator <|> parseNumber <|> parseLocAexp)
      case operator of 
        '+' -> return $ Add num rest 
        '-' -> return $ Sub num rest 
        '*' -> return $ Mult num rest 


-- Parsers for Bexpressions: 
parseBexp :: GenParser Char st Bexp
parseBexp = spaces >> (parseEq <|> parseLeq <|> parseNot <|> parseAnd <|> parseOr)

parseBoolean :: GenParser Char st Bexp
parseBoolean = 
  do  bool <- string "True" <|> string "False"
      return $ if bool == "True" then T else F

parseEq :: GenParser Char st Bexp
parseEq = parseInfixOp parseAexp "==" >>= return . uncurry Eq

parseLeq :: GenParser Char st Bexp
parseLeq = parseInfixOp parseAexp "<=" >>= return . uncurry Leq

parseNot :: GenParser Char st Bexp
parseNot = 
  do  string "Not"
      p <- parseBexp
      return $ Not p 

parseAnd :: GenParser Char st Bexp
parseAnd = parseInfixOp parseBexp "And" >>= return . uncurry And

parseOr :: GenParser Char st Bexp
parseOr = parseInfixOp parseBexp "Or" >>= return . uncurry Or


-- Parsers for commands:
parseCommands :: GenParser Char st Com
parseCommands = endOfLine >> spaces >> (parseSeq <|> parseSet <|> parseIf <|> parseWhile <|> parseSkip)


parseSkip :: GenParser Char st Com
parseSkip = spaces >> return Skip

parseSet :: GenParser Char st Com
parseSet = 
  do  l    <- lower 
      rest <- optionMaybe $ many alphaNum
      spaces 
      string "="
      spaces
      x <- parseAexp
      return $
        case rest of 
          Nothing  -> Set (Loc [l]) x
          Just str -> Set (Loc (l : str)) x

parseSeq :: GenParser Char st Com
parseSeq = 
  do  x <- parseCommands 
      y <- parseCommands
      return $ Seq x y 

parseIf :: GenParser Char st Com
parseIf = 
  do  string "If"
      space
      b <- parseBexp
      space
      string "Then"
      c1 <- parseCommands
      string "Else"
      c2 <- parseCommands
      string "End If"
      return $ If b c1 c2

parseWhile :: GenParser Char st Com
parseWhile = 
  do  string "While"
      space
      b <- parseBexp
      space
      string "Do"
      c <- parseCommands
      string "End While"
      return $ While b c

-- Parse return 
parseReturn :: GenParser Char st Return
parseReturn = string "Return" >> space >> parseLocAexp >>= return . Return 

{-|

data Com 
  = Skip
  | Set Loc Aexp
  | Seq Com Com 
  | If Bexp Com Com 
  | While Bexp Com 

data Aexp 
  = Num Number 
  | LocAexp Loc
  | Add Aexp Aexp 
  | Sub Aexp Aexp 
  | Mult Aexp Aexp
  deriving(Show)

data Bexp 
  = T
  | F
  | Eq Aexp Aexp 
  | Leq Aexp Aexp
  | Not Bexp 
  | Or Bexp Bexp
  | And Bexp Bexp
  deriving(Show)

data Sexp 
  = Empty 
  | Cons Char Sexp 
  deriving(Show)

-}