module IMPParser where 

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Data.Maybe(isJust, fromMaybe)
import Data.Map.Strict (toList, empty)
import qualified Data.Map.Strict as Map

import IMP 

type BinaryOpParser inp out = Parser (inp -> inp -> out)

parser :: Parser Number
parser = 
  do  spaces 
      c <- parseCommands
      spaces 
      r <- parseReturn
      return $ evalR r (evalC c empty) 


parseBinaryOp :: Parser a -> Parser a -> BinaryOpParser a b -> Parser b 
parseBinaryOp parseLeft parseRight binaryOpP = 
  do  l <- parseLeft
      spaces
      binaryOp <- binaryOpP
      spaces
      r <- parseRight
      return $ binaryOp l r

-- Parses an infix operator that may be applied recursively, 
-- e.g. x + y - z * w ... 
--      x And y Or z And w ...
parseRecursiveBinaryOp :: Maybe a -> Parser a -> BinaryOpParser a a -> Parser a
parseRecursiveBinaryOp maybeVal p binaryOpP = 
  do  let val = fromMaybe p (return <$> maybeVal)
      combined <- optionMaybe (try $ parseBinaryOp val p binaryOpP)
      case combined of 
        Nothing -> val 
        Just c  -> 
          do 
            result <- optionMaybe (parseRecursiveBinaryOp (Just c) p binaryOpP)
            case result of 
              Nothing -> return c
              Just r  -> return r

-- Takes a string and returns a parser that parses that string 
-- and returns the input value (or function) supplied
stringToParser :: String -> a -> Parser a
stringToParser str inp = string str >> return inp

-- Parsers for Aexpressions: 
parseAexp :: Parser Aexp
parseAexp = parseMathOp -- <|> parseNumber <|> parseLocAexp

parseNumber :: Parser Aexp
parseNumber = -- many $ oneOf "0123456789"
  do  negative    <- optionMaybe $ oneOf "-"
      beforePoint <- many1 $ digit
      -- point       <- oneOf "."
      -- afterPoint  <- many $ digit
      -- let num = (if isJust negative then "-" else "") ++ beforePoint ++ '.' : afterPoint
      let num = (if isJust negative then "-" else "") ++ beforePoint
      return $ Num (read num :: Double) 

parseLocAexp :: Parser Aexp
parseLocAexp = many alphaNum >>= return . LocAexp . Loc

parseMathOp :: Parser Aexp
parseMathOp = try $ parseRecursiveBinaryOp Nothing infixOp parser
  where parser  = stringToParser "+" Add <|> stringToParser "-" Sub <|> stringToParser "*" Mult
        infixOp = parseNumber <|> parseLocAexp


-- Parsers for Bexpressions: 
parseBexp :: Parser Bexp
parseBexp = parseBoolOp -- <|> parseEq <|> parseLeq <|> parseBoolean <?> "failed parsing boolean"-- <|> parseNot 
  
parseBoolean :: Parser Bexp
parseBoolean = try $ stringToParser "True" T <|> stringToParser "False" F

parseEq :: Parser Bexp
parseEq = try $ parseBinaryOp parseAexp parseAexp (string "==" >> return Eq)

parseLeq :: Parser Bexp
parseLeq = try $ parseBinaryOp parseAexp parseAexp (string "<=" >> return Leq)

parseBoolOp :: Parser Bexp
parseBoolOp = try $ parseRecursiveBinaryOp Nothing parser infixOp
  where parser  = parseEq <|> parseLeq <|> parseBoolean
        infixOp = stringToParser "&&" And <|> stringToParser "||" Or


-- Parsers for commands:
parseCommands :: Parser Com
parseCommands = parseSeq


parseSkip :: Parser Com
parseSkip = try $ endOfLine >> spaces >> return Skip

parseSet :: Parser Com
parseSet = try $
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

parseSeq :: Parser Com
parseSeq = try $ parseRecursiveBinaryOp Nothing parser infixOp
  where parser = parseSet <|> parseIf <|> parseWhile
        infixOp = return Seq

parseIf :: Parser Com
parseIf = try $ 
  do  string "If"
      many1 space
      b <- parseBexp
      many1 space
      string "Then"
      many1 space
      c1 <- parseCommands
      many1 space
      string "Else"
      many1 space
      c2 <- parseCommands
      many1 space
      string "End If"
      return $ If b c1 c2

parseWhile :: Parser Com
parseWhile = try $
  do  string "While"
      many1 space
      b <- parseBexp
      many1 space
      string "Do"
      many1 space
      c <- parseCommands
      many1 space
      string "End While"
      return $ While b c

-- Parse return 
parseReturn :: Parser Return
parseReturn = 
  do  string "Return" 
      many1 space
      toReturn <- parseLocAexp 
      spaces
      eof
      return (Return toReturn)