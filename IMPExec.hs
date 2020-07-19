module IMPExec where 

import Safe(headNote)
import Text.Parsec
import Text.Parsec.String(parseFromFile)
import System.Environment(getArgs)

import IMP
import IMPParser(parser)

main :: IO ()
main = 
  do  args <- getArgs
      let file = headNote "Error: please specify filepath of file" args
      parsedFile <- parseFromFile parser file 
      case parsedFile of
        Left e  -> putStrLn $ "Error parsing input: " ++ show e
        Right n -> print n