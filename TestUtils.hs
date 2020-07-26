module TestUtils where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import IMP

data Test a = Test 
  { label :: String
  , expected :: a
  , actual :: a
  } deriving(Eq)

runTests :: [Maybe String] -> IO ()
runTests tests = 
  let failing = foldr addFailure [] tests
  in  case failing of 
        [] -> putStrLn "All tests passing!"
        _  -> mapM_ putStrLn failing
  where addFailure x xs = 
          case x of 
            Nothing -> xs 
            Just x  -> (x:xs)

-- Utility functions for testing:
test :: (Show a, Eq a) => Test a -> Maybe String
test (Test testName actual expected)
  | actual == expected = Nothing 
  | otherwise = 
      Just $ "Test " ++ testName ++ ": " ++ 
        if actual == expected then 
          "passing" 
        else "\nfailed: expected " ++ show expected ++ " but got " ++ show actual


evalTests :: (Show a, Eq a) => [Test a] -> [Maybe String]
evalTests = map test 