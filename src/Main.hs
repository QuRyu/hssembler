module Main where

import Command 
import System.Environment
import Data.Foldable (foldl')


lineNumber :: [String] -> [(String, Int)]
lineNumber = flip zip [1..]

lexer :: [(String, Int)] -> Either String [Command]
lexer = reverseCommand . foldl' step (Right [])
    where 
        reverseCommand :: Either String [Command] -> Either String [Command]
        reverseCommand s = s >>= return . reverse
        interpret :: (String, Int) -> Either String Command 
        interpret (s, i) = command $ words s
        step :: Either String [Command] -> (String, Int) -> Either String [Command] 
        step commands c@(s, i) = commands >>= \xs -> case interpret c of 
                                                Right x -> Right (x:xs)
                                                Left  e -> Left $ "Parse error at line " ++ show i ++ ", because " ++ show e

parser :: Either String [Command] -> Either String [String]
parser (Left  a)        = Left a
parser (Right commands) = Right $ map converter commands

parse :: String -> Either String [String]
parse = parser . lexer . lineNumber . lines 

printer :: IO ()
printer  = do
    path <- getArgs
    flip mapM_ path (\filePath -> do 
                        content <- readFile filePath 
                        case parse content of 
                          Left  a        -> putStrLn ("failed to parse file " ++ filePath) >> putStrLn a
                          Right commands -> mapM_ putStrLn commands
                    )

main = printer 
