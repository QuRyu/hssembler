module Command (Command, 
                command,
                converter)
       where

import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Data.Foldable (foldl')

type Addr = String 
type Val  = String
type Src  = String
type Dst  = String 
type Op   = String


opEncod = [("Add", "000"), ("Sub", "001"), ("Shift_L", "010"), ("Shift_R", "011"), 
           ("Xor", "100"), ("And", "101"), ("Rotate_L", "110"), ("Rotate_R", "111")]
srcMEncod = [("ACC", "00"), ("LR", "01"), ("IR", "10"), ("Fill", "11")]
dstMEncod = [("ACC", "00"), ("LR", "01"), ("ACC_L", "10"), ("ACC_H", "11")]
srcAEncod = [("ACC", "00"), ("LR", "01"), ("IR", "10"), ("Fill", "11")]
dstAEncod = [("ACC", "0"), ("LR", "1")]
srcBEncod = [("ACC", "0"), ("LR", "1")]

fstList :: [(a, b)] -> [a]
fstList = map fst 

opTable   = fstList opEncod
srcMTable = fstList srcMEncod
dstMTable = fstList dstMEncod
srcATable = fstList srcAEncod
dstATable = fstList dstAEncod
srcBTable = fstList srcBEncod

-- type encoding for commands
data Command = Move   Src Dst Val        -- move instruction
             | Arith  Op  Src Dst Val   -- binary opeartor 
             | Jump   Addr                 -- unconditional jump
             | Branch Src Addr            -- conditional jump
                deriving (Eq, Show)
            
command :: [String] -> Either String Command 
command ["Move", src, dst, val] = let x =  elemEither src srcMTable >> 
                                           elemEither dst dstMTable >> 
                                           isMVal     val  
                                  in case x of 
                                       Right _  -> Right $ Move src dst val
                                       Left s   -> Left s
command ["Jump", addr]         = let x = isAddr addr 
                                 in case x of 
                                      Right _  -> Right $ Jump addr 
                                      Left s   -> Left s
command ["Branch", src, addr]  = let x = elemEither src srcBTable >>
                                         isAddr    addr
                                 in case x of 
                                      Right _ -> Right $ Branch src addr
                                      Left s  -> Left s 
command [op, src, dst, val]    = let x = elemEither op  opTable >>
                                         elemEither src srcATable >> 
                                         elemEither dst dstATable >>
                                         isAVal     val
                                 in case x of 
                                      Right _  -> Right $ Arith op src dst val
                                      Left s -> Left s
command s@(x:xs)               = Left $ "could not parse" ++ unwords s
command [""]                   = Left "No command given"
command _                      = Left "unable to parse"

                               

elemEither :: (Eq a, Show a, Foldable t) => a -> t a -> Either String a
elemEither x xs = if x `elem` xs 
                    then Right x 
                    else Left $ show x ++ " is not supported"

isMVal :: String -> Either String String 
isMVal s = if length s == 4 && isDigitString s
             then Right ""
             else Left $ s ++ " is not supported" 

isAVal :: String -> Either String String 
isAVal s = if length s == 2 && isDigitString s 
             then Right ""
             else Left $ s ++ "is not supported"

isAddr = isMVal

{-# INLINE isAddr #-}

isDigitString :: String -> Bool
isDigitString = foldl' step True 
    where 
      step False _ = False 
      step True  x = isDigit x

lookupS :: (Eq a) => a -> [(a, b)] -> b
lookupS x xs = fromJust $ lookup x xs 

converter :: Command -> String 
converter (Move src dst val) = "00" ++ lookupS dst dstMEncod ++ lookupS src srcMEncod ++ val
converter (Arith op src dst val) = "01" ++ lookupS op opEncod ++ lookupS src srcAEncod ++ lookupS dst dstAEncod ++ val
converter (Jump addr)            = "10" ++ "0000" ++ addr
converter (Branch src addr)      = "11" ++ lookupS src srcBEncod ++ "000" ++ addr
