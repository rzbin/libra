module Eval where

import Debug.Trace (trace, traceShow)
import Types
import Util

--type Token = (Type, String, Loc)

--type Loc = (String, Integer, Integer)

-- Stack [(type, val, generated at)]
--type Stack = [Token]

-- Call stack [(Token that raised it, [Tokens of code])]

type CallStack = [(Token, [Token])]

type Memory = [(Integer, Token)]

-- eval Stack -> Code -> (Output stack, data Stack)
eval :: Stack -> [Token] -> Memory -> (Stack, Stack)
-- int
eval stack (token@(IntType, val, loc) : code) mem = eval (token : stack) code mem
-- str
eval stack (token@(StrType, val, loc) : code) mem = eval (token : stack) code mem
-- bln
eval stack (token@(BlnType, val, loc) : code) mem = eval (token : stack) code mem
-- #
eval stack ((OpType, "#", loc) : code) mem = eval ((PtrType, "0", loc) : stack) code mem
-- print
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "print", loc) : code) mem =
  if ttyp == IntType || ttyp == StrType || ttyp == BlnType
    then ((StrType, tval ++ "\n", tloc) : os, ds)
    else expectError top "print" [IntType, StrType, BlnType] loc
  where
    (os, ds) = eval stack code mem
-- put
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "put", loc) : code) mem =
  if ttyp == IntType || ttyp == StrType || ttyp == BlnType
    then ((StrType, tval, tloc) : os, ds)
    else expectError top "put" [IntType, StrType, BlnType] loc
  where
    (os, ds) = eval stack code mem
-- dup
eval (top : stack) ((OpType, "dup", loc) : code) mem = eval (top : top : stack) code mem
-- drop
eval (top : stack) ((OpType, "drop", loc) : code) mem = eval stack code mem
-- not
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "!", loc) : code) mem =
  if ttyp == BlnType
    then eval ((ttyp, toStr (not (toBln tval)), loc) : stack) code mem
    else expectError top "!" [BlnType] loc
-- +
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "+", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((IntType, toStr (toInt tval + toInt bval), loc) : stack) code mem
  | ttyp == IntType && btyp == PtrType = eval ((PtrType, toStr (toInt tval + toInt bval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "+" ([IntType], [IntType, PtrType]) loc
-- -
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "-", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((IntType, toStr (toInt bval - toInt tval), loc) : stack) code mem
  | ttyp == IntType && btyp == PtrType = eval ((PtrType, toStr (toInt bval - toInt tval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "-" ([IntType], [IntType]) loc
-- multiply
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "*", loc) : code) mem =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval * toInt tval), loc) : stack) code mem
    else expect2Error (top, bot) "*" ([IntType], [IntType]) loc
-- /
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "/", loc) : code) mem =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval `div` toInt tval), loc) : stack) code mem
    else expect2Error (top, bot) "/" ([IntType], [IntType]) loc
-- %
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "%", loc) : code) mem =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval `mod` toInt tval), loc) : stack) code mem
    else expect2Error (top, bot) "%" ([IntType], [IntType]) loc
-- 2dup
eval full@(top : bot : _) ((OpType, "2dup", loc) : code) mem = eval (top : bot : full) code mem
-- 2drop
eval (top : bot : stack) ((OpType, "2drop", loc) : code) mem = eval stack code mem
-- swap
eval (top : bot : stack) ((OpType, "swap", loc) : code) mem = eval (bot : top : stack) code mem
-- over
eval full@(top : bot : _) ((OpType, "over", loc) : code) mem = eval (bot : full) code mem
-- =
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "=", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt tval == toInt bval), loc) : stack) code mem
  | ttyp == StrType && btyp == StrType = eval ((BlnType, toStr (tval == bval), loc) : stack) code mem
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval == toBln bval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "=" ([IntType, StrType, BlnType], [IntType, StrType, BlnType]) loc
-- or
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "|", loc) : code) mem
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval || toBln bval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "|" ([BlnType], [BlnType]) loc
-- and
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "&", loc) : code) mem
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval && toBln bval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "&" ([BlnType], [BlnType]) loc
-- <
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "<", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval < toInt tval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "<" ([IntType], [IntType]) loc
-- >
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, ">", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval > toInt tval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) ">" ([IntType], [IntType]) loc
-- <
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "<=", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval <= toInt tval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) "<=" ([IntType], [IntType]) loc
-- >
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, ">=", loc) : code) mem
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval >= toInt tval), loc) : stack) code mem
  | otherwise = expect2Error (top, bot) ">=" ([IntType], [IntType]) loc
-- s (store)
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "s", loc) : code) mem
  | btyp == PtrType =
    if toInt bval < 0
      then libraError ("Cant store to negative location: " ++ bval ++ "\n\tTrying to store:\n\t" ++ formatLexedInfo [top]) (Just bloc)
      else eval stack code (storeToMemory mem (toInt bval) top)
  | otherwise = expectError bot "s (bottom)" [PtrType] loc
-- r (read)
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "r", loc) : code) mem =
  if ttyp == PtrType
    then eval (token : stack) code mem
    else expectError top "r" [PtrType] loc
  where
    token = readFromMemory mem (toInt tval)
-- if true
eval ((BlnType, "True", _) : stack) (token@(HghType, "if", loc) : code) mem = eval stack code mem
-- if false
eval ((BlnType, "False", _) : stack) (token@(HghType, "if", loc) : code) mem = eval stack (snd $findEnd code 1) mem
-- while
eval stack (token@(HghType, "while", loc) : code) mem =
  -- trace ("while at " ++ formatLocation loc) $
  (out, bSTACK)
  where
    (conditionCode, afterCondition) = findRun code 0
    (condOutput, (ttyp, cond, _) : conditionStack) = eval stack conditionCode mem
    (bodyCode, afterBody) = findEnd afterCondition 1
    (bodyOutput, bodyStack) = eval conditionStack bodyCode mem
    (rETURN, bSTACK) = if cond == "True" then eval bodyStack (token : code) mem else eval conditionStack afterBody mem
    out = (if cond == "True" then bodyOutput else []) ++ condOutput ++ rETURN
-- end
eval stack (token@(LowType, "end", loc) : code) mem = eval stack code mem
-- rest
eval [] [] _ = ([], [])
eval stack ((_, s, loc) : _) _ = libraError ("Cant evaluate \"" ++ s ++ "\"\nStack: \n" ++ formatLexedInfo stack) (Just loc)
-- eval _ [] cs | not $ null cs = libraError "Non-empty call stack at the end of code" Nothing
-- eval stack [] [] = libraError ("Non-empty stack at the end of code" ++ "\nStack: \n" ++ formatLexedInfo stack) Nothing
eval stack [] _ = ([], stack)

-- eval _ _ _ = libraError "Something went wrong!" Nothing

readFromMemory :: Memory -> Integer -> Token
readFromMemory [] loc = libraError ("Cant read from memory location " ++ show loc) Nothing
readFromMemory ((memLoc, token) : xs) loc | memLoc == loc = token
readFromMemory (x : xs) loc = readFromMemory xs loc

--storeToMemory :: Memory -> Location -> Token -> Memory (Integer, Token)
storeToMemory :: Memory -> Integer -> Token -> Memory
storeToMemory [] location token = [(location, token)]
storeToMemory ((memLoc, _) : xs) location token | memLoc == location = (location, token) : xs
storeToMemory (x : xs) location token = x : storeToMemory xs location token

-- storeToMemory (x : xs) 0 token = Just token : xs
-- storeToMemory [] 0 token = [Just token]
-- storeToMemory (x : xs) location token = x : storeToMemory xs (location + (- 1)) token
-- -- storeToMemory _ location token = traceShow (storeToMemory [] (location + (- 1)) token) $ (Nothing : storeToMemory [] (location + (- 1)) token)
-- storeToMemory [] location token = replicate (fromIntegral location) Nothing ++ [Just token]

-- find end :: code -> height (0) -> (before, rest)
findRun :: [Token] -> Integer -> ([Token], [Token])
findRun ((OpType, "run", _) : code) 0 = ([], code)
findRun (token@(tokenType, _, _) : code) height
  | tokenType == HghType = let (before, rest) = findRun code (height + 1) in (token : before, rest)
  | tokenType == LowType = let (before, rest) = findRun code (height - 1) in (token : before, rest)
  | otherwise = let (before, rest) = findRun code height in (token : before, rest)
findRun [] _ = error "Cant find do"

-- find end :: code -> height (1) -> (before, rest)
findEnd :: [Token] -> Integer -> ([Token], [Token])
findEnd code 0 = ([], code)
findEnd (token@(tokenType, _, _) : code) height
  | tokenType == HghType = let (before, rest) = findEnd code (height + 1) in (token : before, rest)
  | tokenType == LowType = let (before, rest) = findEnd code (height - 1) in (token : before, rest)
  | otherwise = let (before, rest) = findEnd code height in (token : before, rest)
findEnd [] _ = error "Cant find end"

-- expect Tokens to test -> Operator -> Types they should be -> Location at which the test is happening -> error
expect2Error :: (Token, Token) -> String -> ([Type], [Type]) -> Loc -> a
expect2Error (top@(ttyp, tval, tloc), bot@(btyp, bval, bloc)) op (topTypes, botTypes) loc = libraError lines (Just loc)
  where
    lines = firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine
    firstLine = "\t\"" ++ op ++ "\" encountered type missmatch:"
    secondLine =
      "\t\tTop can have any of the types:\t\t" ++ show topTypes
        ++ "\n\t\tBut encountered:\t\t\t"
        ++ show ttyp
        ++ "\t \""
        ++ tval
        ++ "\" generated at "
        ++ formatLocation tloc
    thirdLine =
      "\t\tBottom can have any of the types:\t" ++ show botTypes
        ++ "\n\t\tBut encountered:\t\t\t"
        ++ show btyp
        ++ "\t \""
        ++ bval
        ++ "\" generated at "
        ++ formatLocation bloc

-- expect Token to test -> Operator -> Type it should be -> Location at which the test is happening -> error
expectError :: Token -> String -> [Type] -> Loc -> a
expectError (typ, val, loc) op (expType : expTypes) whereUsed = libraError str (Just whereUsed)
  where
    str =
      ( if null expTypes
          then
            "\t\"" ++ op ++ "\" expected type:\t" ++ show expType
              ++ "\n\t\tHowever it encountered:\t"
          else
            "\t\"" ++ op ++ "\" expected any of the types:\t" ++ show (expType : expTypes)
              ++ "\n\t\tHowever it encountered:\t\t\t"
      )
        ++ show typ
        ++ "\t \""
        ++ val
        ++ "\" generated at "
        ++ formatLocation loc
expectError _ _ _ loc = libraError "expectError called incorrectly (empty types)" (Just loc)

-- type Memory = [(Integer, Token)]
formatMemory :: Memory -> String
formatMemory [] = []
formatMemory ((loc, token) : xs) = "Memory location " ++ show loc ++ "\t" ++ formatLexedInfo [token] ++ "\n" ++ formatMemory xs
