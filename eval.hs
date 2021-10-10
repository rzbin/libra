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

-- eval Stack -> Code -> (Output stack, data Stack)
eval :: Stack -> [Token] -> (Stack, Stack)
-- int
eval stack (token@(IntType, val, loc) : code) = eval (token : stack) code
-- str
eval stack (token@(StrType, val, loc) : code) = eval (token : stack) code
-- bln
eval stack (token@(BlnType, val, loc) : code) = eval (token : stack) code
-- print
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "print", loc) : code) =
  if ttyp == IntType || ttyp == StrType || ttyp == BlnType
    then ((StrType, tval ++ "\n", tloc) : os, ds)
    else expectError top "print" [IntType, StrType, BlnType] loc
  where
    (os, ds) = eval stack code
-- put
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "put", loc) : code) =
  if ttyp == IntType || ttyp == StrType || ttyp == BlnType
    then ((StrType, tval, tloc) : os, ds)
    else expectError top "put" [IntType, StrType, BlnType] loc
  where
    (os, ds) = eval stack code
-- dup
eval (top : stack) ((OpType, "dup", loc) : code) = eval (top : top : stack) code
-- drop
eval (top : stack) ((OpType, "drop", loc) : code) = eval stack code
-- not
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "!", loc) : code) =
  if ttyp == BlnType
    then eval ((ttyp, toStr (not (toBln tval)), loc) : stack) code
    else expectError top "!" [BlnType] loc
-- +
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "+", loc) : code) =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt tval + toInt bval), loc) : stack) code
    else expect2Error (top, bot) "+" ([IntType], [IntType]) loc
-- -
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "-", loc) : code) =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval - toInt tval), loc) : stack) code
    else expect2Error (top, bot) "-" ([IntType], [IntType]) loc
-- multiply
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "*", loc) : code) =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval * toInt tval), loc) : stack) code
    else expect2Error (top, bot) "*" ([IntType], [IntType]) loc
-- /
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "/", loc) : code) =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval `div` toInt tval), loc) : stack) code
    else expect2Error (top, bot) "/" ([IntType], [IntType]) loc
-- 2dup
eval full@(top : bot : _) ((OpType, "2dup", loc) : code) = eval (top : bot : full) code
-- 2drop
eval full@(top : bot : _) ((OpType, "2drop", loc) : code) = eval full code
-- swap
eval (top : bot : stack) ((OpType, "swap", loc) : code) = eval (bot : top : stack) code
-- over
eval full@(top : bot : _) ((OpType, "over", loc) : code) = eval (bot : full) code
-- =
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "=", loc) : code)
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt tval == toInt bval), loc) : stack) code
  | ttyp == StrType && btyp == StrType = eval ((BlnType, toStr (tval == bval), loc) : stack) code
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval == toBln bval), loc) : stack) code
  | otherwise = expect2Error (top, bot) "=" ([IntType, StrType, BlnType], [IntType, StrType, BlnType]) loc
-- or
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "|", loc) : code)
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval || toBln bval), loc) : stack) code
  | otherwise = expect2Error (top, bot) "|" ([BlnType], [BlnType]) loc
-- and
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "&", loc) : code)
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval && toBln bval), loc) : stack) code
  | otherwise = expect2Error (top, bot) "&" ([BlnType], [BlnType]) loc
-- <
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "<", loc) : code)
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval < toInt tval), loc) : stack) code
  | otherwise = expect2Error (top, bot) "<" ([IntType], [IntType]) loc
-- >
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, ">", loc) : code)
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval > toInt tval), loc) : stack) code
  | otherwise = expect2Error (top, bot) ">" ([IntType], [IntType]) loc
-- <
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "<=", loc) : code)
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval <= toInt tval), loc) : stack) code
  | otherwise = expect2Error (top, bot) "<=" ([IntType], [IntType]) loc
-- >
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, ">=", loc) : code)
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval >= toInt tval), loc) : stack) code
  | otherwise = expect2Error (top, bot) ">=" ([IntType], [IntType]) loc
-- if true
eval ((BlnType, "True", _) : stack) (token@(HghType, "if", loc) : code) = eval stack code
-- if false
eval ((BlnType, "False", _) : stack) (token@(HghType, "if", loc) : code) = eval stack (snd $findEnd code 1)
-- while
eval stack (token@(HghType, "while", loc) : code) =
  -- trace ("while at " ++ formatLocation loc) $
  (out, bSTACK)
  where
    (conditionCode, afterCondition) = findRun code 0
    (condOutput, (ttyp, cond, _) : conditionStack) = eval stack conditionCode
    (bodyCode, afterBody) = findEnd afterCondition 1
    (bodyOutput, bodyStack) = eval conditionStack bodyCode
    (rETURN, bSTACK) = if cond == "True" then eval bodyStack (token : code) else eval conditionStack afterBody
    out = (if cond == "True" then bodyOutput else []) ++ condOutput ++ rETURN

-- end
eval stack (token@(LowType, "end", loc) : code) = eval stack code
-- rest
eval [] [] = ([], [])
eval stack ((_, s, loc) : _) = libraError ("Cant evaluate \"" ++ s ++ "\"\nStack: \n" ++ formatLexedInfo stack) (Just loc)
-- eval _ [] cs | not $ null cs = libraError "Non-empty call stack at the end of code" Nothing
-- eval stack [] [] = libraError ("Non-empty stack at the end of code" ++ "\nStack: \n" ++ formatLexedInfo stack) Nothing
eval stack [] = ([], stack)

-- eval _ _ _ = libraError "Something went wrong!" Nothing

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