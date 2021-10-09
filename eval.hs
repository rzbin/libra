module Eval where

import Types
import Util

--type Token = (Type, String, Loc)

--type Loc = (String, Integer, Integer)

-- Stack [(type, val, generated at)]
--type Stack = [Token]

-- Call stack [(Token that raised it, [Tokens of code])]

type CallStack = [(Token, [Token])]

-- eval Stack -> Code -> Outputs
eval :: Stack -> [Token] -> CallStack -> [String]
-- int
eval stack (token@(IntType, val, loc) : code) cs = eval (token : stack) code cs
-- str
eval stack (token@(StrType, val, loc) : code) cs = eval (token : stack) code cs
-- bln
eval stack (token@(BlnType, val, loc) : code) cs = eval (token : stack) code cs
-- print
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "print", loc) : code) cs =
  if ttyp == IntType || ttyp == StrType || ttyp == BlnType
    then (tval ++ "\n") : eval stack code cs
    else expectError top "print" [IntType, StrType, BlnType] loc
-- put
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "put", loc) : code) cs =
  if ttyp == IntType || ttyp == StrType || ttyp == BlnType
    then tval : eval stack code cs
    else expectError top "put" [IntType, StrType, BlnType] loc
-- dup
eval (top : stack) ((OpType, "dup", loc) : code) cs = eval (top : top : stack) code cs
-- not
eval (top@(ttyp, tval, tloc) : stack) ((OpType, "!", loc) : code) cs =
  if ttyp == BlnType
    then eval ((ttyp, toStr (not (toBln tval)), loc) : stack) code cs
    else expectError top "!" [BlnType] loc
-- +
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "+", loc) : code) cs =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt tval + toInt bval), loc) : stack) code cs
    else expect2Error (top, bot) "+" ([IntType], [IntType]) loc
-- -
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "-", loc) : code) cs =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval - toInt tval), loc) : stack) code cs
    else expect2Error (top, bot) "-" ([IntType], [IntType]) loc
-- multiply
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "*", loc) : code) cs =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval * toInt tval), loc) : stack) code cs
    else expect2Error (top, bot) "*" ([IntType], [IntType]) loc
-- /
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "/", loc) : code) cs =
  if ttyp == IntType && btyp == IntType
    then eval ((IntType, toStr (toInt bval `div` toInt tval), loc) : stack) code cs
    else expect2Error (top, bot) "/" ([IntType], [IntType]) loc
-- 2dup
eval full@(top : bot : _) ((OpType, "2dup", loc) : code) cs = eval (top : bot : full) code cs
-- swap
eval (top : bot : stack) ((OpType, "swap", loc) : code) cs = eval (bot : top : stack) code cs
-- over
eval full@(top : bot : _) ((OpType, "over", loc) : code) cs = eval (bot : full) code cs
-- =
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "=", loc) : code) cs
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt tval == toInt bval), loc) : stack) code cs
  | ttyp == StrType && btyp == StrType = eval ((BlnType, toStr (tval == bval), loc) : stack) code cs
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval == toBln bval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) "=" ([IntType, StrType, BlnType], [IntType, StrType, BlnType]) loc
-- or
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "|", loc) : code) cs
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval || toBln bval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) "|" ([BlnType], [BlnType]) loc
-- and
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "&", loc) : code) cs
  | ttyp == BlnType && btyp == BlnType = eval ((BlnType, toStr (toBln tval && toBln bval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) "&" ([BlnType], [BlnType]) loc
-- <
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "<", loc) : code) cs
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval < toInt tval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) "<" ([IntType], [IntType]) loc
-- >
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, ">", loc) : code) cs
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval > toInt tval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) ">" ([IntType], [IntType]) loc
-- <
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, "<=", loc) : code) cs
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval <= toInt tval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) "<=" ([IntType], [IntType]) loc
-- >
eval (top@(ttyp, tval, tloc) : bot@(btyp, bval, bloc) : stack) ((OpType, ">=", loc) : code) cs
  | ttyp == IntType && btyp == IntType = eval ((BlnType, toStr (toInt bval >= toInt tval), loc) : stack) code cs
  | otherwise = expect2Error (top, bot) ">=" ([IntType], [IntType]) loc
-- if true
eval ((BlnType, "True", _) : stack) (token@(HghType, "if", loc) : code) cs = eval stack code ((token, []) : cs)
-- if false
eval ((BlnType, "False", _) : stack) (token@(HghType, "if", loc) : code) cs = eval stack (findEnd code 1) cs
-- end
eval stack (token@(LowType, "end", loc) : code) cs = eval stack code (tail cs)
-- rest
eval [] [] [] = []
eval stack ((_, s, loc) : _) cs = libraError ("Cant evaluate \"" ++ s ++ "\"\nStack: \n" ++ formatLexedInfo stack) (Just loc)
eval _ [] cs | not $ null cs = libraError "Non-empty call stack at the end of code" Nothing
eval stack [] [] = libraError ("Non-empty stack at the end of code" ++ "\nStack: \n" ++ formatLexedInfo stack) Nothing
eval _ _ _ = libraError "Something went wrong!" Nothing

-- find end
findEnd :: [Token] -> Integer -> [Token]
findEnd code 0 = code
findEnd ((tokenType, _, _) : code) height
  | tokenType == HghType = findEnd code (height + 1)
  | tokenType == LowType = findEnd code (height -1)
  | otherwise = findEnd code height
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