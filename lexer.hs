module Lexer where

import Debug.Trace (trace, traceShow)
import Types

-- Lexes an input file
lexer :: String -> String -> [Token]
lexer str fileName = lexerHelper str (fileName, 1, 1) []

type Macro = (String, [Token])

-- String code -> (String file, Integer line, Integer char)-> [Macros] -> [(Type typ, String val, (String file, Integer line, Integer char))]
lexerHelper :: String -> Loc -> [Macro] -> [Token]
lexerHelper [] _ m = []
lexerHelper ('\n' : code) (file, line, char) macros = lexerHelper code (file, line + 1, 1) macros
lexerHelper ('~' : code) (file, line, char) macros = lexerHelper (dropWhile (/= '\n') code) (file, line, 1) macros
lexerHelper (' ' : code) (file, line, char) macros = lexerHelper code (file, line, char + 1) macros
lexerHelper ('\"' : code) loc@(file, line, char) macros = (StrType, word, loc) : lexerHelper rest (file, line, char + wordLength + 2) macros
  where
    (word, rest, wordLength) = extractString code loc
-- create macro
lexerHelper ('$' : code) loc@(file, line, char) macros =
  lexerHelper restStr (file, rLine, rCharMinus1 + 1) ((word, lexerHelper bodyStr (file, bLine, bCharMinus1 + 1) macros) : macros)
  where
    (newCode, newLoc@(_, newLine, newChar)) = extractSpaces code loc
    (word, rest, wordLength) = extractWord newCode
    (_, _ : bodyRest, bodyLoc@(_, bLine, bCharMinus1)) = moveUntil rest (== '[') (file, newLine, newChar + wordLength + 1)
    (bodyStr, _ : restStr, rLoc@(_, rLine, rCharMinus1)) = moveUntil bodyRest (== ']') (file, bLine, bCharMinus1 + 1)
-- use macro
lexerHelper code loc@(file, line, char) macros | isMacro = macro ++ lexerHelper rest (file, line, char + wordLength) macros
  where
    (isMacro, macro) = inMacros word macros
    (word, rest, wordLength) = extractWord code
-- word
lexerHelper code loc@(file, line, char) macros = (typ, word, loc) : lexerHelper rest (file, line, char + wordLength) macros
  where
    (word, rest, wordLength) = extractWord code
    typ = getType word loc

inMacros :: String -> [Macro] -> (Bool, [Token])
inMacros _ [] = (False, [])
inMacros str ((name, tokens) : xs)
  | inOthers = (inOthers, othersToken)
  | str == name = (True, tokens)
  | otherwise = (False, [])
  where
    (inOthers, othersToken) = inMacros str xs

-- Extract a word from the front of a string untill a space is encountered,
-- return (String word, String rest, Integer length)
moveUntil :: String -> (Char -> Bool) -> Loc -> (String, String, Loc)
moveUntil [] _ loc = libraError "Could fully extract until" (Just loc)
moveUntil (x : xs) f loc@(file, line, char)
  | f x = ([], x : xs, loc)
  | x == '\n' = (x : n1, n2, n3)
  | otherwise = (x : s1, s2, s3)
  where
    space = (file, line, char + 1)
    newLine = (file, line + 1, 1)
    (s1, s2, s3) = moveUntil xs f space
    (n1, n2, n3) = moveUntil xs f newLine

--extractSpaces :: String code -> (rest, loc of next char)
extractSpaces :: String -> Loc -> (String, Loc)
extractSpaces code oldLoc@(file, line, char)
  | head code == ' ' = extractSpaces rest space
  | head code == '\n' = extractSpaces rest newLine
  | otherwise = (code, oldLoc)
  where
    rest = tail code
    space = (file, line, char + 1)
    newLine = (file, line + 1, 1)

-- Extract a word from the front of a string untill a space is encountered,
-- return (String word, String rest, Integer length)
extractString :: String -> Loc -> (String, String, Integer)
extractString [] loc = libraError "Could fully extract string" (Just loc)
extractString ('\"' : rest) _ = ("", rest, 0)
extractString (char : rest) loc = (char : restWord, restRest, restLength + 1)
  where
    (restWord, restRest, restLength) = extractString rest loc

-- return (String string, String rest, Integer length)
extractWord :: String -> (String, String, Integer)
extractWord [] = ("", "", 0)
extractWord rest | head rest `elem` " \n" = ("", rest, 0)
extractWord (char : rest) = (char : restWord, restRest, restLength + 1)
  where
    (restWord, restRest, restLength) = extractWord rest
