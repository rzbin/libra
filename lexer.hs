module Lexer where

import Types

-- Lexes an input file
-- String code -> (String file, Integer line, Integer char) -> [(Type typ, String val, (String file, Integer line, Integer char))]
lexer :: String -> Loc -> [Token]
lexer [] _ = []
lexer ('\n' : code) (file, line, char) = lexer code (file, line + 1, 1)
lexer ('~' : code) (file, line, char) = lexer (dropWhile (/= '\n') code) (file, line, 1)
lexer (' ' : code) (file, line, char) = lexer code (file, line, char + 1)
lexer ('\"' : code) loc@(file, line, char) = (StrType, word, loc) : lexer rest (file, line, char + wordLength + 2)
  where
    (word, rest, wordLength) = extractString code loc
lexer code loc@(file, line, char) = (typ, word, loc) : lexer rest (file, line, char + wordLength)
  where
    (word, rest, wordLength) = extractWord code
    typ = getType word loc

-- Extract a word from the front of a string untill a space is encountered,
-- return (String word, String rest, Integer length)
extractString :: String -> Loc -> (String, String, Integer)
extractString [] loc = libraError "Could fully extract string" (Just loc)
extractString ('\"' : rest) _ = ("", rest, 0)
extractString (char : rest) loc = (char : restWord, restRest, restLength + 1)
  where
    (restWord, restRest, restLength) = extractString rest loc

-- Extract a word from the front of a string untill a space is encountered,
-- return (String word, String rest, Integer length)
extractWord :: String -> (String, String, Integer)
extractWord [] = ("", "", 0)
extractWord rest | head rest `elem` " \n" = ("", rest, 0)
extractWord (char : rest) = (char : restWord, restRest, restLength + 1)
  where
    (restWord, restRest, restLength) = extractWord rest
