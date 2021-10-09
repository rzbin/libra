module Types where

data Type = IntType | StrType | OpType | BlnType
  deriving (Eq, Show)

ops :: [String]
ops = ["+", "-", "*", "/", "print", "put", "dup", "!", "2dup", "swap", "over", "=", "|", "&"]

type Token = (Type, String, Loc)

type Loc = (String, Integer, Integer)

-- Stack [(type, val, generated at)]
type Stack = [Token]

formatLocation :: Loc -> String
formatLocation (file, line, char) = file ++ ":" ++ show line ++ ":" ++ show char

formatLexedInfo :: [Token] -> String
formatLexedInfo [] = []
formatLexedInfo ((typ, val, loc) : rest) = info ++ formatLexedInfo rest
  where
    info = formatLocation loc ++ "\t" ++ show typ ++ "\t\t" ++ val ++ if null rest then "" else "\n"

getType :: String -> Loc -> Type
getType str loc
  | isNumerical str = IntType
  | str `elem` ops = OpType
  | str `elem` ["True", "False"] = BlnType
  | otherwise = libraError ("Can't determine type for \"" ++ str ++ "\"") (Just loc)
  where
    isNumerical :: String -> Bool
    isNumerical = foldr (\x -> (&&) (x `elem` "0123456789")) True

libraError :: String -> Maybe Loc -> a
libraError err Nothing = errorWithoutStackTrace ("\nERROR:\n" ++ err)
libraError err (Just loc) = errorWithoutStackTrace ("\nERROR:\n" ++ formatLocation loc ++ " " ++ err)