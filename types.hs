module Types where

data Type = IntType | StrType | BlnType | OpType | HghType | LowType | PtrType
  deriving (Eq, Show)

-- OpType
ops :: [String]
ops =
  [ "#",
    "+",
    "-",
    "*",
    "/",
    "%",
    "print",
    "put",
    "dup",
    "drop",
    "!",
    "2dup",
    "2drop",
    "swap",
    "over",
    "=",
    "|",
    "&",
    "<",
    ">",
    "<=",
    ">=",
    "run",
    "s",
    "r",
    "else"
  ]

-- HghType
highers :: [String]
highers = ["if", "while"]

-- LowType
lowers :: [String]
lowers = ["end"]

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
  | str `elem` ["True", "False"] = BlnType
  | str `elem` highers = HghType
  | str `elem` lowers = LowType
  | str `elem` ops = OpType
  | otherwise = libraError ("Can't determine type for \"" ++ str ++ "\"") (Just loc)
  where
    isNumerical :: String -> Bool
    isNumerical = foldr (\x -> (&&) (x `elem` "0123456789")) True

libraError :: String -> Maybe Loc -> a
libraError err Nothing = errorWithoutStackTrace ("\n\n♎️ Libra error:\n" ++ err ++ "\n")
libraError err (Just loc) = errorWithoutStackTrace ("\n\n♎️ Libra error:\n" ++ formatLocation loc ++ " " ++ err ++ "\n")