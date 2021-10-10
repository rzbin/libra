import Debug.Trace (trace)
import Eval
import Lexer
import Types
import Util

test = run "test.♎️"

run :: FilePath -> IO ()
run fileName = readFile fileName >>= \s -> handleInput s fileName

handleInput :: String -> String -> IO ()
-- handleInput str fileName = putStr $ formatLexedInfo lexed
handleInput str fileName = putStr $ returnStackToString $ fst out
  where
    lexed = lexer str (fileName, 1, 1)
    out = trace (formatLexedInfo lexed) $ eval [] lexed

returnStackToString :: [Token] -> String
returnStackToString [] = []
returnStackToString ((StrType, val, _) : xs) = val ++ returnStackToString xs
returnStackToString ((typ, val, loc) : xs) =
  libraError ("Invalid type returned from calulation: " ++ show typ ++ "\t" ++ val ++ "\tgenerated at " ++ formatLocation loc) Nothing
