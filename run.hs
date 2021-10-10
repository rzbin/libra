import Debug.Trace (trace, traceShow)
import Eval
import Lexer
import Types
import Util

test = run "test.♎️"

printLexed = True

run :: FilePath -> IO ()
run fileName = readFile fileName >>= \s -> handleInput s fileName

runString :: String -> IO ()
runString str = handleInput str "♎️"

handleInput :: String -> String -> IO ()
-- handleInput str fileName = putStr $ formatLexedInfo lexed
handleInput str fileName = putStr $ returnStackToString outputStack
  where
    lexed = lexer str fileName
    (returnStack, leftStack) = if printLexed then trace (formatLexedInfo lexed) $ eval [] lexed else eval [] lexed
    outputStack =
      if null leftStack
        then returnStack
        else libraError ("Items left on the stack after evalutating code:\n" ++ formatLexedInfo leftStack) Nothing

returnStackToString :: [Token] -> String
returnStackToString [] = []
returnStackToString ((StrType, val, _) : xs) = val ++ returnStackToString xs
returnStackToString ((typ, val, loc) : xs) =
  libraError ("Invalid type returned from calulation: " ++ show typ ++ "\t" ++ val ++ "\tgenerated at " ++ formatLocation loc) Nothing
