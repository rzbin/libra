import Debug.Trace (trace, traceShow)
import Eval
import Lexer
import System.Environment
import System.IO
import Types
import Util

test = run "test.♎️"

main = do
  [filename] <- getArgs
  run filename

printLexed = False

run :: FilePath -> IO ()
run fileName = readFile fileName >>= \s -> handleInput s fileName

runString :: String -> IO ()
runString str = handleInput str "♎️"

handleInput :: String -> String -> IO ()
-- handleInput str fileName = putStr $ formatLexedInfo lexed
handleInput str fileName = putStr $ returnStackToString outputStack
  where
    lexed = lexer str fileName
    (returnStack, leftStack, _) = if printLexed then trace (formatLexedInfo lexed) evaluated else evaluated
    evaluated = eval [] lexed []
    outputStack =
      if null leftStack
        then returnStack
        else libraError ("Items left on the stack after evalutating code:\n" ++ formatLexedInfo leftStack) Nothing

returnStackToString :: [Token] -> String
returnStackToString [] = []
returnStackToString ((StrType, val, _) : xs) = val ++ returnStackToString xs
returnStackToString ((typ, val, loc) : xs) =
  libraError ("Invalid type returned from calulation: " ++ show typ ++ "\t" ++ val ++ "\tgenerated at " ++ formatLocation loc) Nothing
