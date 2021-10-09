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
handleInput str fileName = putStr $ concat out
  where
    lexed = lexer str (fileName, 1, 1)
    out = trace (formatLexedInfo lexed) $ eval [] lexed []
