import Data.String
import Control.Monad.State

eval : String -> Maybe Integer
eval src = eval' [] $ words src
  where 
    eval' : List Integer -> List String -> Maybe Integer
    eval' (arg0::arg1::stack) ("+"::src) = 
      eval' (arg1 + arg0 ::　stack) src
    eval' (arg0::arg1::stack) ("*"::src) = 
      eval' (arg1 * arg0　::　stack) src
    eval' (arg0::arg1::stack) ("-"::src) = 
      eval' (arg1 - arg0　::　stack) src
    eval' stack (numSrc::src) = do 
      num <- parseInteger numSrc
      eval' (num::stack) src
    eval' (num::stack) [] = pure num
    eval' [] [] = Nothing

-- loop : List String -> IO ()
-- loop (arg0::"+"::arg1::rest) = do
--   printLn $ (+) <$> parseInteger arg0 <*> parseInteger arg1
--   loop rest
-- loop a = printLn a

partial
main : IO ()
main = do
  args <- getArgs
  case args of
    _::srcs => printLn (eval <$> srcs)
    _ => putStrLn "no input"
