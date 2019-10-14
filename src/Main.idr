import Data.String
import Control.Monad.State
import Debug.Trace

%default partial

eval : Monad m => String -> StateT (List Integer) m (Maybe Integer)
eval src = eval' $ words src
  where 
    eval' : Monad m => List String -> StateT (List Integer) m (Maybe Integer)
    eval' ("+"::srcs) = do
        stack <- get
        case stack of 
          arg1::arg0::rest => put (arg0 + arg1 :: rest) *> eval' srcs
          _                => pure Nothing
    
    eval' (src::srcs) = case (parsePositive src) of
      Just num => do
        modify (\s => the (List Integer) (num::s))
        eval' srcs
      Nothing => do
        stack <- get
        case (src, stack) of
          ("-" , arg1::arg0::rest) => put (arg0 - arg1 :: rest)     *> eval' srcs
          ("*" , arg1::arg0::rest) => put (arg0 * arg1 :: rest)     *> eval' srcs
          ("/" , arg1::arg0::rest) => put (arg0 `div` arg1 :: rest) *> eval' srcs
          ("%" , arg1::arg0::rest) => put (arg0 `mod` arg1 :: rest) *> eval' srcs
          _                        => pure Nothing
    eval' [] = do 
      stack <- get
      case stack of
        [num] => pure $ Just num
        _     => pure Nothing

repl : IO ()
repl = runStateT loop [] *> pure ()
  where
    loop : StateT (List Integer) IO ()
    loop = do
      line  <- lift (putStr "repl > " *> getLine)
      eval line
      stack <- get
      lift $ printLn stack
      loop
              
main : IO ()
main = do
  (_::args) <- getArgs
  case args of
    ["repl"] => repl
    "run"::srcs => printLn ((\src => evalState (eval src) []) <$> srcs)
    _ => putStrLn "wrong"
