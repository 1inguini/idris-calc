import Data.String
import Control.Monad.State
import Debug.Trace

%default partial
              
main : IO ()
main = do
  ("run"::srcs) <- getArgs
    | [_, "repl"] => repl
    | _           => putStrLn "wrong"
  printLn ((\src => evalState (eval src) []) <$> srcs)
  
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

eval : Monad m => String -> StateT (List Integer) m (Maybe Integer)
eval src = eval' $ words src
  where 
    eval' : Monad m => List String -> StateT (List Integer) m (Maybe Integer)
    eval' ("+"::srcs) = do
        (arg1::arg0::rest) <- get
          | _ => pure Nothing
        put (arg0 + arg1 :: rest) *> eval' srcs
    
    eval' (src::srcs) = case (parsePositive src) of
      Just num => do
        modify (\s => the (List Integer) (num::s))
        eval' srcs

      Nothing => do
        (arg1::arg0::rest) <- get
          | _ => pure Nothing
        case src of
          "-" => put (arg0 - arg1 :: rest)     *> eval' srcs
          "*" => put (arg0 * arg1 :: rest)     *> eval' srcs
          "/" => put (arg0 `div` arg1 :: rest) *> eval' srcs
          "%" => put (arg0 `mod` arg1 :: rest) *> eval' srcs
          _   => pure Nothing

    eval' [] = do 
      [num] <- get
        | _ => pure Nothing
      pure $ Just num
