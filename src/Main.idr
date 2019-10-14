import Data.String
import Control.Monad.State

eval : String -> State (List Integer) (Maybe Integer)
eval src = eval' $ words src
  where 
    eval' : List String -> State (List Integer) (Maybe Integer)
    eval' (src::srcs) = case (parsePositive src) of
      Just num => do
        modify (\s => the (List Integer) (num::s))
        eval' srcs
      Nothing => do
        stack <- get
        case (src, stack) of 
          ("+" , arg1::arg0::rest) => put (arg0 + arg1 :: rest)     *> eval' srcs
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
        
main : IO ()
main = do
  args <- getArgs
  case args of
    _::srcs => printLn ((\src => evalState (eval src) []) <$> srcs)
