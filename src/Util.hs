module Util
    ( -- * Data types
      Command (..)

      -- * Util functions
      , checkParentheses
      , exit
      , getExpPos
      , showTree
      , printResponse
      , readCommand

      -- * Not implemented handling
      , notImplemented
      , notImplementedError
  ) where

import           System.Exit       (ExitCode (ExitFailure), exitFailure,
                                    exitSuccess, exitWith)

import qualified Baalbolge.Abs     as BG
import           Baalbolge.Print   (Print, printTree)
import           Interpreter.Types


-- | A command for controlling the interpeter in the interactive mode.
data Command = Exit | Help | Parse
  deriving (Show)

{- | Takes a string and an integer and checkes the number of opening and closing
parentheses in the string. Returns the given integer increased by the number of opening
parentheses and decreased by the number of closing parentheses.
It doesn't check the correctness of the parentheses.

>>> checkParentheses "(" 0
1

>>> checkParentheses ")" 0
-1

>>> checkParentheses "()" 0
0

>>> checkParentheses "(())()()" 0
0

>>> checkParentheses ")(" 0
0
-}
checkParentheses :: String -> Int -> Int
checkParentheses ('(':t) c = checkParentheses t (c + 1)
checkParentheses (')':t) c = checkParentheses t (c - 1)
checkParentheses (_:t) c   = checkParentheses t c
checkParentheses [] c      = c

{- | Tries to match a string with one of the commands. If it can't, it assumes the command
is parsing.

>>> readCommand "\\q"
Exit

>>> readCommand "\\h"
Help

>>> readCommand "Lorem ipsum dolor sit amet duis."
Parse

>>> readCommand "a\\q"
Parse

>>> readCommand "b\\h"
Parse
-}
readCommand :: String -> Command
readCommand "\\q" = Exit
readCommand "\\h" = Help
readCommand _     = Parse

-- | Prints parsed syntax tree. Shamelessly stolen from parser generated by BNFC.
showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
    putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

printResponse :: Result -> IO ()
printResponse RUnit     = putStrLn ""
printResponse (RBool b) = print b
printResponse (RInt v)  = print v
printResponse RFunc {}  = putStrLn "I'm a teapot"
printResponse RBFunc {} = putStrLn "I'm a teapot"

exit :: Result -> IO ()
exit RUnit = exitSuccess
exit (RBool b)
    | b = exitSuccess
    | otherwise = exitFailure
exit (RInt v)
    | retVal == 0 = exitSuccess
    | otherwise = exitWith $ ExitFailure (fromIntegral retVal)
  where
    retVal = v `mod` 256
exit RFunc {} = exitWith $ ExitFailure 13
exit RBFunc {} = exitWith $ ExitFailure 13

notImplemented :: IO()
notImplemented = putStrLn "Not yet implemented..."

notImplementedError :: IO()
notImplementedError = notImplemented >> putStrLn "Quitting..." >> exitFailure

-- | Gets a position of the expression
getExpPos :: BG.Exp -> BG.BNFC'Position
getExpPos (BG.EInt pos _)      = pos
getExpPos (BG.EBool pos _)     = pos
getExpPos (BG.EFunc pos _ _)   = pos
getExpPos (BG.EInternal pos _) = pos
getExpPos (BG.EVar pos _)      = pos
getExpPos (BG.EUnit pos)       = pos
getExpPos (BG.EList pos _)     = pos
