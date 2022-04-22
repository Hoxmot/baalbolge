module Main
  where

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           Baalbolge.Abs      ()
import           Baalbolge.Lex      (Token, mkPosToken)
import           Baalbolge.Par      (myLexer, pExps)
import           Baalbolge.Print    (Print, printTree)
import           Baalbolge.Skel     ()

type Err        = Either String
type ParseFun a = [Token] -> Err a

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      mapM_ (putStrLn . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: baalbolge [arguments]"
    , "  --help          Display this help message."
    , "  (no arguments)  Interpret stdin verbosely."
    ]

main :: IO()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run pExps
    _          -> usage
