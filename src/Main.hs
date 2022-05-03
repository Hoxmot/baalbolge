import           System.Environment (getArgs, withArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hFlush, stdout)

import qualified Baalbolge.Skel     (Result)
import qualified TypeChecker        (checkTypes)
import qualified Util               (checkParentheses, notImplemented,
                                     readCommand, showTree)

import           Baalbolge.Abs      (Exps)
import           Baalbolge.Lex      (Token, mkPosToken)
import           Baalbolge.Par      (myLexer, pExps)
import           Baalbolge.Print    (Print)
import           Types
import           Util               (Command (..))


type ParseFun a = [Token] -> Err a
type Result     = Exps

fileMode :: [String] -> IO ()
fileMode = mapM_ readFileIn

readFileIn :: FilePath -> IO ()
readFileIn f = putStrLn f >> readFile f >>= run

interactiveMode :: IO ()
interactiveMode = readStdIn "" 0 >> interactiveMode

readStdIn :: String -> Int -> IO ()
readStdIn line c = do
    putStr ">> "
    hFlush stdout
    currentLine <- getLine
    case Util.readCommand currentLine of
      Exit  -> exitSuccess
      -- TODO: #17
      Help  -> Util.notImplemented
      Parse -> continueParse line currentLine c

continueParse :: String -> String -> Int -> IO ()
continueParse line currentLine c =
    if nc == 0
        then run allLines
        else readStdIn allLines nc
  where
    nc = Util.checkParentheses currentLine c
    allLines = line ++ '\n':currentLine

run :: String -> IO ()
run s =
    case interpret s of
      Left err -> do
        putStrLn err
        exitFailure
      Right tree -> do
        Util.showTree tree
  where
    showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

interpret :: String -> Err Result
interpret s = do
    exps <- pExps tokens
    TypeChecker.checkTypes exps
  where
    tokens = myLexer s

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: baalbolge [arguments]"
        , "  --help          Display this help message."
        , "  (no arguments)  Interactive mode."
        , "  [files]         Reads and interprets the code from files."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--help"] -> usage
      []         -> interactiveMode
      fs         -> fileMode fs
