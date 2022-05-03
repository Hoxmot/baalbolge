import           System.Environment (getArgs, withArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (hFlush, stdout)

import qualified Baalbolge.Skel     (Result)
import qualified Interpreter        (interpret)
import qualified TypeChecker        (checkTypes)
import qualified Util               (checkParentheses, exit, notImplemented,
                                     readCommand, showTree)

import           Baalbolge.Abs      (Exps)
import           Baalbolge.Lex      (Token, mkPosToken)
import           Baalbolge.Par      (myLexer, pExps)
import           Baalbolge.Print    (Print)
import           Types
import           Util               (Command (..), printResponse)


fileMode :: FilePath -> IO ()
fileMode f = putStrLn f >> readFile f >>= runFile

runFile :: String -> IO ()
runFile s =
    case run s of
      Left err -> do
        putStrLn err
        exitFailure
      Right res -> do
        Util.printResponse res
        Util.exit res

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
        then runInteractive allLines
        else readStdIn allLines nc
  where
    nc = Util.checkParentheses currentLine c
    allLines = line ++ '\n':currentLine

runInteractive :: String -> IO ()
runInteractive s =
    case run s of
      Left err -> do
        putStrLn err
      Right res -> do
        Util.printResponse res

run :: String -> Err Result
run s = do
    exps <- pExps $ myLexer s
    exps <- TypeChecker.checkTypes exps
    Interpreter.interpret exps

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: baalbolge [arguments]"
        , "  --help          Display this help message."
        , "  (no arguments)  Interactive mode."
        , "  FILE            Reads and interprets the code from file."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--help"] -> usage
      []         -> interactiveMode
      (h:_)      -> fileMode h
