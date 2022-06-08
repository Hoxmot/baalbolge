import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (hFlush, stdout)

import qualified Interpreter          (interpret)
import qualified TypeChecker          (checkTypes)
import qualified Util                 (checkParentheses, exit, notImplemented,
                                       readCommand)

import           Baalbolge.Abs        (Exps)
import           Baalbolge.Par        (myLexer, pExps)
import           Interpreter.Types    (Result)
import           Types                (Err)
import           Util                 (Command (..), printResponse)


fileMode :: FilePath -> IO ()
fileMode f = putStrLn f >> readFile f >>= runFile

runFile :: String -> IO ()
runFile s = do
    progRes <- runExceptT $ run s
    case progRes of
        Left err  -> putStrLn err >> exitFailure
        Right res -> Util.printResponse res >> Util.exit res

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
runInteractive s = do
    progRes <- runExceptT $ run s
    case progRes of
        Left err  -> putStrLn err
        Right res -> Util.printResponse res

run :: String -> Err Result
run s = do
    e1 <- runLexer s
    e2 <- TypeChecker.checkTypes e1
    Interpreter.interpret e2

runLexer :: String -> Err Exps
runLexer s =
    case pExps $ myLexer s of
        Left l  -> throwError l
        Right r -> return r


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
