import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)

import qualified Util               (checkParentheses, notImplemented,
                                     readCommand, showTree)

import           Baalbolge.Abs      ()
import           Baalbolge.Lex      (Token, mkPosToken)
import           Baalbolge.Par      (myLexer, pExps)
import           Baalbolge.Print    (Print)
import           Baalbolge.Skel     ()
import           Util               (Command (..))


type Err        = Either String
type ParseFun a = [Token] -> Err a

interactiveMode :: IO ()
interactiveMode = readStdIn "" 0 >> interactiveMode

readStdIn :: String -> Int -> IO()
readStdIn line c = do
    currentLine <- getLine
    case Util.readCommand currentLine of
      Exit  -> exitSuccess
      -- TODO: #17
      Help  -> Util.notImplemented
      Parse -> continueParse line currentLine c

continueParse :: String -> String -> Int -> IO()
continueParse line currentLine c =
    if nc == 0
        then run pExps allLines
        else readStdIn allLines nc
  where
    nc = Util.checkParentheses currentLine c
    allLines = line ++ '\n':currentLine

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
        Util.showTree tree
  where
    ts = myLexer s
    showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: baalbolge [arguments]"
        , "  --help          Display this help message."
        , "  (no arguments)  Interactive mode."
        ]

main :: IO()
main = do
    args <- getArgs
    case args of
      ["--help"] -> usage
      []         -> interactiveMode
      _          -> usage
