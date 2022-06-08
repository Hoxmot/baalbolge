module Interpreter.Types
    ( -- * Data types
      Arg (..)
      , BuiltInFunction
      , InterpreterMemory
      , InterpreterMemoryState
      , InterpreterReader
      , InterpreterState
      , Name

      -- * Data structures
      , Result (..)
      , MemoryObj (..)
  ) where


import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (StateT)

import qualified Baalbolge.Abs        as BG


{- | The representation of types used in Baalbolge for the purpose of computing the result
of computation.
-}
data Result = RUnit
    | RInt Integer
    | RBool Bool
    | RFunc BG.Type [Arg] [BG.Exp] InterpreterMemory
    | RBFunc BuiltInFunction

instance Show Result
  where
    show RUnit     = "unit"
    show (RInt i)  = "int " ++ show i
    show (RBool b) = "bool " ++ show b
    show RFunc {}  = "func"
    show RBFunc {} = "built-in"

type ExT = ExceptT String

type Name = String

type InterpreterMemory = M.Map Name MemoryObj

type InterpreterReader = ExT (ReaderT InterpreterMemory IO) Result
type InterpreterState = ExT (StateT InterpreterMemory IO) Result
type InterpreterMemoryState = ExT (StateT InterpreterMemory IO) InterpreterMemory

type BuiltInFunction = [BG.Exp] -> InterpreterState
data Arg = Arg BG.Type Name

data MemoryObj = Var Result
    | BuiltIn BuiltInFunction
    | Func BG.Type [Arg] [BG.Exp] InterpreterMemory
