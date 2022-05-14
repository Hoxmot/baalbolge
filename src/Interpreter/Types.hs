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
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Baalbolge.Abs        as BG

import           Types

type ExT = ExceptT String

type Name = String

type InterpreterMemory = M.Map Name MemoryObj

type InterpreterReader = ExT (Reader InterpreterMemory) Result
type InterpreterState = ExT (State InterpreterMemory) Result
type InterpreterMemoryState = ExT (State InterpreterMemory) InterpreterMemory

type BuiltInFunction = [BG.Exp] -> InterpreterState
data Arg = Arg BG.Type Name

data MemoryObj = Var Result | BuiltIn BuiltInFunction | Func BG.Type [Arg] [BG.Exp]
