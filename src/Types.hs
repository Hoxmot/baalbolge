module Types
    ( -- * Data types
      Err
      , InterpreterReader
      , InterpreterState

      -- * Data structures
      , Result (..)
  ) where

import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State


type Err = Either String
type ExT = ExceptT String

type Name = String

type InterpreterEnv = M.Map Name Result
type InterpreterSt = InterpreterEnv

type InterpreterReader = ExT (Reader InterpreterEnv) Result
type InterpreterState = ExT (State InterpreterSt) Result

{- | The representation of types used in Baalbolge for the purpose of computing the result
of computation.
-}
data Result = RUnit | RInt Integer | RBool Bool deriving (Show)
