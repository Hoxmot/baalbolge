module Types
    ( -- * Data types
      Err
      , CheckTypeReader
      , CheckTypeState
      , InterpreterReader
      , InterpreterState

      -- * Data structures
      , Result (..)
      , Type (..)

      -- * Type functions
      , tSum
  ) where

import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

type Err = Either String
type ExT = ExceptT String

type Name = String

type CheckTypeEnv = M.Map Name Type
type CheckTypeSt = CheckTypeEnv

type InterpreterEnv = M.Map Name Result
type InterpreterSt = InterpreterEnv

type CheckTypeReader = ExT (Reader CheckTypeEnv) Type
type CheckTypeState =  ExT (State CheckTypeSt) Type

type InterpreterReader = ExT (Reader InterpreterEnv) Result
type InterpreterState = ExT (State InterpreterSt) Result

-- | The representation of types used in Baalbolge for the purpose of type checking
data Type = TUnit | TInt | TBool | TVar

{- | The representation of types used in Baalbolge for the purpose of computing the result
of computation.
-}
data Result = RUnit | RInt Integer | RBool Bool deriving (Show)

instance Show Type
  where
    show TUnit = "unit"
    show TInt  = "int"
    show TBool = "bool"
    show TVar  = "var"

{- | Specific implementation of Eq for types in type checker. TVar is equal to any type
in Baalbolge.
-}
instance Eq Type
  where
    TVar == _ = True
    _ == TVar = True
    TUnit == TUnit = True
    TInt == TInt = True
    TBool == TBool = True
    _ == _ = False

tSum :: Type -> Type -> Type
tSum TUnit TUnit = TUnit
tSum TInt TInt = TInt
tSum TBool TBool = TBool
tSum _ _ = TVar
