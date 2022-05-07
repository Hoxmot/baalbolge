module Types
    ( -- * Data types
      Err
      , CheckTypeReader
      , CheckTypeState

      -- * Data structures
      , Result (..)
      , Type (..)
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map as M

type Err = Either String
type ExT = ExceptT String

type Name = String
type CheckTypeEnv = M.Map Name Type

type CheckTypeReader = ExT (Reader CheckTypeEnv) Type
type CheckTypeState =  ExT (State CheckTypeEnv) Type

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
