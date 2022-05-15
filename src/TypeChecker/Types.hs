module TypeChecker.Types
    ( -- * Data types
      CheckTypeReader
      , CheckTypeState
      , CheckTypeMemory
      , CheckTypeMemoryState
      , Name

      -- * Data structures
      , MemoryObj (..)
      , Type (..)

      -- * Type functions
      , tSum
  ) where


import qualified Data.Map             as M

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State


-- Internal types
type Name = String
type ExT = ExceptT String

type CheckTypeMemory = M.Map Name MemoryObj

type CheckTypeReader = ExT (Reader CheckTypeMemory) Type
type CheckTypeState =  ExT (State CheckTypeMemory) Type
type CheckTypeMemoryState = ExT (State CheckTypeMemory) CheckTypeMemory

-- Internal datastructures
data MemoryObj = Var Type | Func Type [Type]

-- | The representation of types used in Baalbolge for the purpose of type checking
data Type = TUnit | TInt | TBool | TVar | TFunc Type [Type]

instance Show Type
  where
    show TUnit       = "unit"
    show TInt        = "int"
    show TBool       = "bool"
    show TVar        = "var"
    show (TFunc _ _) = "func"

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
tSum TInt TInt   = TInt
tSum TBool TBool = TBool
tSum _ _         = TVar
