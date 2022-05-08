module TypeChecker.Types
    ( -- * Data types
      CheckTypeReader
      , CheckTypeState

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

type CheckTypeEnv = M.Map Name MemoryObj
type CheckTypeSt = CheckTypeEnv

type CheckTypeReader = ExT (Reader CheckTypeEnv) Type
type CheckTypeState =  ExT (State CheckTypeSt) Type

-- Internal datastructures
data MemoryObj = Var Type | Func Type [Type]

-- | The representation of types used in Baalbolge for the purpose of type checking
data Type = TUnit | TInt | TBool | TVar

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
tSum TInt TInt   = TInt
tSum TBool TBool = TBool
tSum _ _         = TVar