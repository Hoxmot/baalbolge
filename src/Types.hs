module Types
    ( -- * Data types
      Err

      -- * Data structures
      , Result (..)
      , Type (..)
  ) where

type Err = Either String

-- | The representation of types used in Baalbolge
data Type = TUnit | TInt | TBool | TVar
data Result = RUnit | RInt Integer | RBool Bool deriving (Show)

instance Show Type
  where
    show TUnit = "unit"
    show TInt  = "int"
    show TBool = "bool"
    show TVar  = "var"

instance Eq Type
  where
    TVar == _ = True
    _ == TVar = True
    TUnit == TUnit = True
    TInt == TInt = True
    TBool == TBool = True
    _ == _ = False
