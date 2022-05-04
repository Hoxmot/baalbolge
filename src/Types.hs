module Types
    ( -- * Data types
      Err
      
      -- * Data structures
      , Result (..)
      , Type (..)
  ) where

type Err = Either String

-- | The representation of types used in Baalbolge
data Type = TUnit | TInt | TBool | TVar deriving (Show)
data Result = RUnit | RInt Integer | RBool Bool deriving (Show)
