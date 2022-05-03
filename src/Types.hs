module Types
    ( -- * Data types
      Err
      
      -- * Data structures
      , Type (..)
  ) where

type Err = Either String

-- | The representation of types used in Baalbolge
data Type = TUnit | TInt | TBool | TVar deriving (Show)
