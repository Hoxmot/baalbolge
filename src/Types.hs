module Types
    ( -- * Data types
      Err

      -- * Data structures
      , Result (..)
  ) where


type Err = Either String

{- | The representation of types used in Baalbolge for the purpose of computing the result
of computation.
-}
data Result = RUnit | RInt Integer | RBool Bool deriving (Show)
