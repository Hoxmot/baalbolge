module Types
    ( -- * Data types
      Err
      , ExT
  ) where

import           Control.Monad.Except (ExceptT)

type ExT = ExceptT String
type Err = ExT IO
