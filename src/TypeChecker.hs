module TypeChecker
    ( -- * Functions
      checkTypes
  ) where

import qualified Baalbolge.Abs (Exps)

type Err = Either String

-- checkTypes
checkTypes :: Baalbolge.Abs.Exps -> Err Baalbolge.Abs.Exps
checkTypes = undefined
