module TypeChecker.Util
    ( -- * Functions
      varNotFoundHandler
      , typesError
  ) where

import           Control.Monad.Except

import qualified Baalbolge.Abs        as BG

import           Baalbolge.Print
import           TypeChecker.Types


{- | Handler of 'variable not found' error, which adds details about the error
for more readability.
-}
varNotFoundHandler :: Print a => a -> BG.BNFC'Position -> String -> CheckTypeState
varNotFoundHandler ex pos er = throwError $ varNotFoundError ex er pos

-- | Creates a message about types error in the code.
typesError :: Print a => a -> String -> BG.BNFC'Position -> Type -> Type -> String
typesError ex op (Just (line, col)) t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n  Expected '"
    ++ show t1 ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n  Expected '" ++ show t1
    ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"

{- | Extends a simple information regaring 'variable not found' error with information
position of the erorr and the code itself.
-}
varNotFoundError :: Print a => a -> String -> BG.BNFC'Position -> String
varNotFoundError ex er (Just (line, col)) = er ++ "\n  In line " ++ show line
    ++ ", column " ++ show col ++ ":\n    " ++ printTree ex
varNotFoundError ex er Nothing = er ++ "\n  At undetermined position:\n    "
    ++ printTree ex
