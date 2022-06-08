module TypeChecker.Util
    ( -- * Functions
      notPrintableError
      , printTypesErrorHandler
      , varNotFoundHandler
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

-- | Handler of 'types don't match' error for print function.
printTypesErrorHandler :: Print a => a -> BG.BNFC'Position -> String -> CheckTypeState
printTypesErrorHandler ex pos er = throwError $ printTypesError ex er pos

-- | Creates a message about types error in the code.
typesError :: Print a => a -> String -> BG.BNFC'Position -> Type -> Type -> String
typesError ex op (Just (line, col)) t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n  Expected '"
    ++ show t1 ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 = "Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n  Expected '" ++ show t1
    ++ "' but got '" ++ show t2 ++ "'!\n    (" ++ printTree ex ++ ")"

-- | Creates a message about types error inside the print internal function.
notPrintableError :: Print a => a -> BG.BNFC'Position -> String
notPrintableError ex (Just (line, col)) = "Types don't match! In line " ++ show line
    ++ ", column " ++ show col
    ++ ":\n  Expected printable, but got function. You cannot print a function!\n    "
    ++ printTree ex
notPrintableError ex Nothing =
    "Types don't match! At undetermined position:\n  Expected printable, but got function."
    ++ " You cannot print a function!\n    " ++ printTree ex

{- | Extends a simple information regaring 'variable not found' error with information
position of the erorr and the code itself.
-}
varNotFoundError :: Print a => a -> String -> BG.BNFC'Position -> String
varNotFoundError ex er (Just (line, col)) = er ++ "\n  In line " ++ show line
    ++ ", column " ++ show col ++ ":\n    " ++ printTree ex
varNotFoundError ex er Nothing = er ++ "\n  At undetermined position:\n    "
    ++ printTree ex

{- | Extends a simple information regarding 'types don't match' for internal function
print. Nicely wraps the error and adds more information.
-}
printTypesError :: Print a => a -> String -> BG.BNFC'Position -> String
printTypesError ex er (Just (line, col)) =
    "Types don't match! In operation 'print' in line " ++ show line ++ ", column "
    ++ show col ++ ":\n  " ++ printTree ex ++ "\nRoot exception:\n" ++ er
printTypesError ex er Nothing =
    "Types don't match! In operation 'print' at undetermined position:\n  "
    ++ printTree ex ++ "Root exception:\n" ++ er
