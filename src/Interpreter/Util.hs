module Interpreter.Util
    ( -- * Functions
      typeEq
      , unionRight
      , typesError
      , varNotFoundHandler
      , printTypesErrorHandler
      , pprintResult
      , pprintType
      , notPrintableError
  ) where


import           Control.Monad.Except

import qualified Baalbolge.Abs        as BG

import           Baalbolge.Print
import           Interpreter.Types


{- | Compares the type of variable and the value we try to assign to it. It's an extension
of type checking in the TypeChecker as type var can produce runtime type incompatibilities.
-}
typeEq :: BG.Type -> Result -> Bool
typeEq (BG.TInt _) (RInt _)   = True
typeEq (BG.TBool _) (RBool _) = True
typeEq (BG.TVar _) _          = True
typeEq (BG.TUnit _) RUnit     = True
typeEq (BG.TUnit _) _         = False
typeEq _ _                    = False


{- | Handler of 'variable not found' error, which adds details about the error
for more readability.
-}
varNotFoundHandler :: Print a => a -> BG.BNFC'Position -> String -> InterpreterState
varNotFoundHandler ex pos er =
    throwError $ varNotFoundError ex ("Runtime exception! " ++ er) pos

-- | Handler of 'types don't match' error for print function.
printTypesErrorHandler :: Print a => a -> BG.BNFC'Position -> String -> InterpreterState
printTypesErrorHandler ex pos er = throwError $ printTypesError ex er pos


-- | Creates a message about types error in the code.
typesError :: Print a => a -> String -> BG.BNFC'Position -> String -> String -> String
typesError ex op (Just (line, col)) t1 t2 =
    "Runtime exception! Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n  Expected '"
    ++ t1 ++ "' but got '" ++ t2 ++ "'!\n    (" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 =
    "Runtime exception! Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n  Expected '" ++ t1
    ++ "' but got '" ++ t2 ++ "'!\n    (" ++ printTree ex ++ ")"

-- | Creates a message about types error inside the print internal function.
notPrintableError :: Print a => a -> BG.BNFC'Position -> String -> String
notPrintableError ex (Just (line, col)) t = "Types don't match! In line " ++ show line
    ++ ", column " ++ show col ++ ":\n  Expected printable, but got '" ++  t
    ++ "'. You cannot print a function!\n    " ++ printTree ex
notPrintableError ex Nothing t =
    "Types don't match! At undetermined position:\n  Expected printable, but got '" ++ t
    ++ "'. You cannot print a function!\n    " ++ printTree ex

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
    "Runtime exception! Types don't match! In operation 'print' in line " ++ show line
    ++ ", column " ++ show col ++ ":\n  " ++ printTree ex ++ "\nRoot exception:\n" ++ er
printTypesError ex er Nothing =
    "Runtime exception! Types don't match! In operation 'print' at undetermined position:\n  "
    ++ printTree ex ++ "Root exception:\n" ++ er


-- | Pretty prints the type used in Baalbolge
pprintType :: BG.Type -> String
pprintType (BG.TInt _)  = "int"
pprintType (BG.TBool _) = "bool"
pprintType (BG.TUnit _) = "unit"
pprintType (BG.TVar _)  = "var"
-- TODO: implementation of lists
pprintType _            = undefined

-- | Pretty prints the type of a result of an expression
pprintResult :: Result -> String
pprintResult RUnit     = "unit"
pprintResult (RInt _)  = "int"
pprintResult (RBool _) = "bool"
pprintResult RFunc {} = "func"
pprintResult RBFunc {} = "built-in"

{- | A function used for Data.Map.unionWith, which preferes the second object over the
first one

>>> unionRight "a" "b"
"b"

>>> unionRight 7 42
42

>>> unionRight [42] [7]
[7]
-}
unionRight :: a -> a -> a
unionRight _o1 o2 = o2
