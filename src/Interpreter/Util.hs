module Interpreter.Util
    ( -- * Functions
      typeEq,
      varNotFoundHandler,
      typesError,
      pprintResult,
      pprintType
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
varNotFoundHandler ex pos er = throwError $ varNotFoundError ex er pos

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

{- | Extends a simple information regaring 'variable not found' error with information
position of the erorr and the code itself.
-}
varNotFoundError :: Print a => a -> String -> BG.BNFC'Position -> String
varNotFoundError ex er (Just (line, col)) = er ++ "\n  In line " ++ show line
    ++ ", column " ++ show col ++ ":\n    " ++ printTree ex
varNotFoundError ex er Nothing = er ++ "\n  At undetermined position:\n    "
    ++ printTree ex

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
