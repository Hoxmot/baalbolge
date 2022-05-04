module TypeChecker
    ( -- * Functions
      checkTypes
  ) where

import           Control.Monad (foldM)

import qualified Baalbolge.Abs as BG

import Baalbolge.Print
import           Types

{- | Checks the types in the given program. The check is pefrormed in a static manner.
If some of the types are unknown due to usage of `var` type, the check is a success and
any problems with types are found and thrown during execution of the program.

For the whole program, any type can be returned.
-}
checkTypes :: BG.Exps -> Err BG.Exps
checkTypes prog@(BG.Program _ exps) = do
    _ <- checkTypesExps exps
    return prog

{- | A function, which checks the return type of the list of Exps. The check is run for
each element of the list of Exps.

For the list of Exps, any type can be returned.
-}
checkTypesExps :: [BG.Exp] -> Err Type
checkTypesExps = foldM expsMapper TUnit

{- | A function, which maps Exps from the list and determines the return type of the whole
list. In Baalbolge, the first Exp to return a type other than Unit determines the return
Type of the whole list of Exps.
-}
expsMapper :: Type -> BG.Exp -> Err Type
expsMapper TUnit e = checkTypesExp e
expsMapper t e =
    case checkTypesExp e of
        Left err -> Left err
        _        -> Right t

{- | A function, which determines the return type of a single Exp.

$setup
>>> let p = Just (2,2)
>>> let b = (BG.BTrue p)

>>> checkTypesExp (BG.EUnit p)
Right unit

>>> checkTypesExp (BG.EInt p 42)
Right int

>>> checkTypesExp (BG.EBool p b)
Right bool
-}
checkTypesExp :: BG.Exp -> Err Type
checkTypesExp (BG.EUnit _) = return TUnit
checkTypesExp (BG.EInt _ _) = return TInt
checkTypesExp (BG.EBool _ _) = return TBool
checkTypesExp (BG.EInternal _ d) = checkTypesIFunc d
checkTypesExp e = Left $ "Checking types of exp: " ++ show e ++ " is not yet implemented"

checkTypesIFunc :: BG.InternalFunc -> Err Type
checkTypesIFunc iFunc@(BG.IVarDecl pos t v e) = do
    tDecl <- checkTypesType t
    tExp <- checkTypesExp e
    if tDecl == tExp
      then return TUnit
      else typesError iFunc "VariableDeclaration" pos tDecl tExp
checkTypesIFunc e = Left $ "Checking types for internal function: " ++ show e
    ++ " is not yet implemented"

checkTypesType :: BG.Type -> Err Type
checkTypesType (BG.TInt _) = return TInt
checkTypesType e = Left $ "Checking types of type: " ++ show e ++ " is not yet implemented"

typesError :: Print a => a -> String -> BG.BNFC'Position -> Type -> Type -> Err Type
typesError ex op (Just (line, col)) t1 t2 = Left $ "Types don't match! In operation '" ++ op
    ++ "' in line " ++ show line ++ ", column " ++ show col ++ ":\n\tExpected '"
    ++ show t1 ++ "' but got '" ++ show t2 ++ "'!\n(" ++ printTree ex ++ ")"
typesError ex op Nothing t1 t2 = Left $ "Types don't match! In operation '" ++ op
    ++ "' at undetermined position:\n\tExpected '" ++ show t1
    ++ "' but got '" ++ show t2 ++ "'!\n(" ++ printTree ex ++ ")"
