module TypeChecker
    ( -- * Functions
      checkTypes
  ) where

import           Control.Monad (foldM)

import qualified Baalbolge.Abs as BG

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
expsMapper TUnit e =
    case checkTypesExp e of
        Left err -> Left err
        Right t  -> Right t
expsMapper t e =
    case checkTypesExp e of
        Left err -> Left err
        _        -> Right t

{- | A function, which determines the return type of a single Exp.

$setup
>>> let p = Just (2,2)
>>> let b = (BG.BTrue p)

>>> checkTypesExp (BG.EUnit p)
Right TUnit

>>> checkTypesExp (BG.EInt p 42)
Right TInt

>>> checkTypesExp (BG.EBool p b)
Right TBool
-}
checkTypesExp :: BG.Exp -> Err Type
checkTypesExp (BG.EUnit _) = return TUnit
checkTypesExp (BG.EInt _ _) = return TInt
checkTypesExp (BG.EBool _ _) = return TBool

checkTypesExp e = Left $ "Checking types of exp: " ++ show e ++ " is not yet implemented"
