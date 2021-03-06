module TypeChecker.Memory
    ( -- * Functions
      initialTypeCheckerMem
  ) where

import qualified Data.Map             as M

import qualified TypeChecker.Types    as TC

-- | The initial memory for the Type Checker. Consists of all the built-in functions.
initialTypeCheckerMem :: TC.CheckTypeMemory
initialTypeCheckerMem = M.fromList typeCheckerMemElems

typeCheckerMemElems :: [(TC.Name, TC.MemoryObj)]
typeCheckerMemElems = [
    ("+", TC.Func TC.TInt [TC.TInt, TC.TInt])
    , ("-", TC.Func TC.TInt [TC.TInt, TC.TInt])
    , ("*", TC.Func TC.TInt [TC.TInt, TC.TInt])
    , ("/", TC.Func TC.TInt [TC.TInt, TC.TInt])
    , ("%", TC.Func TC.TInt [TC.TInt, TC.TInt])
    , (">", TC.Func TC.TBool [TC.TInt, TC.TInt])
    , ("=", TC.Func TC.TBool [TC.TInt, TC.TInt])
  ]
