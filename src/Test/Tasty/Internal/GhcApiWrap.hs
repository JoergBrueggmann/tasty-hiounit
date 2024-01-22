{-|
Description : provides more convinient functions to use the GHC API.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tasty.Internal.GhcApiWrap
    (
        compileAndLoad
    ) where


import qualified Control.Exception.Safe as Exc
import qualified Data.Dynamic as Dyn
import qualified Data.Proxy as Px
import qualified Data.Typeable as T
import qualified GHC as Ghc
import qualified GHC.Paths as Pth
import Data.Functor ((<&>))


compileAndLoad 
    :: forall a. T.Typeable a 
    => String 
    -> String 
    -> String 
    -> IO (Either String a)
compileAndLoad dir mdl sym = runGhcCatched >>= ethSym
  where
    runGhcCatched = 
        Ghc.runGhc (Just Pth.libdir) (loadModule dir mdl sym)
        `Exc.catchAny`
        (\ex -> return (Left ("Exception: " ++ show ex)))
    ethSym (Right dynSym) = 
        return (ethSym' (Dyn.fromDynamic dynSym) (sErrFromDynSym dynSym))
    ethSym (Left sErr) = 
        return (Left sErr)
    sErrFromDynSym dynSym = 
        "Symbol " ++ sym ++ " has type \"" ++ show dynSym ++ 
        "\" but expected type \"" ++ show symbolType ++ "\""
    ethSym' (Just dynSym) _ = Right dynSym
    ethSym' Nothing sErr = Left sErr
    symbolType = T.typeRep (Px.Proxy :: Px.Proxy a)

loadModule 
    :: Ghc.GhcMonad m 
    => String 
    -> String 
    -> String 
    -> m (Either String Dyn.Dynamic)
loadModule dir mdl sym = 
        Ghc.getSessionDynFlags >>= 
        Ghc.setSessionDynFlags >> 
        Ghc.guessTarget (dir ++ mdl ++ ".hs") Nothing Nothing >>= 
        (\target -> Ghc.setTargets [target]) >> 
        Ghc.load Ghc.LoadAllTargets >>= 
        returnFromLoadStatus
    where
        returnFromLoadStatus Ghc.Succeeded = 
            dynSym <&> Right
        returnFromLoadStatus Ghc.Failed = 
            return (Left ("Could not load the module \"" ++ mdl ++ "\"!"))
        dynSym = Ghc.parseImportDecl ("import " ++ mdl) >>= 
                 (\importDecl -> Ghc.setContext [Ghc.IIDecl importDecl]) >> 
                 Ghc.dynCompileExpr (mdl ++ "." ++ sym)
