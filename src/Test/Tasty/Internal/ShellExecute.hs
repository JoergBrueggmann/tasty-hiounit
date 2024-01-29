{-|
Description : ... executes shell commands.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Test.Tasty.Internal.ShellExecute' provides helper functions to execute shell commands.

Suggested import line:

    @
import qualified Test.Tasty.Internal.ShellExecute as Sh
    @
-}


module Test.Tasty.Internal.ShellExecute
    (
        runCommandWithIO
    ) where


import qualified Test.Tasty.Internal.Safer as Sfr

import qualified System.Process as Proc
import qualified Control.Concurrent as Conc
import qualified System.IO as SyIO
import qualified System.Exit as Sys
import qualified Data.Functor as Fnc
import qualified Control.Monad as M


-- TODO: Add documentation
runCommandWithIO :: Maybe FilePath -> Maybe FilePath -> String -> [String] -> Maybe String -> IO (Maybe (String, String, Int))
runCommandWithIO msShell msWorkingDirectory sCommand lsArgs msInputToPassAsStdIn = 
    do
        mHandles <- runInteractiveCommandInDir msShell msWorkingDirectory sCommand lsArgs
        case mHandles of
            Just (hInput, hOutput, hError, hProcess) ->
                do
                    Sfr.ifJust msInputToPassAsStdIn (forkIO' . SyIO.hPutStr hInput) (return ())
                    mVarStdOut <- Conc.newEmptyMVar
                    mVarStdErr <- Conc.newEmptyMVar
                    forkIO' (hOutput `hGetContentsStrictlyAnd` Conc.putMVar mVarStdOut)
                    forkIO' (hError `hGetContentsStrictlyAnd` Conc.putMVar mVarStdErr)
                    exitCode <- Proc.waitForProcess hProcess Fnc.<&> fromExitCode
                    sStdOut <- Conc.takeMVar mVarStdOut
                    sStdErr <- Conc.takeMVar mVarStdErr
                    return (Just (sStdOut, sStdErr, exitCode))
            _                                           ->
                do
                    putStrLn "Error: Function 'runInteractiveCommandInDir' failed to create handles."
                    return Nothing
    where
        forkIO' :: IO () -> IO ()
        forkIO' io = M.void (Conc.forkIO io)

-- TODO: Add documentation
fromExitCode :: Sys.ExitCode -> Int
fromExitCode Sys.ExitSuccess = 0
fromExitCode (Sys.ExitFailure exitCode) = exitCode

-- TODO: Add documentation
runInteractiveCommandInDir :: Maybe FilePath -> Maybe FilePath -> String -> [String] -> IO (Maybe (SyIO.Handle, SyIO.Handle, SyIO.Handle, Proc.ProcessHandle))
runInteractiveCommandInDir msShell msWorkingDirectory sCommand lsArgs = do
        (mhStdIn, mhStdOut, mhStdErr, hProcess) <-
            Proc.createProcess $
                processSpec
                    { Proc.cwd = msWorkingDirectory
                    , Proc.std_in  = Proc.CreatePipe
                    , Proc.std_out = Proc.CreatePipe
                    , Proc.std_err = Proc.CreatePipe }
        return
            (Sfr.ifJust3
                mhStdIn
                mhStdOut
                mhStdErr
                (\hStdIn hStdOut hStdErr -> Just (hStdIn, hStdOut, hStdErr, hProcess))
                Nothing)
    where
        processSpec = Sfr.ifJust msShell (\msShell' -> Proc.proc msShell' (["-c", sCommand] ++ lsArgs)) (Proc.proc sCommand lsArgs)

-- TODO: Add documentation
hGetContentsStrictlyAnd :: SyIO.Handle -> (String -> IO b) -> IO b
hGetContentsStrictlyAnd h f = SyIO.hGetContents h >>= \s -> length s `seq` f s
