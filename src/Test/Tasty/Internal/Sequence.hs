{-|
Description : provides functions to sequence IO functions that are started in prallel.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Test.Tasty.Internal.Sequence' provides functions to sequence IO functions that are started in prallel.

Example: The functions a, b, and c run in paralell by execAllParallel but Seq.ioOpen and Seq.ioClose prevent that.

    @
import qualified Test.Tasty.Internal.Sequence as Seq

import qualified Control.Concurrent as Con
import qualified Control.Monad as M
import qualified System.IO as Sys


main :: IO ()
main =
    do
        Sys.hSetBuffering Sys.stdout Sys.NoBuffering
        (lio,mvarEnd) <- Seq.ioOpen [ a, b, c ]
        lmvar <- execAllParallel lio
        Seq.ioClose mvarEnd
        waitAllParallel lmvar
        putStrLn ""

execAllParallel :: [IO ()] -> IO [Con.MVar ()]
execAllParallel = execAllParallel' []
    where
        execAllParallel' :: [Con.MVar ()] -> [IO ()] -> IO [Con.MVar ()]
        execAllParallel' acc [] = return acc
        execAllParallel' acc (io:lrio) = Con.newEmptyMVar >>= (\mvar -> M.void (Con.forkIO (io >> Con.putMVar mvar ())) >> execAllParallel' (mvar : acc) lrio)

waitAllParallel :: [Con.MVar ()] -> IO ()
waitAllParallel [] = return ()
waitAllParallel (mvar:lrmvar) = M.void $ Con.takeMVar mvar >> waitAllParallel lrmvar

a :: IO ()
a = putStr "Vom Eise befreit sind Strom und Bäche..."

b :: IO ()
b = putStr "________________________________________"

c :: IO ()
c = putStr "----------------------------------------"
    @

-}


module Test.Tasty.Internal.Sequence
    (
        ioOpen, 
        ioClose, 
        linkSequential
    )
    where


import qualified Control.Concurrent as Con
import qualified Control.Monad as M


ioOpen :: [IO ()] -> IO ([IO ()],Con.MVar ())
ioOpen lio = 
    do
        mvar0 <- Con.newMVar ()
        ioOpen' mvar0 lio
    where
        ioOpen' :: Con.MVar () -> [IO ()] -> IO ([IO ()],Con.MVar ())
        ioOpen' mvar0 [] = return ([],mvar0)
        ioOpen' mvar0 (io:lrio) = 
            do
                mvar1 <- Con.newEmptyMVar
                (lio',mvarZ) <- ioOpen' mvar1 lrio
                return (linkSequential io mvar0 mvar1 : lio',mvarZ)

ioClose :: Con.MVar () -> IO ()
ioClose mvarEnd = M.void $ Con.takeMVar mvarEnd

linkSequential :: IO () -> Con.MVar () -> Con.MVar () -> IO ()
linkSequential io mvarToTake mvarToPut = M.void $ Con.takeMVar mvarToTake >> io >> Con.putMVar mvarToPut ()
