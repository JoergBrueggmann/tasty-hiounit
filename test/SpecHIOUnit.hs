{-|
Description : test module to test 'Test.Tasty.HIOUnit' with some example code.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE QuasiQuotes #-}


module SpecHIOUnit
    (
        testGroup
    ) where


import qualified Test.Tasty as T

import qualified Test.Tasty.HIOUnit as HIOU


testGroup :: T.TestTree
testGroup =
    T.testGroup
        "module Test.Tasty.HIOUnit"
        [
            tgFib
        ]

tgFib :: T.TestTree
tgFib = 
    T.testGroup
        "Fib.ioFib"
        [
            -- edge cases using values arround zero
            HIOU.testCase "Fib.ioFib -2" (HIOU.IOTestParameter ( Just "-2" ) fdefFib ( Just "-1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib -1" (HIOU.IOTestParameter ( Just "-1" ) fdefFib ( Just "1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 0" (HIOU.IOTestParameter ( Just "0" ) fdefFib ( Just "0" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 1" (HIOU.IOTestParameter ( Just "1" ) fdefFib ( Just "1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 2" (HIOU.IOTestParameter ( Just "2" ) fdefFib ( Just "1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 3" (HIOU.IOTestParameter ( Just "3" ) fdefFib ( Just "2" ) ( Just "" ) ( Just 0 ))
        ]

fdefFib :: HIOU.IOFunctionDefinition
fdefFib = 
    HIOU.IOFunctionDefinition
        "Fib"
        (Just "ioFib")
        [HIOU.sMultiLine|
import qualified System.IO as Sys


ioFib :: IO ()
ioFib = 
    do
        Sys.hSetBuffering Sys.stdout Sys.NoBuffering
        sNumber <- getLine
        let
            niNumber = (read sNumber) :: Integer
        putStr (show (fib niNumber))

fib :: (Integral n) => n -> n
fib n 
    | n >= 0 = fib' n (0,1)
    | even n = 0 - fib (-n)
    | otherwise = fib (-n)
  where
    fib' n' (a, b)
        | n'==0      = a
        | otherwise = fib' (n'-1) (b, a+b)
|]
