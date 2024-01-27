{-|
Description : test module to test 'Test.Tasty.HIOUnit' with some example code.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module SpecHIOUnit
    (
        testGroup
    ) where


import qualified Test.Tasty as T

import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU

import qualified Test.Tasty.HIOUnit as HIOU -- <<< import like this

import qualified SpecTestHelper as TH


testGroup :: T.TestTree
testGroup =
    T.testGroup
        "module Test.Tasty.HIOUnit"
        [
            tgIOFib, 
            tgFib
        ]

tgIOFib :: T.TestTree
tgIOFib = 
    T.sequentialTestGroup   -- <<< NOTE: Unit tests with HIOUnit have to be executed sequential.
        "Fib.ioFib"
        T.AllSucceed
        [
            -- unit test with generated stdio input 
            -- edge cases using values arround zero, executing the code as defined by function 'fdefFib'
            HIOU.testCase "Fib.ioFib -2" (HIOU.IOTestParameter ( Just "-2" ) fdefFib ( Just "-1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib -1" (HIOU.IOTestParameter ( Just "-1" ) fdefFib ( Just "1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 0" (HIOU.IOTestParameter ( Just "0" ) fdefFib ( Just "0" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 1" (HIOU.IOTestParameter ( Just "1" ) fdefFib ( Just "1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 2" (HIOU.IOTestParameter ( Just "2" ) fdefFib ( Just "1" ) ( Just "" ) ( Just 0 )), 
            HIOU.testCase "Fib.ioFib 3" (HIOU.IOTestParameter ( Just "3" ) fdefFib ( Just "2" ) ( Just "" ) ( Just 0 ))
        ]

tgFib :: T.TestTree
tgFib = 
    T.testGroup
        "fib"
        [
            -- usual quick check and unit tests
            QC.testProperty "fib" (\(n :: TH.Int256Plus ) -> True QC.==> (fib (TH.niFromInt256Plus n)) == (fib ((TH.niFromInt256Plus n) - 1)) + (fib ((TH.niFromInt256Plus n) - 2)) ), 
            HU.testCase "fib (-2)" (fib (-2 :: Integer) HU.@?= (-1)), 
            HU.testCase "fib (-1)" (fib (-1 :: Integer) HU.@?= 1), 
            HU.testCase "fib 0" (fib (0 :: Integer) HU.@?= 0), 
            HU.testCase "fib 1" (fib (1 :: Integer) HU.@?= 1), 
            HU.testCase "fib 2" (fib (2 :: Integer) HU.@?= 1), 
            HU.testCase "fib 3" (fib (3 :: Integer) HU.@?= 2)
        ]

-- fdefFib
{- ...is a definition of a function that i.e. leads to creation of file 'ioFib' 
    with the code defined in the multiline expression when executed in 'HIOU.testCase'.
* the file will be created in directory './.iotest/'
* the module name will be 'Fib' as indicated as first parameter
* hence the file name will be 'Fib.hs'
-}
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

fib :: (Integral n) => n -> n
fib n 
    | n >= 0 = fib' n (0,1)
    | even n = 0 - fib (-n)
    | otherwise = fib (-n)
  where
    fib' n' (a, b)
        | n'==0      = a
        | otherwise = fib' (n'-1) (b, a+b)
