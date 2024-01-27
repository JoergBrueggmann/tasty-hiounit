# Test.Tasty.HIOUnit in package tasty-hiounit
A test provider for the test framework *tasty*. Like like *tasty-hunit* it enables unit tests of IO functions that may use *stdin* where *stdout*, *stderr* and exit code is checked.

NOTE: This package needs at least version 1.5 of package tasty. This is why 'extra-deps:' has been set to 'tasty-1.5' in file 'stack.yaml'.

## Background
The test framework *tasty* automates a lot of different kinds of tests and provides an overview of all tests at the end.
For instance, tests of invariants with many values and unit test with selected values on edge cases.

The following extract of test code shows how functions are tested

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

All this is software unit testing on function level where *IO* is not involved.

However, test of CLI applications are only complete if you also test functions when integrated into its monadic *IO* context.

## Problem
Unfortunately, function tests with *IO* involved can become tricky, in particular if you want to integrate functions that expect 'stdin' an you want to its output, because once stdin is consumed it hardly can be recreated. At least I tried, failed and gave up this method.

How to perform a function test of the following code?

    ioFib :: IO ()
    ioFib = 
        do
            Sys.hSetBuffering Sys.stdout Sys.NoBuffering
            sNumber <- getLine
            let
                niNumber = (read sNumber) :: Integer
            putStr (show (fib niNumber))

See also:
https://stackoverflow.com/questions/77471164/how-do-i-capture-stdout-and-simulate-stdin-in-a-haskell-tasty-test

## Proposed Solution
The code below creates module 'Fib' and calls a program that compiles and executes 'ioFib' with the given 'stdin' input.

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

The example code in folder './test/' creates the following output:

    tasty-hiounit> test (suite: tasty-hiounit-test)

    Tests
    module Test.Tasty.HIOUnit
        Fib.ioFib
        Fib.ioFib -2: OK (2.73s)
        Fib.ioFib -1: OK (2.82s)
        Fib.ioFib 0:  OK (2.66s)
        Fib.ioFib 1:  OK (2.57s)
        Fib.ioFib 2:  OK (2.54s)
        Fib.ioFib 3:  OK (2.55s)
        fib
        fib:          OK
            +++ OK, passed 100 tests.
        fib (-2):     OK
        fib (-1):     OK
        fib 0:        OK
        fib 1:        OK
        fib 2:        OK
        fib 3:        OK

    All 13 tests passed (15.87s)



    tasty-hiounit> Test suite tasty-hiounit-test passed

## Before Usage
IMPORTANT: Before you can use the module Test.Tasty.HIOUnit make sure the program 'hdynexec' is build and executable.

To do this:

- build the package with `stack build`
- install `hdynexec` with `stack install`

## License
BSD-3-Clause

See file 'LICENSE'

## Author
Jörg Karl-Heinz Walter Brüggmann

## Maintainer
Jörg Karl-Heinz Walter Brüggmann
