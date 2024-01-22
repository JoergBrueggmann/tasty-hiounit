# Test.Tasty.HIOUnit in package tasty-hiounit
A test provider for the test framework *tasty*. Like like *tasty-hunit* it enables unit tests of IO functions that may use *stdin* where *stdout*, *stderr* and exit code is checked.

**IMPORTANT: Known issue: It doesn't run without error when you run it the first time. Because some source files are generated lazily while *ghc* tries to compile the file.**

## Background
The test framework *tasty* automates a lot of different kinds of tests and provides an overview of all tests at the end.
For instance, tests of invariants with many values and unit test with selected values on edge cases.

The following extract of test code shows how functions

    testGroup :: T.TestTree
    testGroup = 
        T.testGroup
            "N.fib"
            [
                -- check of invariant with random values
                QC.testProperty "N.fib" (... -> True QC.==> (N.fib (... n)) == (N.fib ((... n) - 1)) + (N.fib ((... n) - 2)) ), 
                -- edge cases using values arround zero
                HU.testCase "N.fib (-2)" (N.fib (-2 :: Integer) HU.@?= (-1)), 
                HU.testCase "N.fib (-1)" (N.fib (-1 :: Integer) HU.@?= 1), 
                HU.testCase "N.fib 0" (N.fib (0 :: Integer) HU.@?= 0), 
                HU.testCase "N.fib 1" (N.fib (1 :: Integer) HU.@?= 1), 
                HU.testCase "N.fib 2" (N.fib (2 :: Integer) HU.@?= 1), 
                HU.testCase "N.fib 3" (N.fib (3 :: Integer) HU.@?= 2)
            ]

All this is software unit testing on function level where *IO* is not involved.

However, test are only complete if you also test functions that are integrated into *IO*.

## Problem
Unfortunately, function tests with *IO* involved can become tricky, in particular if you want to integrate functions that expect stdin an you want to its output, because once stdin is consumed it hardly can be recreated. At least I tried, failed and gave up this method.

How to perform a function test of the following code?

    ioFib :: IO ()
    ioFib = 
        do
            sNumber <- getLine
            let
                niNumber = (read sNumber) :: Integer
            print $ N.fib niNumber

See also:
https://stackoverflow.com/questions/77471164/how-do-i-capture-stdout-and-simulate-stdin-in-a-haskell-tasty-test

## Proposed Solution
The code below creates module 'Fib' and calls a program that compiles and executes 'ioFib' with the given stdin input.

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

## Before Usage
Before you can use the module Test.Tasty.HIOUnit make sure the program 'hdynexec' is build and executable.

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
