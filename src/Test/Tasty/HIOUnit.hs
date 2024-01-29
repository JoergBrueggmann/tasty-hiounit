{-|
Description : ... execute test where IO like stdin, stdout, and stderr is involved, dynamically.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Test.Tasty.HIOUnit' provides functions to execute test where IO like stdin, stdout, and stderr is involved, dynamically.

Suggested import line:

    @
import qualified Test.Tasty.HIOUnit as HIOU
    @

Example:

    @
import qualified Test.Tasty.HIOUnit as HIOU

    ...

tgIOFib :: T.TestTree
tgIOFib = 
    T.sequentialTestGroup   -- <<< NOTE: Unit tests with HIOUnit have to be executed sequential.
        "Fib.ioFib"
        T.AllFinish
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
    @
-}


{-# LANGUAGE TemplateHaskellQuotes #-}


module Test.Tasty.HIOUnit
    (
        testCase,
        IOTestParameter(..),
        IOFunctionDefinition(..),
        sMultiLine
    ) where


import qualified Test.Tasty.Internal.DynamicIOFunction as Dyn
import qualified Test.Tasty.Internal.ShellExecute as Sh
import qualified Test.Tasty.Internal.Safer as Sfr

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

import qualified Language.Haskell.TH.Quote as THQ
import qualified Data.String as Str


-- IOTestParameter
-- | ...parameter to define input for 'testCase' and expected results
{- |
* prefix: iotParameter
-}
data IOTestParameter =
    IOTestParameter {
        -- | may be a string to to inject as *stdin* stream into
        riotParameterStdIn :: Maybe String,
        -- | Haskell function definition
        riotIOFunctionDefinition :: IOFunctionDefinition,
        -- | function verify stdout as test result, or 'Nothing' for no expectations regarding stdout
        riotParameterStdOut :: Maybe String,
        -- | function verify stderr as test result, or 'Nothing' for no expectations regarding stderr
        riotParameterStdErr :: Maybe String,
        -- | function verify exit code as test result, or 'Nothing' for no expectations regarding exit code
        riotParameterExitCode :: Maybe Int
    }

-- IOFunctionDefinition
-- | ...parameter to define input for 'testCase' and expected results.
{- |
* prefix: fdef
-}
data IOFunctionDefinition =
    IOFunctionDefinition {
        -- | module name, has to start with upper case letter
        rsModuleName :: String,
        -- | name of funcion that has to exported and executed
        rmsFunctionNameToExportAndExecute :: Maybe String,
        -- | string containing the haskell source code, without module and export list declaration
        rsHaskellSourceCode :: String
    }

-- testCase
{-| ...creates a test within a test tree.

example:

    @
HIOU.testCase "Fib.ioFib -2" (HIOU.IOTestParameter ( Just "-2" ) fdefFib ( Just "-1" ) ( Just "" ) ( Just 0 )), 
    @

-}
testCase
    -- | name of test case
    :: String
    -- | parameter to define input, code to be executed and expected results
    -> IOTestParameter
    -- | the resulting item to be inserted in a test tree
    -> T.TestTree
testCase nameOfTestCase iotestParameter =
    HU.testCase
        nameOfTestCase
        (assertFromIOTestParameter iotestParameter)
    where
        assertFromIOTestParameter :: IOTestParameter -> HU.Assertion
        assertFromIOTestParameter (IOTestParameter msStdIn iotIOFunctionDefinition msStdOut msStdErr mnExitCode) =
            do
                done <- Dyn.writeSourceCodeFileIfNecessary
                    "./.iotest/"
                    (rsModuleName iotIOFunctionDefinition)
                    [ sFunctionNameToExportAndExecute iotIOFunctionDefinition ]
                    (rsHaskellSourceCode iotIOFunctionDefinition)
                executionResult <-
                    done
                    `seq`
                    Sh.runCommandWithIO
                        Nothing
                        Nothing
                        "hdynexec"
                        ["-hs", "./.iotest/", rsModuleName iotIOFunctionDefinition, sFunctionNameToExportAndExecute iotIOFunctionDefinition]
                        msStdIn
                Sfr.ifJust
                    executionResult
                    (assertionFromExecutionResult msStdOut msStdErr mnExitCode)
                    (HU.assertFailure ("Did not execute module under test \"" ++ rsModuleName iotIOFunctionDefinition ++ "\"!"))
        sFunctionNameToExportAndExecute :: IOFunctionDefinition -> String
        sFunctionNameToExportAndExecute iotIOFunctionDefinition =
            Sfr.ifJust
                (rmsFunctionNameToExportAndExecute iotIOFunctionDefinition)
                -- then
                id
                -- else
                "main"
        assertionFromExecutionResult :: Maybe String -> Maybe String -> Maybe Int -> (String, String, Int) -> HU.Assertion
        assertionFromExecutionResult msStdOut msStdErr mnExitCode (sActualStdOut, sActualStdErr, nActualExitCode) =
            do
                Sfr.ifJust
                    msStdOut
                    (\sStdOutExpected -> HU.assertEqual "stdout" sStdOutExpected sActualStdOut)
                    (return ())
                Sfr.ifJust
                    msStdErr
                    (\sStdErrExpected -> HU.assertEqual "stderr" sStdErrExpected sActualStdErr)
                    (return ())
                Sfr.ifJust
                    mnExitCode
                    (\nExitCodeExpected -> HU.assertEqual "exit code" nExitCodeExpected nActualExitCode)
                    (return ())

-- sMultiLine
{- | ...QuasiQuoter for a non-interpolating IsString literal. The pattern portion is undefined.

* can be used to define multiline strings embedded in haskell code

Example:

The following code defines a multi line string.

    @
{-# LANGUAGE QuasiQuotes #-}

...

import qualified Test.Tasty.HIOUnit as HIOU

...

sourceCode = 
    [HIOU.sMultiLine|
main :: IO ()
main = putStrLn "Hello..."
|]
    @

The code equals:

    @
sourceCode = "main :: IO ()\nmain = putStrLn "Hello..."\n"
    @

-}
sMultiLine :: THQ.QuasiQuoter
sMultiLine = THQ.QuasiQuoter
        ((\a -> [|Str.fromString a|]) . trimLeadingNewline . removeCRs)
        (error "Cannot use q as a pattern")
        (error "Cannot use q as a type")
        (error "Cannot use q as a dec")
    where
        removeCRs :: String -> String
        removeCRs = filter (/= '\r')
        trimLeadingNewline :: String -> String
        trimLeadingNewline ('\n':xs) = xs
        trimLeadingNewline xs = xs
