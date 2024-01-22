{-|
Description : provides functions to dynamically execute test where IO like stdin, stdout, and stderr is involved.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
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


-- import qualified Debug.Trace as Dbg -- TODO: Remove in final version. Remove comment begin to use 'trace'


-- IOTestParameter
-- | ...parameter to define input for 'test' and expected results.
{- |
* prefix: iotParameter
-}
data IOTestParameter =
    IOTestParameter {
        -- | may be a string to to inject as 'stdin' stream into
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
-- | ...parameter to define input for 'test' and expected results.
{- |
* prefix: fdef
-}
data IOFunctionDefinition = 
    IOFunctionDefinition {
        -- | module name, has to start with upper case letter
        rsModuleName :: String, 
        rmsFunctionNameToExportAndExecute :: Maybe String, 
        rsHaskellSourceCode :: String
    }

-- testCase
{-| ...creates a test within a test tree.
-}
testCase
    :: String  -- ^ name of test case
    -> IOTestParameter  -- ^ parameter to define input, code to be executed and expected results
    -> T.TestTree
testCase nameOfTestCase iotestParameter = 
    HU.testCase
        nameOfTestCase
        (assertFromIOTestParameter iotestParameter)
    where
        assertFromIOTestParameter :: IOTestParameter -> HU.Assertion
        assertFromIOTestParameter (IOTestParameter msStdIn iotIOFunctionDefinition msStdOut msStdErr mnExitCode) =
            do
                Dyn.writeSourceCodeFileIfNecessary
                    "./.iotest/"
                    (rsModuleName iotIOFunctionDefinition)
                    [ sFunctionNameToExportAndExecute iotIOFunctionDefinition ]
                    (rsHaskellSourceCode iotIOFunctionDefinition)
                executionResult <- 
                    Sh.runCommandWithIO
                        Nothing
                        Nothing
                        "hdynexec"
                        ["-hs", "./.iotest/", rsModuleName iotIOFunctionDefinition, sFunctionNameToExportAndExecute iotIOFunctionDefinition]
                        msStdIn
                Sfr.ifJust
                    executionResult
                    (assertionFromExecutionResult msStdOut msStdErr mnExitCode)
                    (HU.assertFailure ("Did not execute module under test \"" ++ (rsModuleName iotIOFunctionDefinition) ++ "\"!"))
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
