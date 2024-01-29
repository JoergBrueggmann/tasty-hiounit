{-|
Description : ... dynamically create source code files, compile and load them.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Test.Tasty.Internal.DynamicIOFunction' provides helper functions to  create source code files, compile and load them, dynamically.

Suggested import line:

    @
import qualified Test.Tasty.Internal.DynamicIOFunction as Dyn
    @
-}


{-# LANGUAGE BangPatterns #-}


module Test.Tasty.Internal.DynamicIOFunction
    (
        executeFromString,
        executeFromFile,
        writeSourceCodeFileIfNecessary,
        deleteSourceCodeFile
    ) where


import qualified Test.Tasty.Internal.GhcApiWrap as Ghc

import qualified System.IO as SIo
import qualified System.Directory as Dir
import qualified Data.Maybe as M
import qualified Control.Monad
import qualified Control.Exception.Base as Ex


-- TODO: Add documentation
executeFromFile :: String -> String -> Maybe String -> Bool -> IO ()
executeFromFile folderPath moduleName maybeFunctionName doPreserveFiles =
    do
        eitherFunctionOrError <- Ghc.compileAndLoad folderPath moduleName functionName
        case eitherFunctionOrError of
            (Right function) ->
                function
            (Left sErrorMassage) ->
                putStrLn ("Error: " ++ sErrorMassage)
        case eitherFunctionOrError of
            (Right _)    -> deleteBuiltFiles
            _                   -> return ()
    where
        functionName = M.fromMaybe "main" maybeFunctionName
        deleteBuiltFiles :: IO ()
        deleteBuiltFiles =
            Control.Monad.when doPreserveFiles
                (
                    do
                        Dir.removeFile (folderPath ++ moduleName ++ ".hi")
                        Dir.removeFile (folderPath ++ moduleName ++ ".o")
                )

-- TODO: Add documentation
executeFromString :: Maybe String -> Maybe String -> [ String ] -> String -> Maybe String -> Bool -> IO ()
executeFromString maybeFolderPath maybeModuleName lExportElements !sourceCodeWithoutModuleDeclaration maybeFunctionName doPreserveFiles =
    do
        done <- writeSourceCodeFileIfNecessary folderPath moduleName lExportElements sourceCodeWithoutModuleDeclaration
        done `seq` (
            do
                executeFromFile folderPath moduleName maybeFunctionName doPreserveFiles
                deleteSourceCodeFile folderPath moduleName)
    where
        folderPath = M.fromMaybe "./dyn/" maybeFolderPath
        moduleName = M.fromMaybe "Temp" maybeModuleName

-- writeSourceCodeFileIfNecessary
{- | ...writes a source code file.

* creates a new file if it doesnt exist and can be created
* modifies an existing file if it would lead to a change
-}
writeSourceCodeFileIfNecessary 
    -- | path of folder, ending with '/' (or '\' for windows
    :: String 
    -- | name of module, without file extension
    -> String 
    -- | elements to be exported and list in an automaticalls generated export list
    -> [ String ] 
    -- | source without module and export declaration
    -> String 
    -- | indication whether the function has been executed successfully
    -> IO Bool
writeSourceCodeFileIfNecessary folderPath moduleName lExportElements sourceCodeWithoutModuleDeclaration =
    do
        Dir.createDirectoryIfMissing True folderPath
        exists <- Dir.doesFileExist sFilePath
        if exists
            then do
                isNecessary <- modificationNecessary
                if isNecessary
                    then writeSourceCode
                    else return True
            else writeSourceCode
    where
        modificationNecessary :: IO Bool
        modificationNecessary = readFile sFilePath >>= return . (sourceCode /=)
        writeSourceCode :: IO Bool
        writeSourceCode = 
            (
                do
                    handle <- SIo.openFile sFilePath SIo.WriteMode
                    SIo.hPutStr handle sourceCode
                    SIo.hClose handle
                    return True
            )
            `Ex.catch`
            (\e ->
                do
                    SIo.hPutStrLn SIo.stderr ("Error: In writeSourceCodeFileIfNecessary/writeSourceCode, " ++ show (e :: Ex.SomeException))
                    return False)
        sourceCode :: String
        sourceCode = 
            "module " ++ 
            moduleName ++ 
            sExportList lExportElements ++ 
            " where\n\n" ++ 
            sourceCodeWithoutModuleDeclaration ++ "\n"
        sFilePath = folderPath ++ moduleName ++ ".hs"
        sExportList :: [ String ] -> String
        sExportList [] = ""
        sExportList lExportElements' = "\n    (\n" ++ sExportList' lExportElements' ++ "    )"
        sExportList' :: [ String ] -> String
        sExportList' [] = ""
        sExportList' ( exportElement : exportElement2 : lrExportElements ) = "        " ++ exportElement ++ ", \n" ++ sExportList' (exportElement2 : lrExportElements)
        sExportList' [exportElement] = "        " ++ exportElement ++ "\n"

-- TODO: Add documentation
deleteSourceCodeFile ::String -> String -> IO ()
deleteSourceCodeFile folderPath moduleName = 
    do
        Dir.removeFile (folderPath ++ moduleName ++ ".hs")
