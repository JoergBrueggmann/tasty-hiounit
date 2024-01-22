

import qualified Test.Tasty.Internal.DynamicIOFunction as Dyn

import qualified System.Environment as Env


main :: IO ()
main = 
    do
        sArgs <- Env.getArgs
        processArgs sArgs

processArgs :: [String] -> IO ()
processArgs ["-hs", sHaskellFolder, sHaskellModule, sHaskellFunctionToExecute] = Dyn.executeFromFile sHaskellFolder sHaskellModule (Just sHaskellFunctionToExecute) False
processArgs ["--echo"] = echo
processArgs ["--help"] = printHelp
processArgs sArgs = printParameterError sArgs >> printHelp

printParameterError :: [String] -> IO ()
printParameterError sArgs = 
    do
        putStrLn ""
        putStrLn "Error: Parameter wrong."
        putStrLn ""
        putStrLn "Parameters:"
        printNumberedList "       " 1 sArgs
        putStrLn ""

printNumberedList :: Show a => String -> Integer -> [a] -> IO ()
printNumberedList _ _ [] = return ()
printNumberedList indent n (value:lrvalue) = putStr indent >> putStr (show n) >> putStr ": " >> putStrLn (show value) >> printNumberedList indent ( n + 1 ) lrvalue

printHelp :: IO ()
printHelp = 
    do
        putStrLn "NAME"
        putStrLn "       hdynexec"
        putStrLn ""
        putStrLn "SYNOPSIS"
        putStrLn "       hdynexec ( -hs <HaskellFolder> <HaskellModule> <HaskellIOFunctionToExecute> | --help | --echo )"
        putStrLn ""
        putStrLn "DESCRIPTION"
        putStrLn "       -hs <HaskellFolder> <HaskellModule> <HaskellIOFunctionToExecute>"
        putStrLn "              ...compiles and loads the <HaskellModule> in <HaskellFolder> and executes <HaskellIOFunctionToExecute>."
        putStrLn ""
        putStrLn "       --help"
        putStrLn "              ...this help."
        putStrLn ""
        putStrLn "       --echo"
        putStrLn "              ...just echos the input, for debugging."
        putStrLn ""
        putStrLn "AUTHOR"
        putStrLn "       Written by Jörg Karl-Heinz Walter Brüggmann."
        putStrLn ""
        putStrLn "REPORTING BUGS"
        putStrLn "       GitHub: <https://github.com/JoergBrueggmann>"
        putStrLn ""
        putStrLn "COPYRIGHT"
        putStrLn ""
        putStrLn "    Copyright © 2023 Jörg Karl-Heinz Walter Brüggmann."
        putStrLn ""
        putStrLn "    This program is free software: you can redistribute it and/or modify"
        putStrLn "    it under the terms of the GNU General Public License as published by"
        putStrLn "    the Free Software Foundation, either version 3 of the License, or"
        putStrLn "    (at your option) any later version."
        putStrLn ""
        putStrLn "    This program is distributed in the hope that it will be useful,"
        putStrLn "    but WITHOUT ANY WARRANTY; without even the implied warranty of"
        putStrLn "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
        putStrLn "    GNU General Public License for more details."
        putStrLn ""
        putStrLn "    You should have received a copy of the GNU General Public License in"
        putStrLn "    file 'LICENSE' along with this program. If not, see"
        putStrLn "    <https://www.gnu.org/licenses/>."

echo :: IO ()
echo = 
    do
        sStdIn <- getContents
        putStr sStdIn
