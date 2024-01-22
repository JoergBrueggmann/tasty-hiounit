{-|
Description : test to demonstrate module 'Test.Tasty.HIOUnit'.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}


import qualified Test.Tasty as T

import qualified SpecHIOUnit


main :: IO ()
main = 
    T.defaultMain tests

tests :: T.TestTree
tests = 
    T.testGroup 
        "Tests" 
        [
            SpecHIOUnit.testGroup
        ]
