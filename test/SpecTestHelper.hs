
{-|
Description : Helper functions for test.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}


module SpecTestHelper
    (
        Int256Plus, 
        njFromInt256Plus, 
        niFromInt256Plus
    ) where


import qualified Test.Tasty.QuickCheck as QC


newtype Int256Plus = Int256Plus Int
    deriving Show

instance QC.Arbitrary Int256Plus where
    arbitrary =
        do
            nj <- QC.frequency
                [
                    -- 
                    (1, QC.choose (-100, -1)),
                    -- 
                    (1, QC.choose (0, 255)),
                    -- 
                    (1, QC.choose (256, 355))
                ]
            return (Int256Plus nj)

njFromInt256Plus :: Int256Plus -> Int
njFromInt256Plus (Int256Plus nj) = nj

niFromInt256Plus :: Int256Plus -> Integer
niFromInt256Plus (Int256Plus nj) = fromIntegral nj
