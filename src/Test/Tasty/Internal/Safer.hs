{-|
Description : provides classes and functions helping to reduce runtime errors.
Copyright   : (c) Jörg Karl-Heinz Walter Brüggmann, 2021-2024
License     : BSD-3-Clause
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Safer provides classes and functions helping to to reduce runtime errors.

-}


module Test.Tasty.Internal.Safer (
        ifJust, 
        ifJust2, 
        ifJust3
    ) where



ifJust :: Maybe a -> (a -> b) -> b -> b
ifJust (Just x) then' _ = then' x
ifJust Nothing _ bDflt = bDflt

ifJust2 :: Maybe a -> Maybe b -> (a -> b -> c) -> c -> c
ifJust2 (Just x1) (Just x2) then' _ = then' x1 x2
ifJust2 Nothing _ _ bDflt = bDflt
ifJust2 _ Nothing _ bDflt = bDflt

ifJust3 :: Maybe a -> Maybe b -> Maybe c -> (a -> b -> c -> d) -> d -> d
ifJust3 (Just x1) (Just x2) (Just x3) then' _ = then' x1 x2 x3
ifJust3 Nothing _ _ _ bDflt = bDflt
ifJust3 _ Nothing _ _ bDflt = bDflt
ifJust3 _ _ Nothing _ bDflt = bDflt
