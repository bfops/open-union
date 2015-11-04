-- | Flexible, type-safe open unions.
module Data.OpenUnion
    ( Union
    , (@>)
    , liftUnion
    , reUnion
    , restrict
    , typesExhausted
    ) where

import Data.OpenUnion.Internal
