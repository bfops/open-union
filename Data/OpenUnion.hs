-- | Flexible, type-safe open unions.
module Data.OpenUnion
    ( Union
    , (@>)
    , liftUnion
    , reUnion
    , flatUnion
    , flattenUnion
    , restrict
    , typesExhausted
    ) where

import Data.OpenUnion.Internal
