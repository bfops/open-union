-- | Flexible, type-safe open unions.
module Data.OpenUnion
    ( Union
    , (@>)
    , liftUnion
    , reUnion
    , flattenUnion
    , restrict
    , typesExhausted
    ) where

import Data.OpenUnion.Internal
