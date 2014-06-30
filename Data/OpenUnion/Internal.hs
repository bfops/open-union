-- | Exposed internals for Data.OpenUnion
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.OpenUnion.Internal
    ( Union (..)
    , (:<)
    , (:\)
    , (@>)
    , liftUnion
    , reUnion
    , restrict
    , typesExhausted
    ) where

import Data.Dynamic

-- | The @Union@ type - the phantom parameter @s@ is a list of types
-- denoting what this @Union@ might contain.
-- The value contained is one of those types.
newtype Union (s :: [*]) = Union Dynamic

-- general note: try to keep from re-constructing Unions if an existing one
-- can just be type-coerced.

-- | Remove a type from anywhere in the list.
type family s :\ a where
    '[] :\ a = '[]
    (a ': s) :\ a = s :\ a
    (a' ': s) :\ a = a' ': (s :\ a)

-- | There exists a @s :< s'@ instance if every type in the list @s@
-- can be lifted to @s'@.
class (:<) (s :: [*]) (s' :: [*])
instance '[] :< s
instance (s :< s', Typeable a) => (a ': s) :< (a ': s')
instance (s :< s', '[a] :< s', Typeable a) => (a ': s) :< s'

-- | `restrict` in right-fixable style.
(@>) :: Typeable a => (a -> b) -> (Union (s :\ a) -> b) -> Union s -> b
r @> l = either l r . restrict
infixr 2 @>
{-# INLINE (@>) #-}

liftUnion :: (Typeable a, '[a] :< s) => a -> Union s
liftUnion = Union . toDyn
{-# INLINE liftUnion #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (s :\ a)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reUnion :: (s :< s') => Union s -> Union s'
reUnion (Union d) = Union d
{-# INLINE reUnion #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
