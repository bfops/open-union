-- | Exposed internals for Data.OpenUnion
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.OpenUnion.Internal
    ( Union (..)
    , LiftToUnion (..)
    , (:<)
    , (:\)
    , (@>)
    , reunion
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

-- | Typeclass for lifting values to @Union@s.
class Typeable a => LiftToUnion (s :: [*]) a where
  liftUnion :: a -> Union s
  liftUnion = Union . toDyn
  {-# INLINE liftUnion #-}

instance Typeable a => LiftToUnion (a ': s) a
instance LiftToUnion s a => LiftToUnion (a' ': s) a

-- | Remove a type from anywhere in the list.
type family s :\ a where
    '[] :\ a = '[]
    (a ': s) :\ a = s :\ a
    (a' ': s) :\ a = a' ': (s :\ a)

-- | There exists a @s :< s'@ instance if every type in the list @s@
-- has a @`LiftToUnion` s'@ instance.
class (:<) (s :: [*]) (s' :: [*])
instance '[] :< s
instance (s :< s', LiftToUnion s' a) => (a ': s) :< s'

-- | `restrict` in right-fixable style.
(@>) :: Typeable a => (a -> b) -> (Union (s :\ a) -> b) -> Union s -> b
r @> l = either l r . restrict
infixr 2 @>
{-# INLINE (@>) #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (s :\ a)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reunion :: (s :< s') => Union s -> Union s'
reunion (Union d) = Union d
{-# INLINE reunion #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
