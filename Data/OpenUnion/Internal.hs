-- | Exposed internals for Data.OpenUnion
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.OpenUnion.Internal
    ( Union (..)
    , (:<)
    , (:\)
    , (@>)
    , (@!>)
    , liftUnion
    , reUnion
    , restrict
    , typesExhausted
    ) where

import Data.Dynamic
import Data.Type.Bool
import GHC.Exts

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
-- class (:<) (s :: [*]) (s' :: [*])

type (:<) s s' =
  ( SubList s s'
  , AllSatisfy Typeable s
  , AllSatisfy Typeable s')

class NotASubListOf s s'

type SubList s s' = If (SubListB s s')
                       (() :: Constraint)
                       (NotASubListOf s s')

type family SubListB (s :: [*]) (s' :: [*]) :: Bool where
  SubListB '[] s'      = 'True
  SubListB (a ': as) s = (ElemB a s) && (SubListB as s)

class ElementNotFound a s

-- type Elem a s = DropFalseErr (ElemB a s) (ElementNotFound a s)
type Elem a s = If (ElemB a s)
                   (() :: Constraint)
                   (ElementNotFound a s)

type family ElemB (a :: *) (s :: [*]) :: Bool where
  ElemB a '[]       = 'False
  ElemB a (a ': as) = 'True
  ElemB a (b ': bs) = ElemB a bs

type family AllSatisfy (const :: k -> Constraint) (s :: [k]) :: Constraint where
  AllSatisfy c '[]       = ()
  AllSatisfy c (a ': as) = (c a, AllSatisfy c as)

-- | `restrict` in right-fixable style.
(@>) :: Typeable a => (a -> b) -> (Union (s :\ a) -> b) -> Union s -> b
r @> l = either l r . restrict
infixr 2 @>
{-# INLINE (@>) #-}

(@!>) :: (Typeable a, Elem a s)
      => (a -> b)
      -> (Union (s :\ a) -> b)
      -> Union s -> b
r @!> l = either l r . restrict
infixr 2 @!>

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
