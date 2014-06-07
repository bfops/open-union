{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.OpenUnion.Internal
    ( Union (..)
    , LiftToUnion (..)
    , Subset
    , (:|)
    , (:\)
    , (@>)
    , reunion
    , restrict
    , typesExhausted
    ) where

import Data.Dynamic
import Data.Void

-- | Heterogenous list
data a :| s
infixr 8 :|

-- | The @Union@ type - its parameter is a list of types (using `(:|)`),
-- denoting what this @Union@ might contain. It MUST be one of those types.
newtype Union s = Union Dynamic

-- general note: try to keep from re-constructing Unions if an existing one
-- can just be type-coerced.

class Typeable a => LiftToUnion s a where
  liftUnion :: a -> Union s
  liftUnion = Union . toDyn
  {-# INLINE liftUnion #-}

instance Typeable a => LiftToUnion (a :| s) a
instance LiftToUnion s a => LiftToUnion (a' :| s) a

type family s :\ a where
    Void :\ a = Void
    (a :| s) :\ a = s :\ a
    (a' :| s) :\ a = a' :| (s :\ a)

class Subset s s'
instance Subset Void s
instance Subset s s
instance (Subset s s', LiftToUnion s' a) => Subset (a :| s) s'

-- | `restrict` in continuation passing style.
(@>) :: Typeable a => (a -> b) -> (Union (s :\ a) -> b) -> Union s -> b
r @> l = either l r . restrict
infixr 2 @>
{-# INLINE (@>) #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (s :\ a)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reunion :: Subset s s' => Union s -> Union s'
reunion (Union d) = Union d
{-# INLINE reunion #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union Void -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
