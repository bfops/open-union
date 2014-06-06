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
    , Restrict (..)
    , Subset
    , (:|) (..)
    , (:\)
    , (@>)
    , reunion
    , restrict
    , restrictEntirely
    , typesExhausted
    ) where

import Data.Dynamic
import Data.Void

-- | Heterogenous list
data a :| s = a :| s
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

instance Typeable a => LiftToUnion a a
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

infixr 2 @>
-- | `restrict` in continuation passing style.
(@>) :: Typeable a => (a -> b) -> (Union (s :\ a) -> b) -> Union s -> b
r @> l = either l r . restrict
{-# INLINE (@>) #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (s :\ a)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reunion :: Subset s s' => Union s -> Union s'
reunion (Union d) = Union d
{-# INLINE reunion #-}

-- | Narrow down a @Union@ like `restrict`, but several times at once.
class Restrict s h h' b | h -> h' where -- technically also h -> b and b h' -> h
  type s :\\ h'
  -- | Narrow down a @Union@ like `restrict`, but several times at once.
  restrictMany :: h -> Union s -> Either (Union (s :\\ h')) b

instance Restrict s () Void b where
  type s :\\ Void = s
  restrictMany _ = Left
  {-# INLINE restrictMany #-}

instance (Typeable a, Restrict (s :\ a) hs h' b) => Restrict s ((a -> b) :| hs) (a :| h') b where
  type s :\\ (a :| h') = (s :\ a) :\\ h'
  restrictMany (h :| hs) = either (restrictMany hs) (Right . h) . restrict
  {-# INLINE restrictMany #-}

-- | Given an exhaustive set of functions to generate values from a @Union@, return the value generated.
restrictEntirely :: (Restrict s h h' b, (s :\\ h') ~ Void) => h -> Union s -> b
restrictEntirely h = either typesExhausted id . restrictMany h
{-# INLINE restrictEntirely #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union Void -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
