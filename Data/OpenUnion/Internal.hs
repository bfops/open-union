-- | Exposed internals for Data.OpenUnion
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.OpenUnion.Internal
    ( Union (..)
    , (@>)
    , (@!>)
    , liftUnion
    , reUnion
    , restrict
    , typesExhausted
    ) where

import Data.Dynamic
import TypeFun.Data.List

-- | The @Union@ type - the phantom parameter @s@ is a list of types
-- denoting what this @Union@ might contain.
-- The value contained is one of those types.
newtype Union (s :: [*]) = Union Dynamic

instance Show (Union '[]) where
  show = typesExhausted

instance (Show a, Show (Union (Delete a as)), Typeable a)
         => Show (Union (a ': as)) where
  show u = case restrict u of
    Left sub       -> show sub
    Right (a :: a) ->
       let p = Proxy :: Proxy a
           rep = typeRep p
       in "Union (" ++ show a ++ " :: " ++ show rep ++ ")"

instance Eq (Union '[]) where
  a == _ = typesExhausted a

instance (Typeable a, Eq (Union (Delete a as)), Eq a)
         => Eq (Union (a ': as)) where
  u1 == u2 =
    let r1 = restrict u1
        r2 = restrict u2
    in case (r1, r2) of
       (Right (a :: a), Right (b :: a)) -> a == b
       (Left  a       , Left  b)        -> a == b
       _                                -> False

-- general note: try to keep from re-constructing Unions if an existing one
-- can just be type-coerced.

-- | `restrict` in right-fixable style.
(@>) :: Typeable a
     => (a -> b)
     -> (Union (Delete a s) -> b)
     -> Union s
     -> b
r @> l = either l r . restrict
infixr 2 @>
{-# INLINE (@>) #-}

-- | `restrict` in right-fixable style with existance restriction.
(@!>) :: (Typeable a, Elem a s)
      => (a -> b)
      -> (Union (Delete a s) -> b)
      -> Union s
      -> b
r @!> l = either l r . restrict
infixr 2 @!>
{-# INLINE (@!>) #-}

liftUnion :: (Typeable a, Elem a s) => a -> Union s
liftUnion = Union . toDyn
{-# INLINE liftUnion #-}

-- | Narrow down a @Union@.
restrict :: Typeable a => Union s -> Either (Union (Delete a s)) a
restrict (Union d) = maybe (Left $ Union d) Right $ fromDynamic d
{-# INLINE restrict #-}

-- | Generalize a @Union@.
reUnion :: (SubList s s') => Union s -> Union s'
reUnion (Union d) = Union d
{-# INLINE reUnion #-}

-- | Use this in places where all the @Union@ed options have been exhausted.
typesExhausted :: Union '[] -> a
typesExhausted = error "Union types exhausted - empty Union"
{-# INLINE typesExhausted #-}
