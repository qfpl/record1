{-# language FlexibleContexts #-}
{-# language FlexibleInstances, UndecidableInstances #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language DataKinds, KindSignatures, PolyKinds #-}
{-# language MagicHash #-}
{-# language ScopedTypeVariables, TypeApplications #-}
{-# language InstanceSigs, TypeOperators #-}
{-# language RoleAnnotations #-}
module Data.Record1.Internal where

import Data.Functor.Identity (Identity(..))
import Data.Functor.Classes (Show1(..), showsPrec1)
import Data.Generics.Fixplate.Base (ShowF(..))
import Data.Vector (Vector)
import GHC.Base (Any)
import GHC.Exts (Proxy#, proxy#)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as Vector

newtype Record1 (fs :: [k -> *]) (a :: k)
  = Record1 { unRecord1 :: Vector Any }
type role Record1 representational representational

{-# inline emptyR1 #-}
emptyR1 :: Record1 '[] a
emptyR1 = Record1 Vector.empty

{-# inline shrinkR1 #-}
shrinkR1 :: Record1 (a ': as) x -> Record1 as x
shrinkR1 (Record1 r) = Record1 $ Vector.unsafeTail r

{-# inline trivialR1 #-}
trivialR1 :: a -> Record1 '[] b
trivialR1 _ = emptyR1

instance Functor (Record1 '[]) where
  {-# inline fmap #-}
  fmap _ = trivialR1

instance (Functor f, Functor (Record1 fs)) => Functor (Record1 (f ': fs)) where
  {-# inline fmap #-}
  fmap f = introR1 (fmap f . headR1) (fmap f . shrinkR1)

instance Foldable (Record1 '[]) where
  {-# inline foldMap #-}
  foldMap _ = mempty

instance (Foldable f, Foldable (Record1 fs)) => Foldable (Record1 (f ': fs)) where
  {-# inline foldMap #-}
  foldMap f r = foldMap f (headR1 r) <> foldMap f (shrinkR1 r)

instance Traversable (Record1 '[]) where
  {-# inline traverse #-}
  traverse _ = pure . trivialR1

instance
  (Traversable f, Traversable (Record1 fs)) => Traversable (Record1 (f ': fs)) where
  {-# inline traverse #-}
  traverse f = introR1M (traverse f . headR1) (traverse f . shrinkR1)

instance Show1 (Record1 vs) => ShowF (Record1 vs) where
  {-# inline showsPrecF #-}
  showsPrecF = showsPrec

instance Show1 (Record1 '[]) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec _ _ _ _ = showString "RNil"

instance (Show1 f, Show1 (Record1 fs)) => Show1 (Record1 (f ': fs)) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec a b c r =
    showParen (c > 10) $
    showString "RCons " .
    liftShowsPrec a b 11 (headR1 r) .
    showString " " .
    liftShowsPrec a b 11 (shrinkR1 r)

instance (Show1 (Record1 vs), Show a) => Show (Record1 vs a) where
  {-# inline showsPrec #-}
  showsPrec = showsPrec1

{-# inline introR1M #-}
introR1M
  :: Applicative m
  => (r -> m (a x))
  -> (r -> m (Record1 as x))
  -> r -> m (Record1 (a ': as) x)
introR1M f g r =
  fmap Record1 $
  Vector.cons <$>
  unsafeCoerce (f r) <*>
  fmap unRecord1 (g r)

{-# inline introR1 #-}
introR1
  :: (r -> a x)
  -> (r -> Record1 as x)
  -> r -> Record1 (a ': as) x
introR1 f g = runIdentity . introR1M (Identity . f) (Identity . g)

class Field1 as g | as -> g where
  offset1 :: Proxy# as -> Proxy# g -> Int

instance {-# overlapping #-} Field1 (f ': fs) f where
  {-# inline offset1 #-}
  offset1 _ _ = 0

instance {-# overlappable #-} Field1 fs g => Field1 (f ': fs) g where
  {-# inline offset1 #-}
  offset1 :: forall f fs g. Field1 fs g => Proxy# (f ': fs) -> Proxy# g -> Int
  offset1 _ g = 1 + offset1 (proxy# :: Proxy# fs) g

{-# inline headR1 #-}
headR1 :: Record1 (f ': fs) a -> f a
headR1 = unsafeCoerce . Vector.unsafeHead . unRecord1 

{-# inline get1 #-}
get1 :: forall g fs a. Field1 fs g => Record1 fs a -> g a
get1 (Record1 r) =
  unsafeCoerce $
  Vector.unsafeIndex r $ offset1 (proxy# :: Proxy# fs) (proxy# :: Proxy# g)

{-# inline set1 #-}
set1 :: forall g fs a. Field1 fs g => Record1 fs a -> g a -> Record1 fs a
set1 (Record1 arr) a =
  Record1 $
  Vector.unsafeUpdate arr $
  pure (offset1 (proxy# :: Proxy# fs) (proxy# :: Proxy# g), unsafeCoerce a)
