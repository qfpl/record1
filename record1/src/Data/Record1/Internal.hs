{-# language BangPatterns #-}
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

class MapRecord1 (fs :: [k -> *]) where
  mapRecord1 :: Proxy# fs -> (a -> b) -> Int -> Any -> Any

instance MapRecord1 '[] where
  {-# inline mapRecord1 #-}
  mapRecord1 _ _ _ = undefined

instance (Functor f, MapRecord1 fs) => MapRecord1 (f ': fs) where
  {-# inline mapRecord1 #-}
  mapRecord1 _ f !0 = unsafeCoerce (fmap @f f)
  mapRecord1 _ f !n = mapRecord1 (proxy# :: Proxy# fs) f (n-1)

instance MapRecord1 fs => Functor (Record1 fs) where
  {-# inline fmap #-}
  fmap f (Record1 r) =
    Record1 $ Vector.imap (mapRecord1 (proxy# :: Proxy# fs) f) r

class FoldRecord1 (fs :: [k -> *]) where
  foldrRecord1 :: Proxy# fs -> (a -> b -> b) -> Int -> Any -> b -> b

instance FoldRecord1 '[] where
  {-# inline foldrRecord1 #-}
  foldrRecord1 _ _ _ = undefined

instance (Foldable f, FoldRecord1 fs) => FoldRecord1 (f ': fs) where
  {-# inline foldrRecord1 #-}
  foldrRecord1 _ f !0 = flip (foldr @f f) . unsafeCoerce
  foldrRecord1 _ f !n = foldrRecord1 (proxy# :: Proxy# fs) f (n-1)

instance FoldRecord1 fs => Foldable (Record1 fs) where
  {-# inline foldr #-}
  foldr f z (Record1 r) =
    Vector.ifoldr (foldrRecord1 (proxy# :: Proxy# fs) f) z r

{-# inline itraverseVector #-}
itraverseVector
  :: Applicative f
  => (Int -> a -> f b)
  -> Vector a
  -> f (Vector b)
itraverseVector f = fmap Vector.fromList . itraverseList f . Vector.toList
  where
    {-# inline itraverseList #-}
    itraverseList
      :: Applicative f
      => (Int -> a -> f b)
      -> [a]
      -> f [b]
    itraverseList f = go 0
      where
        go !_ [] = pure []
        go !n (x:xs) = (:) <$> f n x <*> go (n+1) xs

class (MapRecord1 fs, FoldRecord1 fs) => TraverseRecord1 (fs :: [k -> *]) where
  traverseRecord1
    :: Applicative f
    => Proxy# fs
    -> (a -> f b)
    -> Int -> Any -> f Any

instance TraverseRecord1 '[] where
  {-# inline traverseRecord1 #-}
  traverseRecord1 _ _ _ = undefined

instance (Traversable f, TraverseRecord1 fs) => TraverseRecord1 (f ': fs) where
  {-# inline traverseRecord1 #-}
  traverseRecord1 _ f !0 = unsafeCoerce (traverse @f f)
  traverseRecord1 _ f !n = traverseRecord1 (proxy# :: Proxy# fs) f (n-1)

instance (TraverseRecord1 fs) => Traversable (Record1 fs) where
  {-# inline traverse #-}
  traverse f (Record1 r) =
    Record1 <$>
    itraverseVector (traverseRecord1 (proxy# :: Proxy# fs) f) r

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

class Field1 (fs :: [k -> *]) (g :: k -> *) | fs -> g where
  offset1 :: Proxy# fs -> Proxy# g -> Int

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
