{-# language DataKinds #-}
{-# language TypeApplications #-}
module Main where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Foldable (traverse_)
import Data.Traversable (fmapDefault)

import Data.Record1

{-# noinline r1 #-}
r1 :: Record1 [Product Identity Identity, Const String, Identity] Int
r1 =
  introR1 (\_ -> Pair (Identity 2) (Identity 3)) id $
  introR1 (\_ -> Const "") id $
  introR1 (\_ -> Identity 1) id $
  emptyR1

{-# noinline r2 #-}
r2 :: Record1 [Product Identity Identity, Const String, Identity] Int
r2 = fmap (+10000) r1

{-# noinline r3 #-}
r3 :: Record1 [Identity, Product Identity Identity, Identity] Int
r3 =
  introR1 (\_ -> Identity 1) id $
  introR1 (\_ -> Pair (Identity 2) (Identity 3)) id $
  introR1 (\_ -> Identity 4) id $
  emptyR1

{-# noinline r4 #-}
r4 :: Record1 [Product Identity Identity, Const String, Identity] Int
r4 = fmapDefault (+10000) r1

main :: IO ()
main = do
  print $ get1 @Identity r2
  print $ foldr (-) 0 r3
  print $ get1 @Identity r4
  _ <- traverse print r1
  _ <- traverse print r2
  pure ()
