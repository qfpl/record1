{-# language DataKinds #-}
{-# language TypeApplications #-}
module Main where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))

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
r2 = fmap (+9999) r1

main :: IO ()
main = print $ get1 @Identity r2
