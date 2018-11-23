{-# language ExplicitForAll #-}
module Data.Record1.Lens
  ( module Data.Record1
  , field1
  )
where

import Control.Lens.Lens (Lens', lens)
import Data.Record1

{-# inline field1 #-}
field1 :: forall g fs a. Field1 fs g => Lens' (Record1 fs a) (g a)
field1 = lens get1 set1
