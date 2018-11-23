module Data.Record1
  ( Record1
  , emptyR1
  , introR1M
  , introR1
  , trivialR1
  , headR1
  , shrinkR1
  , get1
  , set1
    -- * Helper classes
    --
    -- | These type classes are used internally to implement common operations,
    -- but their members aren't exposed here. The types are only
    -- provided so you can name them in your code
  , Field1, MapRecord1, FoldRecord1, TraverseRecord1
  )
where

import Data.Record1.Internal
