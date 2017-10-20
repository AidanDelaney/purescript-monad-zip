-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Zip
-- Copyright   :  (c) Nils Schweinsberg 2011,
--                (c) George Giorgidze 2011
--                (c) University Tuebingen 2011
--                (c) Aidan Delaney 2017 -- ported to PureScript
-- License     :  BSD-style
--
-- Monadic zipping (used for monad comprehensions)
--
-----------------------------------------------------------------------------

module Control.Monad.Zip where

import Prelude
import Control.Monad (class Monad, liftM1, ap)
import Control.Applicative ((<$>), (<*>))
import Data.List(List(..), zip, zipWith, unzip)
import Data.Maybe(Maybe(..))
import Data.Monoid
import Data.Tuple (Tuple(..), fst, snd, uncurry)

import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Monoid.Additive (Additive(..))

-- | `MonadZip` type class. Minimal definition: `mzip` or `mzipWith`
--
-- Instances should satisfy the laws:
--
-- * Naturality :
--
--   > liftM1 (f *** g) (mzip ma mb) = mzip (liftM1 f ma) (liftM1 g mb)
--
-- * Information Preservation:
--
--   > ap (const ()) ma = ap (const ()) mb
--   > ==>
--   > munzip (mzip ma mb) = Tuple ma mb
--
class (Monad m) <= MonadZip m where
    mzip :: forall a b. m a -> m b -> m (Tuple a b)
    mzipWith :: forall a b c. (a -> b -> c) -> m a -> m b -> m c
    munzip :: forall a b. m (Tuple a b) -> (Tuple (m a) (m b))

mzip_ :: forall a b m. (Monad m) => m a -> m b -> m (Tuple a b)
mzip_ ma mb = Tuple <$> ma <*> mb

mzipWith_ :: forall a b c m. (Monad m) => (a -> b -> c) -> m a -> m b -> m c
mzipWith_ f ma mb = liftM1 (uncurry f) (mzip_ ma mb)

munzip_ :: forall a b m. (Monad m) => m (Tuple a b) -> (Tuple (m a) (m b))
munzip_ mab = Tuple (liftM1 fst mab) (liftM1 snd mab)


instance multiplicativeZip :: MonadZip Multiplicative where
  mzip = mzip_
  mzipWith = mzipWith_
  munzip = munzip_

instance maybeZip :: MonadZip Maybe where
  mzip = mzip_
  mzipWith = mzipWith_
  munzip = munzip_

instance additiveZip :: MonadZip Additive where
  mzip = mzip_
  mzipWith = mzipWith_
  munzip = munzip_

instance listZip :: MonadZip List where
  mzip = zip
  mzipWith = zipWith
  munzip = unzip
