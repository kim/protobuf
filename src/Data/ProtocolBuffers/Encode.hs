{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.ProtocolBuffers.Encode
  ( Encode(..)
  ) where

import Data.Foldable
import Data.Serialize.Put
import Data.Tagged

import GHC.Generics
import GHC.TypeLits

import Data.ProtocolBuffers.Wire

class Encode (a :: *) where
  encode :: a -> Put
  default encode :: (Generic a, GEncode (Rep a)) => a -> Put
  encode = gencode . from

class GEncode (f :: * -> *) where
  gencode :: f a -> Put

instance GEncode a => GEncode (M1 i c a) where
  gencode = gencode . unM1

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode (x :*: y) = gencode x >> gencode y

instance (GEncode a, GEncode b) => GEncode (a :+: b) where
  gencode (L1 x) = gencode x
  gencode (R1 y) = gencode y

instance (Wire a, Foldable f, SingI n) => GEncode (K1 i (Tagged (n :: Nat) (f a))) where
  gencode = traverse_ (encodeWire tag) . unTagged . unK1 where
    tag = fromIntegral $ fromSing (sing :: Sing n)
