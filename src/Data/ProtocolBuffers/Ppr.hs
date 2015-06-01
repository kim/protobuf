{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ProtocolBuffers.Ppr where

import Data.ByteString              (ByteString)
import Data.Int
import Data.ProtocolBuffers.Message
import Data.ProtocolBuffers.Types
import Data.Proxy
import Data.Text                    (Text)
import Data.Word
import GHC.Generics
import GHC.TypeLits
import Text.PrettyPrint


data SomeEnum
    = Enum1
    | Enum2
    | Enum3
    deriving (Show, Bounded, Enum, Eq, Generic)

data Foo = Foo
    { a :: Required 4 (Value Int64)
    , b :: Optional 5 (Value Float)
    , c :: Packed   6 (Value Int32)
    , d :: Repeated 7 (Message Foo)
    , e :: Optional 8 (Enumeration SomeEnum)
    } deriving (Show, Generic)

prettyProto :: forall a . (Generic a, GPrettyProto (Rep a)) => Proxy a -> Doc
prettyProto _ = gprettyProto (Proxy :: Proxy (Rep a))

class GPrettyProto (f :: * -> *) where
  gprettyProto :: Proxy f -> Doc

instance (GPrettyProto f, Datatype d) => GPrettyProto (D1 d f) where
  gprettyProto _
      =   text "message"
      <+> text (datatypeName (undefined :: t d f a))
      $$  gprettyProto (Proxy :: Proxy f)

instance (GPrettyProto f, Constructor c) => GPrettyProto (C1 c f) where
  gprettyProto _ = braces (gprettyProto (Proxy :: Proxy f))

instance (GPrettyProtoSel f, Selector s) => GPrettyProto (S1 s f) where
  gprettyProto _ = gprettyProtoSel (Proxy :: Proxy f) selName'
    where
      selName' = selName (undefined :: t s f a)

instance (GPrettyProto x, GPrettyProto y) => GPrettyProto (x :*: y) where
  gprettyProto _ =
      gprettyProto (Proxy :: Proxy x) $+$ gprettyProto (Proxy :: Proxy y)

class GPrettyProtoSel (f :: * -> *) where
  gprettyProtoSel :: Proxy f -> String -> Doc

instance (KnownNat n, ProtoTypeName a) => GPrettyProtoSel (K1 i (Field n (RequiredField (f a)))) where
  gprettyProtoSel _ selName'
      = text "required"
      <+> text (protoTypeName (Proxy :: Proxy a))
      <+> text selName''
      <+> text "="
      <+> text (show (natVal (Proxy :: Proxy n))) <> char ';'
    where
      selName'' | null selName' = "field" ++ show (natVal (Proxy :: Proxy n))
                | otherwise     = selName'

instance (KnownNat n, ProtoTypeName a) => GPrettyProtoSel (K1 i (Field n (OptionalField (f a)))) where
  gprettyProtoSel _ selName'
      = text "optional"
      <+> text (protoTypeName (Proxy :: Proxy a))
      <+> text selName''
      <+> text "="
      <+> text (show (natVal (Proxy :: Proxy n))) <> char ';'
    where
      selName'' | null selName' = "field" ++ show (natVal (Proxy :: Proxy n))
                | otherwise     = selName'

instance (KnownNat n, ProtoTypeName a) => GPrettyProtoSel (K1 i (Field n (RepeatedField (f a)))) where
  gprettyProtoSel _ selName'
      = text "repeated"
      <+> text (protoTypeName (Proxy :: Proxy a))
      <+> text selName''
      <+> text "="
      <+> text (show (natVal (Proxy :: Proxy n))) <> char ';'
    where
      selName'' | null selName' = "field" ++ show (natVal (Proxy :: Proxy n))
                | otherwise     = selName'

instance (KnownNat n, ProtoTypeName a) => GPrettyProtoSel (K1 i (Field n (PackedField (f a)))) where
  gprettyProtoSel _ selName'
      = text "repeated"
      <+> text (protoTypeName (Proxy :: Proxy a))
      <+> text selName''
      <+> text "="
      <+> text (show (natVal (Proxy :: Proxy n)))
      <+> text "[packed=true]" <> char ';'
    where
      selName'' | null selName' = "field" ++ show (natVal (Proxy :: Proxy n))
                | otherwise     = selName'


class ProtoTypeName a where
  protoTypeName :: Proxy a -> String

instance ProtoTypeName Int32 where
  protoTypeName _ = "int32"

instance ProtoTypeName Int64 where
  protoTypeName _ = "int64"

instance ProtoTypeName Word32 where
  protoTypeName _ = "uint32"

instance ProtoTypeName Word64 where
  protoTypeName _ = "uint64"

instance ProtoTypeName Float where
  protoTypeName _ = "float"

instance ProtoTypeName Double where
  protoTypeName _ = "double"

instance ProtoTypeName Bool where
  protoTypeName _ = "bool"

instance ProtoTypeName Text where
  protoTypeName _ = "string"

instance ProtoTypeName ByteString where
  protoTypeName _ = "string"

instance ProtoTypeName (Fixed Int32) where
  protoTypeName _ = "sfixed32"

instance ProtoTypeName (Fixed Int64) where
  protoTypeName _ = "sfixed64"

instance ProtoTypeName (Fixed Word32) where
  protoTypeName _ = "fixed32"

instance ProtoTypeName (Fixed Word64) where
  protoTypeName _ = "fixed64"

instance ProtoTypeName (Signed Int32) where
  protoTypeName _ = "sint32"

instance ProtoTypeName (Signed Int64) where
  protoTypeName _ = "sint64"

instance ProtoTypeName a => ProtoTypeName (Value a) where
  protoTypeName _ = protoTypeName (Proxy :: Proxy a)

-- this requires UndecidableInstances. any way to do without?
instance MsgTypName (Rep a) => ProtoTypeName (Message a) where
  protoTypeName _ = msgTypName (Proxy :: Proxy (Rep a))

instance MsgTypName (Rep a) => ProtoTypeName (Enumeration a) where
  protoTypeName _ = msgTypName (Proxy :: Proxy (Rep a))

class MsgTypName (f :: * -> *) where
  msgTypName :: Proxy f -> String

instance (Datatype d) => MsgTypName (D1 d f) where
  msgTypName _ = datatypeName (undefined :: t d f a)
