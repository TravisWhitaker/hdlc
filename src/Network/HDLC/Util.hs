module Network.HDLC.Util where

import Data.Bit

import qualified Data.ByteString as BS

import Data.Char

import qualified Data.Vector.Unboxed as V

import Numeric

newtype Hex a = Hex a

instance Integral a => Show (Hex a) where
    showsPrec _ (Hex x) = ("0x" <>) . showHex x

hexBS :: BS.ByteString -> String
hexBS = show . fmap Hex . BS.unpack

newtype Bin a = Bin a

instance Integral a => Show (Bin a) where
    showsPrec _ (Bin x) = ("0b" <>) . showIntAtBase 2 intToDigit x

binBS :: BS.ByteString -> String
binBS = show .fmap Hex . BS.unpack

decBS :: BS.ByteString -> String
decBS = show . BS.unpack
