module Network.HDLC.Util where

import Data.Bit

import qualified Data.ByteString as BS

import qualified Data.Vector.Unboxed as V

import Numeric (showHex)

newtype Hex a = Hex a

instance Integral a => Show (Hex a) where
    showsPrec _ (Hex x) = ("0x" <>) . showHex x

hexBS :: BS.ByteString -> String
hexBS = show . fmap Hex . BS.unpack

fromBools :: [Bool] -> V.Vector Bit
fromBools = V.fromList . fmap Bit
