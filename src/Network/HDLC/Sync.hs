{-# LANGUAGE BangPatterns
           , OverloadedLists
           #-}

module Network.HDLC.Sync where

import Data.Bifunctor

import Data.Bit

import qualified Data.ByteString as BS

import qualified Data.Vector.Unboxed as V

flag :: V.Vector Bit
flag = castFromWords8 (V.singleton 0x7e)

prefixIsEsc :: V.Vector Bit -> Bool
prefixIsEsc v = V.length v >= 5 && V.take 5 v == [1, 1, 1, 1, 1]

prefixIsFrame :: V.Vector Bit -> Bool
prefixIsFrame v = V.length v >= 8 && V.take 8 v == [0, 1, 1, 1, 1, 1, 1, 0]


-- super slow
frame :: V.Vector Bit -> V.Vector Bit
frame input =
    let go v | prefixIsEsc v = [1, 1, 1, 1, 1, 0] : go (V.drop 5 v)
             | V.null v = []
             | otherwise = [V.head v] : go (V.tail v)
        esc = V.concat $ go input
    in flag <> esc <> flag
    
data DeframeError = HangingEsc
                  | BadEsc (V.Vector Bit)
                  deriving (Show)

data Deframer = SeekStartFlag
              | CollectFrame ([V.Vector Bit] -> [V.Vector Bit])
              | Finished (V.Vector Bit) (V.Vector Bit)
              | DeframerError DeframeError

instance Show Deframer where
    show SeekStartFlag = "SeekStartFlag"
    show (CollectFrame cs) = "CollectFrame " <> show (cs [])
    show (Finished f rest) = "Finished " <> show f <> " " <> show rest
    show (DeframerError e) = "DeframerError " <> show e

-- super slow
startDeframer :: V.Vector Bit -> Deframer
startDeframer =
    let go SeekStartFlag i
            | prefixIsFrame i = go (CollectFrame id) (V.drop 8 i)
            | V.null i = SeekStartFlag
            | otherwise = go SeekStartFlag (V.tail i)
        go (CollectFrame cs) i
            | prefixIsFrame i = Finished (V.concat (cs [])) i
            | prefixIsEsc i =
                case i V.!? 5 of
                    Nothing -> DeframerError HangingEsc
                    Just 0 -> go (CollectFrame (cs . ([1,1,1,1,1]:))) (V.drop 6 i)
                    _ -> DeframerError (BadEsc i)
            | V.null i = CollectFrame cs
            | otherwise = go (CollectFrame (cs . ([V.head i] :))) (V.tail i)
        go _ _ = error "startDeframer: go absurd state"
    in go SeekStartFlag

deframeAll :: V.Vector Bit -> [V.Vector Bit]
deframeAll =
    let go (Finished f rest)
            | V.null rest = [f]
            | otherwise = f : deframeAll rest
        go _ = []
    in go . startDeframer
