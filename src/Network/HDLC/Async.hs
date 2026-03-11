{-# LANGUAGE BangPatterns
           #-}

module Network.HDLC.Async (
    frame
  , Deframer(..)
  , startDeframer
  , deframeAll
  ) where

import qualified Data.ByteString as BS

import Network.HDLC.Util

data FS = FSStart | FSIdle | FSFrame | FSEsc | FSEnd

frame :: BS.ByteString -> BS.ByteString
frame input = let
    ilen = BS.length input
    go (FSStart, !i) = Just (0x7e, (FSIdle, i))
    go (FSEnd, _) = Nothing
    go (FSFrame, !i) = Just (0x5e, (FSIdle, i))
    go (FSEsc, !i) = Just (0x5d, (FSIdle, i))
    go (FSIdle, !i)
        | i < ilen =
            case BS.index input i of
                0x7e -> Just (0x7d, (FSFrame, i + 1))
                0x7d -> Just (0x7d, (FSEsc, i + 1))
                x -> Just (x, (FSIdle, i + 1))
        | otherwise = Just (0x7e, (FSEnd, i))
    in fst $ BS.unfoldrN (2 + (ilen * 2)) go (FSStart, 0) 

findStartFlag :: BS.ByteString -> BS.ByteString
findStartFlag = BS.dropWhile (/= 0x7e)

tillNextTok :: BS.ByteString -> (BS.ByteString, BS.ByteString)
tillNextTok = BS.break (\c -> c == 0x7e || c == 0x7d)

data DeframeError = HangingEsc
                  | BadEsc BS.ByteString
                  deriving (Show)

data Deframer = SeekStartFlag
              | CollectFrame ([BS.ByteString] -> [BS.ByteString])
              | Finished BS.ByteString BS.ByteString
              | DeframerError DeframeError

instance Show Deframer where
    show SeekStartFlag = "SeekStartFlag"
    show (CollectFrame cs) = "CollectFrame " <> show (fmap (fmap Hex . BS.unpack) (cs []))
    show (Finished f rest) = "Finished " <> show (fmap Hex (BS.unpack f)) <> " " <> show (fmap Hex (BS.unpack rest))
    show (DeframerError e) = "DeframerError " <> show e

startDeframer :: BS.ByteString -> Deframer
startDeframer =
    let go SeekStartFlag i =
            let s = findStartFlag i
            in if BS.null s
               then SeekStartFlag
               else go (CollectFrame id) (BS.tail s)
        go (CollectFrame cs) i =
            let (c, ts) = tillNextTok i
                cs' = cs . (c:)
            in case BS.uncons ts of
                Nothing -> CollectFrame cs'
                Just (0x7e, _) -> Finished (BS.concat (cs' [])) ts
                Just (0x7d, rest) ->
                    case BS.uncons rest of
                        Nothing -> DeframerError HangingEsc
                        Just (0x5e, rest') ->
                            go (CollectFrame (cs' . ((BS.singleton 0x7e):))) rest'
                        Just (0x5d, rest') ->
                            go (CollectFrame (cs' . ((BS.singleton 0x7d):))) rest'
                        _ -> DeframerError $ BadEsc ts
                _ -> error "startDeframer: broken tillNextTok"
        go _ _ = error "startDeframer: go absurd state"
    in go SeekStartFlag

deframeAll :: BS.ByteString -> [BS.ByteString]
deframeAll =
    let go (Finished f rest)
            | BS.null rest = [f]
            | otherwise = f : deframeAll rest
        go _ = []
    in go . startDeframer
