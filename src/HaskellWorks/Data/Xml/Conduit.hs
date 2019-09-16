{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module HaskellWorks.Data.Xml.Conduit
  ( blankedXmlToInterestBits
  , byteStringToBits
  , blankedXmlToBalancedParens2
  , compressWordAsBit
  , interestingWord8s
  , isInterestingWord8
  ) where

import Control.Monad
import Data.Array.Unboxed             as A
import Data.ByteString                as BS
import Data.Conduit
import Data.Word
import Data.Word8
import HaskellWorks.Data.Bits.BitWise
import Prelude                        as P

import qualified Data.Bits as BITS

interestingWord8s :: A.UArray Word8 Word8
interestingWord8s = A.array (0, 255) [
  (w, if w == _bracketleft
         || w == _braceleft
         || w == _parenleft
         || w == _bracketleft
         || w == _less
         || w == _a || w == _v || w == _t
    then 1
    else 0)
  | w <- [0 .. 255]]
{-# NOINLINE interestingWord8s #-}

isInterestingWord8 :: Word8 -> Word8
isInterestingWord8 b = interestingWord8s ! b
{-# INLINABLE isInterestingWord8 #-}

blankedXmlToInterestBits :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
blankedXmlToInterestBits = blankedXmlToInterestBits' ""

blankedXmlToInterestBits' :: Monad m => BS.ByteString -> ConduitT BS.ByteString BS.ByteString m ()
blankedXmlToInterestBits' rs = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = if BS.length rs /= 0 then BS.concat [rs, bs] else bs
      let lencs = BS.length cs
      let q = lencs `quot` 8
      let (ds, es) = BS.splitAt (q * 8) cs
      let (fs, _) = BS.unfoldrN q gen ds
      yield fs
      blankedXmlToInterestBits' es
    Nothing -> do
      let lenrs = BS.length rs
      let q = lenrs + 7 `quot` 8
      yield (fst (BS.unfoldrN q gen rs))
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen as = if BS.length as == 0
          then Nothing
          else Just ( BS.foldr' (\b m -> (interestingWord8s ! b) .|. (m .<. 1)) 0 (BS.take 8 as)
                    , BS.drop 8 as
                    )

repartitionMod8 :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString)
repartitionMod8 aBS bBS = (BS.take cLen abBS, BS.drop cLen abBS)
  where abBS = BS.concat [aBS, bBS]
        abLen = BS.length abBS
        cLen = (abLen `div` 8) * 8

compressWordAsBit :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
compressWordAsBit = compressWordAsBit' BS.empty

compressWordAsBit' :: Monad m => BS.ByteString -> ConduitT BS.ByteString BS.ByteString m ()
compressWordAsBit' aBS = do
  mbBS <- await
  case mbBS of
    Just bBS -> do
      let (cBS, dBS) = repartitionMod8 aBS bBS
      let (cs, _) = BS.unfoldrN (BS.length cBS + 7 `div` 8) gen cBS
      yield cs
      compressWordAsBit' dBS
    Nothing -> do
      let (cs, _) = BS.unfoldrN (BS.length aBS + 7 `div` 8) gen aBS
      yield cs
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen xs = if BS.length xs == 0
          then Nothing
          else Just ( BS.foldr' (\b m -> ((b .&. 1) .|. (m .<. 1))) 0 (BS.take 8 xs)
                    , BS.drop 8 xs
                    )

blankedXmlToBalancedParens2 :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
blankedXmlToBalancedParens2 = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (cs, _) = BS.unfoldrN (BS.length bs * 2) gen (Nothing, bs)
      yield cs
      blankedXmlToBalancedParens2
    Nothing -> return ()
  where gen :: (Maybe Bool, ByteString) -> Maybe (Word8, (Maybe Bool, ByteString))
        gen (Just True  , bs) = Just (0xFF, (Nothing, bs))
        gen (Just False , bs) = Just (0x00, (Nothing, bs))
        gen (Nothing    , bs) = case BS.uncons bs of
          Just (c, cs) -> case balancedParensOf c of
            MiniN  -> gen         (Nothing    , cs)
            MiniT  -> Just (0xFF, (Nothing    , cs))
            MiniF  -> Just (0x00, (Nothing    , cs))
            MiniTF -> Just (0xFF, (Just False , cs))
          Nothing   -> Nothing

data MiniBP = MiniN | MiniT | MiniF | MiniTF

balancedParensOf :: Word8 -> MiniBP
balancedParensOf c = case c of
    d | d == _less         -> MiniT
    d | d == _greater      -> MiniF
    d | d == _bracketleft  -> MiniT
    d | d == _bracketright -> MiniF
    d | d == _parenleft    -> MiniT
    d | d == _parenright   -> MiniF
    d | d == _t            -> MiniTF
    d | d == _a            -> MiniTF
    d | d == _v            -> MiniTF
    _                      -> MiniN

yieldBitsOfWord8 :: Monad m => Word8 -> ConduitT BS.ByteString Bool m ()
yieldBitsOfWord8 w = do
  yield ((w .&. BITS.bit 0) /= 0)
  yield ((w .&. BITS.bit 1) /= 0)
  yield ((w .&. BITS.bit 2) /= 0)
  yield ((w .&. BITS.bit 3) /= 0)
  yield ((w .&. BITS.bit 4) /= 0)
  yield ((w .&. BITS.bit 5) /= 0)
  yield ((w .&. BITS.bit 6) /= 0)
  yield ((w .&. BITS.bit 7) /= 0)

yieldBitsofWord8s :: Monad m => [Word8] -> ConduitT BS.ByteString Bool m ()
yieldBitsofWord8s = P.foldr ((>>) . yieldBitsOfWord8) (return ())

byteStringToBits :: Monad m => ConduitT BS.ByteString Bool m ()
byteStringToBits = do
  mbs <- await
  case mbs of
    Just bs -> yieldBitsofWord8s (BS.unpack bs) >> byteStringToBits
    Nothing -> return ()
