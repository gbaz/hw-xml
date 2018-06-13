{-# LANGUAGE NamedFieldPuns #-}
module HaskellWorks.Data.Xml.Lazy.Cursor.LazyPointerSpec
where

import Data.Word (Word64)

import Control.Monad                                 (forM_, (<=<))
import Data.Function                                 ((&))
import Data.Vector.Storable                          (Vector, fromList)
import HaskellWorks.Data.Xml.Lazy.Cursor.LazyCursor
import HaskellWorks.Data.Xml.Lazy.Cursor.LazyPointer

import HaskellWorks.Data.Bits.BitRead (bitRead)

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

spec :: Spec
spec = describe "Data.Xml.Lazy.Cursor.LazyPointerSpec" $ do
  let bits = "11001010110100"
  let ptr = mkPointer [fromList [readWord64Bits bits]]

  describe "moveForward" $ do
    let allOnes = zip bits [0..] & filter (\x -> fst x == '1') & map snd

    forM_ (zip [1..] allOnes) $ \(i, n) -> do
      it ("should move pointer " ++ show i ++ " times") $
        let ptr' = moveForwardN (fromIntegral i) ptr
        in extractPos <$> ptr' `shouldBe` Just (i, n)

    it "should not move beyond the end of the bits" $
      moveForwardN 8 ptr `shouldBe` Nothing

  describe "moveToFirstChild" $ do
    let start = moveForward' ptr

    it "should move to 1st child" $ do
      extractPos <$> moveToFirstChild start `shouldBe` Just (2, 1)

    it "should not move to non-existing first child withing a child" $
      (moveToFirstChild start >>= moveToFirstChild) `shouldBe` Nothing

    it "should not move to non-existing first child" $
      (moveForwardN 2 start >>= moveToFirstChild) `shouldBe` Nothing

    it "should move to 1st child of another node" $
      extractPos <$> (moveForwardN 4 start >>= moveToFirstChild) `shouldBe` Just (6, 9)

  describe "moveToNextSibling" $ do
    let start = moveForward ptr
    let sib = moveToNextSibling

    it "should move to 1st sibling" $
      extractPos <$> (start >>= sib) `shouldBe` Just (3, 4)

    it "should move to 2nd sibling" $
      extractPos <$> (start >>= sib >>= sib) `shouldBe` Just (4, 6)

    it "should move to 3nd sibling" $
      extractPos <$> (start >>= sib >>= sib >>= sib) `shouldBe` Just (5, 8)

    it "there is no 4th sibling" $
      (start >>= sib >>= sib >>= sib >>= sib) `shouldBe` Nothing

    it "there is no sibling for 1st child" $
      (start >>= moveToFirstChild >>= sib) `shouldBe` Nothing

  describe "moveToNextParent" $ do
    let start = moveForward ptr

    it "should not step up when on the highest level" $
      (start >>= moveToNextParent) `shouldBe` Nothing

    it "should step up one level" $ do
      extractPos <$> (start >>= moveToFirstChild >>= moveToNextParent) `shouldBe` Just (3, 4)

    it "should not step two levels up because there is only one" $
      (start >>= moveToFirstChild >>= moveToNextParent >>= moveToNextParent) `shouldBe` Nothing

-------------------------------------------------------------------------------

moveForwardN :: Int -> LazyPointer -> Maybe LazyPointer
moveForwardN n = foldr (<=<) pure (replicate n moveForward)

mkPointer :: [Vector Word64] -> LazyPointer
mkPointer bp = LazyPointer (emptyCursor { cursorBp = bp }) 0 0

extractPos :: LazyPointer -> (Word64, Word64)
extractPos LazyPointer{pointerRank, pointerPos} = (pointerRank, pointerPos)

readWord64Bits :: String -> Word64
readWord64Bits s = let Just w = bitRead s in w
