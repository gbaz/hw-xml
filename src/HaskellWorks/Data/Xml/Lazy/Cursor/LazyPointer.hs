{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
module HaskellWorks.Data.Xml.Lazy.Cursor.LazyPointer
where

import Control.Monad                                (mfilter)
import Data.Function                                ((&))
import Data.Vector.Storable                         (Vector)
import Data.Word                                    (Word64)
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Xml.Lazy.Cursor.LazyCursor

data LazyPointer = LazyPointer
  { pointerCursor :: !LazyCursor
  , pointerRank   :: !Word64
  , pointerPos    :: !Word64
  } deriving (Show, Eq)

fromCursor :: LazyCursor -> LazyPointer
fromCursor c = LazyPointer
  { pointerCursor = c
  , pointerRank = cursorRank c
  , pointerPos = cursorBpPos c
  }
{-# INLINE fromCursor #-}

toCursor :: LazyPointer -> LazyCursor
toCursor p =
  let c = pointerCursor p
  in c { cursorBpPos = pointerPos p
       , cursorRank = pointerRank p
       , cursorPos = select1 (cursorIb c) (pointerRank p) - 1
       }
{-# INLINE toCursor #-}

moveForward' :: LazyPointer -> LazyPointer
moveForward' p = p { pointerRank = nextRank, pointerPos = nextPos - 1 }
  where
    bp = cursorBp (pointerCursor p)
    nextPos = select1 bp (pointerRank p + 1)
    nextRank = rank1 bp nextPos
{-# INLINE moveForward' #-}

moveForward :: LazyPointer -> Maybe LazyPointer
moveForward p =
  case moveForward' p of
    p' | pointerRank p == pointerRank p' -> Nothing
    p' -> Just p'
{-# INLINE moveForward #-}

moveToFirstChild :: LazyPointer -> Maybe LazyPointer
moveToFirstChild p =
  moveForward p & mfilter (\p' -> pointerRank p' == pointerRank p + 1 && pointerPos p' == pointerPos p + 1)
{-# INLINE moveToFirstChild #-}

moveToNextSibling :: LazyPointer -> Maybe LazyPointer
moveToNextSibling p = go p
  where
    currentDepth = calcDepth p
    go ptr = case moveForward ptr of
              Nothing -> Nothing
              Just p' ->
                let depth = calcDepth p'
                in if | depth == currentDepth -> Just p'
                      | depth < currentDepth  -> Nothing
                      | otherwise             -> go p'

moveToNextParent :: LazyPointer -> Maybe LazyPointer
moveToNextParent p = go p
  where
    currentDepth = calcDepth p
    go ptr = case moveForward ptr of
              Nothing -> Nothing
              Just p' ->
                let depth = calcDepth p'
                in if | depth >= currentDepth -> go p'
                      | otherwise -> Just p'

calcDepth :: LazyPointer -> Word64
calcDepth LazyPointer{pointerRank, pointerPos} = 2*pointerRank - (pointerPos + 1)
{-# INLINE calcDepth #-}



-------------------------------------------------------------------------------
pointerFrom :: [Vector Word64] -> LazyPointer
pointerFrom bp = LazyPointer (emptyCursor { cursorBp = bp }) 0 0

showPointerPos :: LazyPointer -> String
showPointerPos p = "{ rank: " ++ show (pointerRank p) ++ ", pos: " ++ show (pointerPos p) ++ ", depth: " ++ show (calcDepth p) ++" }"
