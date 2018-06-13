{-# LANGUAGE NamedFieldPuns #-}
module HaskellWorks.Data.Xml.Lazy.Cursor
( LazyCursor
, firstChild
, nextSibling
, nextParent
)
where

import HaskellWorks.Data.Xml.Lazy.Cursor.LazyCursor
import HaskellWorks.Data.Xml.Lazy.Cursor.LazyPointer as Ptr

firstChild :: LazyCursor -> Maybe LazyCursor
firstChild = withPointer moveToFirstChild

nextSibling :: LazyCursor -> Maybe LazyCursor
nextSibling = withPointer moveToNextSibling

nextParent :: LazyCursor -> Maybe LazyCursor
nextParent = withPointer moveToNextParent

-------------------------------------------------------------------------------
withPointer :: (LazyPointer -> Maybe LazyPointer) -> LazyCursor -> Maybe LazyCursor
withPointer f c = Ptr.toCursor <$> f (Ptr.fromCursor c)
{-# INLINE withPointer #-}
