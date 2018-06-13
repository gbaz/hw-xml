module HaskellWorks.Data.Xml.Lazy.Cursor.LazyCursor
where

import Data.ByteString.Lazy (ByteString)
import Data.Vector.Storable (Vector)
import Data.Word            (Word64)

import qualified Data.ByteString.Lazy as LBS

data LazyCursor = LazyCursor
  { cursorText  :: !ByteString
  , cursorBp    :: ![Vector Word64]
  , cursorIb    :: ![Vector Word64]
  , cursorRank  :: !Word64
  , cursorBpPos :: !Word64
  , cursorPos   :: !Word64
  } deriving (Show, Eq)

emptyCursor :: LazyCursor
emptyCursor = LazyCursor
  { cursorText = LBS.empty
  , cursorBp = []
  , cursorIb = []
  , cursorRank = 0
  , cursorBpPos = 0
  , cursorPos = 0
  }
