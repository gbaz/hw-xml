
module HaskellWorks.Data.Xml.Succinct.Cursor.BlankedXml
  ( BlankedXml(..)
  , FromBlankedXml(..)
  , getBlankedXml
  ) where

import HaskellWorks.Data.ByteString
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Xml.Conduit.Blank

import qualified Data.ByteString as BS

newtype BlankedXml = BlankedXml [BS.ByteString] deriving (Eq, Show)

getBlankedXml :: BlankedXml -> [BS.ByteString]
getBlankedXml (BlankedXml bs) = bs

class FromBlankedXml a where
  fromBlankedXml :: BlankedXml -> a

instance FromByteString BlankedXml where
  fromByteString bs = BlankedXml (blankXml (chunkedBy 4064 bs))
