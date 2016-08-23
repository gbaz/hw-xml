{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.Succinct.CursorSpec(spec) where

import           Control.Monad
import qualified Data.ByteString                                            as BS
import qualified Data.Map                                                   as M
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import qualified HaskellWorks.Data.TreeCursor                               as TC
import           HaskellWorks.Data.Xml.Succinct.Cursor                      as C
--import           HaskellWorks.Data.Xml.Succinct.Index
--import           HaskellWorks.Data.Xml.Token
--import           HaskellWorks.Data.Xml.Value
--import           System.IO.MMap
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling
pn = TC.parent
cd = TC.depth
ss = TC.subtreeSize

spec :: Spec
spec = describe "HaskellWorks.Data.Xml.Succinct.CursorSpec" $ do
  describe "Cursor for Element" $ do
    it "depth at top" $ do
      let cursor = "<widget debug='on'/>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd cursor `shouldBe` Just 1
    it "depth at attribute list" $ do
      let cursor = "<widget debug='on' />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> cd) cursor `shouldBe` Just 2
    it "depth first attribute" $ do
      let cursor = "<widget debug='on' enabled='on' />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> fc >=> cd) cursor `shouldBe` Just 3
    it "depth second attribute" $ do
      let cursor = "<widget debug='on' enabled='on' />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
    it "depth at value" $ do
      let cursor = "<widget debug='on'>text</widget>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> ns >=> cd) cursor `shouldBe` Just 2
    -- it "depth at first child of object at second child of array" $ do
    --   let cursor = "[null, {\"field\": 1}]" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
    --   (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3
--   genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
--   genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
--   genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
--   genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
--   genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))
--   it "Loads same Xml consistentally from different backing vectors" $ do
--     let cursor8   = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))
--     let cursor16  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))
--     let cursor32  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))
--     let cursor64  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
--     cursorText cursor8 `shouldBe` cursorText cursor16
--     cursorText cursor8 `shouldBe` cursorText cursor32
--     cursorText cursor8 `shouldBe` cursorText cursor64
--     let ic8   = bitShow $ interests cursor8
--     let ic16  = bitShow $ interests cursor16
--     let ic32  = bitShow $ interests cursor32
--     let ic64  = bitShow $ interests cursor64
--     ic16 `shouldBeginWith` ic8
--     ic32 `shouldBeginWith` ic16
--     ic64 `shouldBeginWith` ic32

-- shouldBeginWith :: (Eq a, Show a) => [a] -> [a] -> IO ()
-- shouldBeginWith as bs = take (length bs) as `shouldBe` bs

-- genSpec :: forall t u.
--   ( Eq                t
--   , Show              t
--   , Select1           t
--   , Eq                u
--   , Show              u
--   , Rank0             u
--   , Rank1             u
--   , BalancedParens    u
--   , TestBit           u
--   , FromForeignRegion (XmlCursor BS.ByteString t u)
--   , IsString          (XmlCursor BS.ByteString t u)
--   , XmlIndexAt       (XmlCursor BS.ByteString t u)
--   )
--   => String -> (XmlCursor BS.ByteString t u) -> SpecWith ()
-- genSpec t _ = do
--   describe ("Cursor for (" ++ t ++ ")") $ do
--     let forXml (cursor :: XmlCursor BS.ByteString t u) f = describe ("of value " ++ show cursor) (f cursor)
--     forXml "[null]" $ \cursor -> do
--       it "depth at top"                   $ cd          cursor `shouldBe` Just 1
--       it "depth at first child of array"  $ (fc >=> cd) cursor `shouldBe` Just 2
--     forXml "[null, {\"field\": 1}]" $ \cursor -> do
--       it "depth at second child of array" $ do
--         (fc >=> ns >=> cd) cursor `shouldBe` Just 2
--       it "depth at first child of object at second child of array" $ do
--         (fc >=> ns >=> fc >=> cd) cursor `shouldBe` Just 3
--       it "depth at first child of object at second child of array" $ do
--         (fc >=> ns >=> fc >=> ns >=> cd) cursor `shouldBe` Just 3

--     describe "For sample Json" $ do
--       let cursor =  "<widget debug=\"on\"> \
--                     \  <window name=\"main_window\"> \
--                     \    <dimension>500</dimension> \
--                     \    <dimension>600.01e-02</dimension> \
--                     \    <dimension>    false   </dimension> \
--                     \  </window> \
--                     \</widget>" :: XmlCursor BS.ByteString t u
--       it "can get token at cursor" $ do
--         (xmlTokenAt                                                                      ) cursor `shouldBe` Just (XmlTokenBraceL                 )
--         (fc                                                                >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "widget"      )
--         (fc >=> ns                                                         >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenBraceL                 )
--         (fc >=> ns >=> fc                                                  >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "debug"       )
--         (fc >=> ns >=> fc >=> ns                                           >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "on"          )
--         (fc >=> ns >=> fc >=> ns >=> ns                                    >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "window"      )
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenBraceL                 )
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "name"        )
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "main_window" )
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenString   "dimensions"  )
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> xmlTokenAt) cursor `shouldBe` Just (XmlTokenBracketL               )
--       it "can navigate up" $ do
--         (                                                                      pn) cursor `shouldBe` Nothing
--         (fc                                                                >=> pn) cursor `shouldBe`                                    Just cursor
--         (fc >=> ns                                                         >=> pn) cursor `shouldBe`                                    Just cursor
--         (fc >=> ns >=> fc                                                  >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
--         (fc >=> ns >=> fc >=> ns                                           >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
--         (fc >=> ns >=> fc >=> ns >=> ns                                    >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> pn) cursor `shouldBe` (fc >=> ns                            ) cursor
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn) cursor `shouldBe` (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
--       it "can get subtree size" $ do
--         (                                                                      ss) cursor `shouldBe` Just 16
--         (fc                                                                >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns                                                         >=> ss) cursor `shouldBe` Just 14
--         (fc >=> ns >=> fc                                                  >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns >=> fc >=> ns                                           >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns >=> fc >=> ns >=> ns                                    >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> ss) cursor `shouldBe` Just 10
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> ss) cursor `shouldBe` Just 1
--         (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ss) cursor `shouldBe` Just 6
