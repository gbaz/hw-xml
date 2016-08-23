{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Xml.TypeSpec (spec) where

import           Control.Monad
import qualified Data.ByteString                                            as BS
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
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
import           HaskellWorks.Data.Xml.Succinct.Index
import           HaskellWorks.Data.Xml.Type
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  describe "Cursor for [Bool]" $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "<elem />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlTypeAt cursor `shouldBe` Just XmlTypeElement
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " <elem />" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      xmlTypeAt cursor `shouldBe` Just XmlTypeElement
    it "cursor can navigate to attr list" $ do
      let cursor = "<a foo='bar' boo='buzz'/>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeAttrList
    it "cursor can navigate through attrs" $ do
      let cursor = "<a foo='bar' boo='buzz'/>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --foo
      (fc >=> fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --bar
      (fc >=> fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --boo
      (fc >=> fc >=> ns >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken --buzz
      (fc >=> fc >=> ns >=> ns >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing --back off!
    it "cursor can navigate to children" $ do
      let cursor = "<a><b /><c /></a>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement --b
      (fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement --c
      (fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing --back off!
    it "cursor recognises child element as an element child next to attr list" $ do
      let cursor = "<a foo='bar'><inner /></a>" :: XmlCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      (fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeAttrList
      (fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement
      (fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing -- no more!

  genSpec "DVS.Vector Word8"  (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  genSpec "Poppy512"          (undefined :: XmlCursor BS.ByteString Poppy512 (SimpleBalancedParens (DVS.Vector Word64)))

genSpec :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u
  , FromForeignRegion (XmlCursor BS.ByteString t u)
  , IsString          (XmlCursor BS.ByteString t u)
  , XmlIndexAt        (XmlCursor BS.ByteString t u)
  )
  => String -> (XmlCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  describe ("Json cursor of type " ++ t) $ do
    let forXml (cursor :: XmlCursor BS.ByteString t u) f = describe ("of value " ++ show cursor) (f cursor)
    forXml "<elem/>" $ \cursor -> do
      it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just XmlTypeElement
    forXml " <elem />" $ \cursor -> do
      it "should have correct type"       $         xmlTypeAt  cursor `shouldBe` Just XmlTypeElement
    forXml "<a foo='bar' boo='buzz'><inner data='none' /></a>" $ \cursor -> do
      it "cursor can navigate to second attribute" $ do
        (fc >=> fc >=> ns >=> ns >=> xmlTypeAt)  cursor  `shouldBe` Just XmlTypeToken
      it "cursor can navigate to first attribute of an inner element" $ do
        (fc >=> ns >=> fc >=> fc >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken
      it "cursor can navigate to first atrribute value of an inner element" $ do
        (fc >=> ns >=> fc >=> fc >=> ns >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken
    describe "For a single element" $ do
      let cursor =  "<a>text</a>" :: XmlCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        (                     xmlTypeAt) cursor `shouldBe` Just XmlTypeElement
        (fc               >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken
        (fc >=> ns        >=> xmlTypeAt) cursor `shouldBe` Nothing
        (fc >=> ns >=> ns >=> xmlTypeAt) cursor `shouldBe` Nothing
    describe "For sample Xml" $ do
      let cursor = "<widget debug=\"on\"> \
                    \  <window name=\"main_window\"> \
                    \    <dimension>500</dimension> \
                    \    <dimension>600.01e-02</dimension> \
                    \    <dimension>    false   </dimension> \
                    \  </window> \
                    \</widget>" :: XmlCursor BS.ByteString t u
      it "can navigate down and forwards" $ do
        (                                                                      xmlTypeAt) cursor `shouldBe` Just XmlTypeElement     --widget
        (fc                                                                >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeAttrList    --widget attrs
        (fc >=> ns                                                         >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement     --window
        (fc >=> ns >=> fc                                                  >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeAttrList    --window attrs
        (fc >=> ns >=> fc >=> ns                                           >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement     --dimension 500
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement     --dimension 600
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeElement     --dimension false
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> xmlTypeAt) cursor `shouldBe` Just XmlTypeToken       --false

