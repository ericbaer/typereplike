-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeRepLike.STypeRep
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | A serializable form of 'TypeRep'. This can be used in a variety of
--   situations if you have a type that you cannot prove is 'Typeable' (for
--   example because you don't even know what it is, statically) but you can
--   recover an 'STypeRep' of it. The most common example is deserialization.
--
--   Basically, if you have a list or tree of captured instance dictionaries,
--   you can use 'STypeRep' to search through the tree.
--
--   Remember that an attacker may send you a false 'STypeRep', causing you to
--   attempt to parse bytes representing a value of one type as though they
--   represent a value of another type. Like in any deserialization situation,
--   you should check that whatever value you deserialized actually obeys the
--   invariants you would expect for its type. See
--   <http://www.reddit.com/r/haskell/comments/1q4r3b/ this reddit thread>
--   for further discussion.
--
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.TypeRepLike.STypeRep (
    STypeRep(..),
    STyCon(..),
    asSTypeRep,
    mkTupleTc,
    listTc,
    funTc
) where

import Control.Applicative
import Data.Aeson
import Data.Data
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Monoid
import Data.Sequence (Seq, (|>))
import GHC.Generics

import qualified Data.Text as Text
import qualified Data.Sequence as Seq

import Data.TypeRepLike.TypeRepLike

-- | The 'Eq' instance is very slow, because of all the comparisons of long
--   'Text's. One options would be to give in and use the GHC "fingerprint"
--   here. Unfortunately we'd still need the lexicographical order for purposes
--   of building & navigating in type trees; see 'Data.TypeTree.TH.treeType'.
data STypeRep = STypeRep {
        _sTyCon :: STyCon,
        _sTyConArgs :: Seq STypeRep
    } deriving (Eq, Ord, Typeable, Data, Generic)

data STyCon = STyCon {
        _sTyConPackage :: Text,
        _sTyConModule :: Text,
        _sTyConName :: Text
    } deriving (Eq, Ord, Typeable, Data, Generic)

asSTypeRep :: STypeRep -> STypeRep
asSTypeRep = id

-----------------------------------------------------------------------------
-- Serialization and conversion
-----------------------------------------------------------------------------

instance TypeRepLike STypeRep STyCon where
    fromTypeRep tr = STypeRep sTyCon sTypeReps where
        (tyCon, typeReps) = splitTyConApp tr
        sTyCon = fromTyCon tyCon
        sTypeReps = Seq.fromList $ map fromTypeRep typeReps
    fromTyCon tyCon = STyCon packageText moduleText nameText where
        packageText = Text.pack $ tyConPackage tyCon
        moduleText = Text.pack $ tyConModule tyCon
        nameText = Text.pack $ tyConName tyCon
    likeSplitTyConApp (STypeRep b as) = (b, as)
    likeMkAppTy (STypeRep f ss) a = STypeRep f (ss |> a)
    likeMkTyConApp = STypeRep

instance ToJSON STypeRep
instance FromJSON STypeRep
instance ToJSON STyCon
instance FromJSON STyCon

instance ToJSON (Seq STypeRep) where
    toJSON = toJSON . toList

instance FromJSON (Seq STypeRep) where
    parseJSON = liftA Seq.fromList . parseJSON

-----------------------------------------------------------------------------
-- Blatant cut-and-paste from Data.Typeable.Internal
-----------------------------------------------------------------------------

instance Show STyCon where
    showsPrec _ (STyCon _ _ s) = showString (Text.unpack s)

instance Show STypeRep where
    showsPrec p (STypeRep tycon tys) =
        case toList tys of
            [] -> showsPrec p tycon
            [x]   | tycon == listTc -> showChar '[' . shows x . showChar ']'
            [a,r] | tycon == funTc  -> showParen (p > 8) $
                                       showsPrec 9 a .
                                       showString " -> " .
                                       showsPrec 8 r
            xs | isTupleTyCon tycon -> showTuple xs
               | otherwise         ->
                    showParen (p > 9) $
                    showsPrec p tycon .
                    showChar ' '      .
                    showArgs (showChar ' ') xs

-- Some (Show.TypeRep) helpers:

showArgs :: Show a => ShowS -> [a] -> ShowS
showArgs _   []     = id
showArgs _   [a]    = showsPrec 10 a
showArgs sep (a:as) = showsPrec 10 a . sep . showArgs sep as

showTuple :: [STypeRep] -> ShowS
showTuple args = showChar '('
               . showArgs (showChar ',') args
               . showChar ')'

isTupleTyCon :: STyCon -> Bool
isTupleTyCon (STyCon _ _ s) = Text.isPrefixOf "(," s

mkTupleTc :: Int -> STyCon
mkTupleTc n = setCount unitTc where
    setCount x = x { _sTyConName = "(" <> Text.replicate (pred n) "," <> ")" }
    unitTc = fromTyCon $ typeRepTyCon $ typeRep (Proxy :: Proxy ())

listTc :: STyCon
listTc = fromTyCon $ typeRepTyCon (typeOf [()])

funTc :: STyCon
funTc = fromTyCon $ typeRepTyCon (typeRep (Proxy :: Proxy (->)))
