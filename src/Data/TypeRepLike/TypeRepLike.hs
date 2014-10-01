-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeRepLike.TypeRepLike
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Written so that some functions which originally worked on
--   'Data.Aeson.Tagged.STypeRep.STypeRep's could be applied to real
--   'TypeRep's too.
--
-----------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TypeRepLike.TypeRepLike (
    TypeRepLike(..),
    -- * Utilities
    compareTypes,
    flattenTypeRepLike2,
    likeTypeOf,
    likeTypeRep,
    untaggedApply,
    -- * Working with Tagged type representations
    taggedTypeRep,
    taggedTypeOf,
    taggedMkAppTy,
    unify,
    apply,
    -- * Casts
    eqPoly,
    Phantom,
    phantomCon,
    taggedCast,
    taggedCastPolymorphic
) where

import Control.Applicative
import Control.Monad.State
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Sequence (Seq, (|>), ViewL(..))
import Data.Typeable
import Data.Tagged
import Unsafe.Coerce

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

class (Eq r, Eq c, Ord r, Ord c) => TypeRepLike r c | r -> c, c -> r where
    -- | Converts a 'TypeRep'
    fromTypeRep :: TypeRep -> r

    -- | Converts a 'TyCon'
    fromTyCon :: TyCon -> c

    -- | Analogous to 'mkAppTy'
    likeMkAppTy :: r -> r -> r

    -- | Analogous to 'mkTyConApp'
    likeMkTyConApp :: c -> Seq r -> r

    -- | Analogous to 'splitTyConApp'
    likeSplitTyConApp :: r -> (c, Seq r)

instance TypeRepLike TypeRep TyCon where
    fromTypeRep = id
    fromTyCon = id
    likeMkAppTy = mkAppTy
    likeMkTyConApp c = mkTyConApp c . toList
    likeSplitTyConApp = fmap Seq.fromList . splitTyConApp

-----------------------------------------------------------------------------
-- Miscellaneous utility methods for manipulatiog 'TypeRepLike's
-----------------------------------------------------------------------------

-- | Analogous to 'typeOf'.
likeTypeOf :: (Typeable x, TypeRepLike r c) => x -> r
likeTypeOf = fromTypeRep . typeOf

-- | Analogous to 'typeRep'.
likeTypeRep :: (Typeable x, TypeRepLike r c) => proxy x -> r
likeTypeRep = fromTypeRep . typeRep

-- | Flatten a right-branching tree of 'TyCon'-like applications.
flattenTypeRepLike2 :: (TypeRepLike r c) => c -> r -> [r]
flattenTypeRepLike2 stackCon stackRep =
    let (_, rs) = likeSplitTyConApp stackRep in
        if Seq.length rs /= 2
        then [stackRep] -- We reached the bottom of the stack
        else let
                l = Seq.index rs 0
                r = Seq.index rs 1
                rcon = fst $ likeSplitTyConApp r
            in
                if stackCon /= rcon
                then [l, r] -- We reached the bottom of the stack
                else l : flattenTypeRepLike2 stackCon r

compareTypes :: (Typeable a, TypeRepLike r c) => r -> proxy a -> Ordering
compareTypes r a = compare r $ likeTypeRep a

untaggedApply :: (TypeRepLike r con) => r -> r -> r
untaggedApply f a = likeMkTyConApp fcon (fargs |> a) where
    (fcon, fargs) = likeSplitTyConApp f

-----------------------------------------------------------------------------
-- Working with 'Tagged' versions of 'TypeRepLike' types
-----------------------------------------------------------------------------

-- | Analogous to 'typeRep'
taggedTypeRep :: (Typeable a, TypeRepLike r c) => proxy a -> Tagged a r
taggedTypeRep = Tagged . likeTypeRep

-- | Analogous to 'typeOf'
taggedTypeOf :: (Typeable a, TypeRepLike r c) => a -> Tagged a r
taggedTypeOf = Tagged . likeTypeOf

-- | Analogous to 'mkAppTy': applies type constructor 'f' to argument 'a'.
taggedMkAppTy :: (TypeRepLike r c) =>
    Tagged f r -> Tagged a r -> Tagged (f a) r
taggedMkAppTy (Tagged f) (Tagged a) = Tagged $ likeMkAppTy f a

-- | Tags a 'TypeRep'-like with 'a', but only if they represent the same type
unify :: (Typeable a, Alternative m, TypeRepLike r c) =>
    proxy a -> r -> m (Tagged a r)
unify a r = if taggedTypeRep a == Tagged r then pure $ Tagged r else empty

-- | Applies a (possibly already partially-applied) type constructor 'f'
--   to an argument 'a'.
apply :: (TypeRepLike r con) => Tagged f r -> Tagged a r -> Tagged (f a) r
apply (Tagged f) (Tagged a) = Tagged $ untaggedApply f a

-----------------------------------------------------------------------------
-- Casts
-----------------------------------------------------------------------------

-- | Casts 'a' to 'b' if they have the same 'TypeRepLike' representation
taggedCast :: (TypeRepLike r c) => Tagged a r -> Tagged b r -> a -> Maybe b
taggedCast (Tagged r) (Tagged s) a =
    if r == s then Just $ unsafeCoerce a else Nothing

-- | A placeholder representing any type; used to represent
--   phantom type variables for 'taggedCastPolymorphic'
data Phantom p deriving Typeable

phantomCon :: (TypeRepLike r c) => c
phantomCon = fst $ likeSplitTyConApp $ likeTypeRep phantom where
    phantom = Proxy :: Proxy (Phantom ())

-- | Compare two 'TypeRepLike's for equality in the presence of 'Phantom's.
--   This only allows 'Phantom's as type arguments, not as type constructors.
eqPoly :: (TypeRepLike r c) => (r, r) -> State (Map r r) Bool
eqPoly (a, b)
    | aCon == phantomCon && maArg /= Nothing = do
        let Just aArg = maArg
        result <- gets (Map.lookup aArg)
        case result of
            -- Never saw this type variable before, bind it
            Nothing -> modify (Map.insert aArg b) >> return True
            -- Seen this type variable before, check against previous
            Just c  -> eqPoly (c, b)
    -- if a is a malformed 'Phantom', just give up
    | aCon == phantomCon                   = return False
    | aCon /= bCon                         = return False
    | Seq.length aArgs /= Seq.length bArgs = return False
    | otherwise                            = and <$> mapM eqPoly abArgs
    where
        (aCon, aArgs) = likeSplitTyConApp a
        (bCon, bArgs) = likeSplitTyConApp b
        abArgs = zip (toList aArgs) (toList bArgs)
        maArg = case Seq.viewl aArgs of
            x :< _ -> Just x
            EmptyL -> Nothing

-- | Casts 'a' to 'b' if they have the same 'TypeRepLike representation, where
--   'a' (but not 'b') is allowed to have 'Phantom's. The caller must ensure
--   that, e.g., $f x y 'Phantom'$ really has the same internal representation
--   as $f x y z$, e.g. 'z' should be a phantom type variable.
--   FIXME: can use 'Coercible' to make this a bit safer?
--   FIXME: restrict to named phantom variables
taggedCastPolymorphic :: forall a b r c. (TypeRepLike r c) =>
    Tagged a r -> Tagged b r -> a -> Maybe b
taggedCastPolymorphic (Tagged ra) (Tagged rb) a = let
    eqPoly' :: (r, r) -> Bool
    eqPoly' rr = evalState (eqPoly rr) (Map.empty :: Map r r)
    in if eqPoly' (ra, rb) then Just $ unsafeCoerce a else Nothing
