-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeRepLike.Test
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Tests
--
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.TypeRepLike.Test where

import Data.Tagged
import Data.Typeable
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.TypeRepLike

-----------------------------------------------------------------------------
-- Type arity tests
-----------------------------------------------------------------------------

typeArityTests :: Test
typeArityTests = testGroup "TypeArity" [
    testCase "arity zero (concrete type)" testZeroArity,
    testCase "arity one" testOneArity,
    testCase "arity one (partially applied type constructor)"
        testOneArityPartiallyApplied,
    testCase "arity two" testTwoArity]

testZeroArity :: Assertion
testZeroArity = 0 @=? (typeArity (Proxy :: Proxy Int))

testOneArity :: Assertion
testOneArity = 1 @=? (typeArity (Proxy :: Proxy Maybe))

testOneArityPartiallyApplied :: Assertion
testOneArityPartiallyApplied = 1 @=? (typeArity (Proxy :: Proxy (Either Int)))

testTwoArity :: Assertion
testTwoArity = 2 @=? (typeArity (Proxy :: Proxy Either))

-----------------------------------------------------------------------------
-- TypeRepLike tests
-----------------------------------------------------------------------------

typeRepLikeTests :: Test
typeRepLikeTests = testGroup "TypeRepLike" [
    testCase "apply (type constructor with one parameter)" testApply,
    testCase "apply (type constructor with two parameters)" testApply2,
    testCase "unify" testUnify,
    testCase "incorrect cast" testCastFailure,
    testCase "polymorphic cast" testCastPolymorphic,
    testCase "polymorphic cast where type variable unification should fail"
        testCastPolymorphicFail]

int :: Tagged Int STypeRep
int = taggedTypeRep (Proxy :: Proxy Int)

string :: Tagged String STypeRep
string = taggedTypeRep (Proxy :: Proxy String)

testApply :: Assertion
testApply = maybe `apply` int @=? maybeInt where
    maybe = fmap asSTypeRep $ taggedTypeRep (Proxy :: Proxy Maybe)
    maybeInt = fmap asSTypeRep $ taggedTypeRep (Proxy :: Proxy (Maybe Int))

testApply2 :: Assertion
testApply2 = either `apply` int `apply` string @=? eitherIntString where
    either = fmap asSTypeRep $ taggedTypeRep (Proxy :: Proxy Either)
    eitherIntString = fmap asSTypeRep $ taggedTypeRep
        (Proxy :: Proxy (Either Int String))

testUnify :: Assertion
testUnify = Just (Tagged utInt) @=? unify (Proxy :: Proxy Int) utInt where
    Tagged utInt = int

testCastFailure :: Assertion
testCastFailure = Nothing @=? taggedCast int string 3

data Foo a b c = Foo c deriving (Show, Typeable, Eq)

testCastPolymorphic :: Assertion
testCastPolymorphic = b @=? taggedCastPolymorphic fooAAInt fooBBInt a where
    a = Foo 3 :: Foo (Phantom ()) (Phantom ()) Int
    b = Just (Foo 3) :: Maybe (Foo String String Int)
    fooAAInt = fmap asSTypeRep $ taggedTypeOf a
    fooBBInt = fmap asSTypeRep $ taggedTypeRep b

testCastPolymorphicFail :: Assertion
testCastPolymorphicFail = b @=? taggedCastPolymorphic fooAAInt fooBBInt a where
    a = Foo 3 :: Foo (Phantom ()) (Phantom ()) Int
    b = Nothing :: Maybe (Foo String Char Int)
    fooAAInt = fmap asSTypeRep $ taggedTypeOf a
    fooBBInt = fmap asSTypeRep $ taggedTypeRep b
