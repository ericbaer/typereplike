-----------------------------------------------------------------------------
--
-- Module      :  MainTestSuite
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Tests for typereplikes
--
-----------------------------------------------------------------------------

module Main (
    main
 ) where

import Test.Framework

import Data.TypeRepLike.Test

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [typeArityTests, typeRepLikeTests]
