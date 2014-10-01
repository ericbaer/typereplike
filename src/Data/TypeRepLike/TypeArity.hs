-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeTree.Operations
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Arity of type constructors.
--
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Data.TypeRepLike.TypeArity (
    -- * Arity of type constructors
    -- ** A digression
    -- | It would also be nice to have a type family to extract a fully-applied
    --   type constructor and break it apart from its arguments, e.g.:
    --
    --   > type family OuterConstructor (a :: *) :: k where
    --   >     OuterConstructor (f a b c) = f
    --   >     OuterConstructor (f a b)   = f
    --   >     OuterConstructor (f a)     = f
    --
    --   The use for that is in something like:
    --
    --   > type OuterArity x = TypeArity (OuterConstructor x)
    --
    --   But this silently fails: 'OuterConstructor' refuses to unify any
    --   argument with one of those equations. A more direct approach to
    --   defining 'OuterArity' is plain old illegal:
    --
    -- @
    -- type family OuterArity (a :: *) where
    --     OuterArity (zz a b c d e f g h)   = 'Eight'
    --     OuterArity (zz a b c d e f g)     = 'Seven'
    --     ...
    -- @
    --
    --   Resulting in the error:
    --
    --   >     Family instance purports to bind type variables ‘k’, ‘k1’
    --   >       but the real LHS (expanding synonyms) is:
    --   >         OuterArity (zz a b c d e f g h) = ...
    --   >     In the equations for closed type family ‘OuterArity’
    --   >     In the type family declaration for ‘OuterArity’
    --
    ---  As the comments to GHC's 'TCValidity.checkValidFamPats' point out,
    --   type families must properly bind all their free type variables.
    -- ** The actual functions
    TypeArity, typeArity
) where

import Data.Typeable

import Data.Peano
import Data.Peano.Extras

-- | The arity of a type constructor. E.g. for 'Int' or $'Maybe' Int$ it's
--   'Zero', for 'Maybe' or $'Either' Int$ it's 'One', for 'Either' it's 'Two',
--   etc. As you can see below, it currently has to be defined by exhaustively
--   listing examples. It could be defined for type constructors of arbitrary
--   arity like this:
--
-- @
-- type family TypeArity (a :: k) where
--     TypeArity (a :: *)       = 'Zero'
--     TypeArity (a :: * -> k1) = 'Succ' (TypeArity (a ()))
-- @
--
--   But unfortunately the recursive step would require 'UndecidableInstances',
--   since it looks like the type family application is getting "bigger". On
--   the bright side, the below definition does not depend on structural
--   relatedness of type-level naturals, so theoretically we could switch it
--   to use GHC's built-in 'TypeNat's.
type family TypeArity (a :: k) where
    TypeArity (a :: *)                                              = Zero
    TypeArity (a :: * -> *)                                         = One
    TypeArity (a :: * -> * -> *)                                    = Two
    TypeArity (a :: * -> * -> * -> *)                               = Three
    TypeArity (a :: * -> * -> * -> * -> *)                          = Four
    TypeArity (a :: * -> * -> * -> * -> * -> *)                     = Five
    TypeArity (a :: * -> * -> * -> * -> * -> * -> *)                = Six
    TypeArity (a :: * -> * -> * -> * -> * -> * -> * -> *)           = Seven
    TypeArity (a :: * -> * -> * -> * -> * -> * -> * -> * -> *)      = Eight
    TypeArity (a :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *) = Nine

-- | Examine the arity of a type constructor at the value level. This allows
--   us to avoid hard-coding "magic numbers" in a few places when processing
--   'Data.Typeable.TypeRep's.
typeArity :: forall a n proxy. (Integral n, Nat (TypeArity a)) => proxy a -> n
typeArity _ = nat (Proxy :: Proxy (TypeArity a))
