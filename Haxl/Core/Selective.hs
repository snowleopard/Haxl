-- |
-- Selective functors.
-- See: https://github.com/snowleopard/selective
--
module Haxl.Core.Selective
  (
    -- * Selective functors
    Selective (..)
  , branch
  , ifS
  , selectA
  , selectM
  , (<||>)
  , (<&&>)
  ) where

import Data.Bool

-- -----------------------------------------------------------------------------
-- Selective applicative functors

-- | Selective applicative functors. You can think of 'select' as a selective
-- function application: when given a value of type @Left a@, you __must apply__
-- the given function, but when given a @Right b@, you __may skip__ the function
-- and associated effects, and simply return the @b@.
class Applicative f => Selective f where
    select :: f (Either a b) -> f (a -> b) -> f b

-- | An operator alias for 'select', which is sometimes convenient. It tries to
-- follow the notational convention for 'Applicative' operators. The angle
-- bracket pointing to the left means we always use the corresponding value.
-- The value on the right, however, may be skipped, hence the question mark.
(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = select

infixl 4 <*?

-- | We can write a function with the type signature of 'select' using the
-- 'Applicative' type class, but it will always execute the effects associated
-- with the second argument, hence being potentially less efficient.
selectA :: Applicative f => f (Either a b) -> f (a -> b) -> f b
selectA x f = (\e f -> either f id e) <$> x <*> f

-- | One can easily implement a monadic 'selectM' that satisfies the laws,
-- hence any 'Monad' is 'Selective'.
selectM :: Monad f => f (Either a b) -> f (a -> b) -> f b
selectM mx mf = do
    x <- mx
    case x of
        Left  a -> fmap ($a) mf
        Right b -> pure b

-- | The 'branch' function is a natural generalisation of 'select': instead of
-- skipping an unnecessary effect, it chooses which of the two given effectful
-- functions to apply to a given argument; the other effect is unnecessary. It
-- is possible to implement 'branch' in terms of 'select', which is a good
-- puzzle (give it a try!).
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch x l r = fmap (fmap Left) x <*? fmap (fmap Right) l <*? r

-- | Branch on a Boolean value, skipping unnecessary effects.
ifS :: Selective f => f Bool -> f a -> f a -> f a
ifS i t e = branch (bool (Right ()) (Left ()) <$> i) (const <$> t) (const <$> e)

-- | A lifted version of lazy Boolean OR.
(<||>) :: Selective f => f Bool -> f Bool -> f Bool
(<||>) a b = ifS a (pure True) b

-- | A lifted version of lazy Boolean AND.
(<&&>) :: Selective f => f Bool -> f Bool -> f Bool
(<&&>) a b = ifS a b (pure False)
