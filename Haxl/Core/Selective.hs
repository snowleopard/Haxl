-- |
-- Selective functors.
-- See: https://github.com/snowleopard/selective
--
module Haxl.Core.Selective
  (
    -- * Selective functors
    Selective (..)
  , branch
  , race
  , ifS
  , (<||>)
  , (<&&>)
  ) where

import Data.Bool

-- -----------------------------------------------------------------------------
-- Selective applicative functors

-- | Selective applicative functors.
-- See https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf.
--
-- This instance uses a "curried" version of the 'biselect' from the paper.
class Applicative f => Selective f where
    biselect :: (a -> Either c (d -> c)) -> (b -> Either c d) -> f a -> f b -> f c

select :: Selective f => f (Either a b) -> f (a -> b) -> f b
select x y = biselect (either (Right . (flip ($))) Left) Right x y

-- | Pick one of the values according to the semantics of the Selective
-- instance. This is a semigroup as long as the arguments yeild the same values.
race :: Selective f => f a -> f a -> f a
race = biselect Left Left

-- | An operator alias for 'select', which is sometimes convenient. It tries to
-- follow the notational convention for 'Applicative' operators. The angle
-- bracket pointing to the left means we always use the corresponding value.
-- The value on the right, however, may be skipped, hence the question mark.
(<*?) :: Selective f => f (Either a b) -> f (a -> b) -> f b
(<*?) = select

infixl 4 <*?

-- | The 'branch' function is a natural generalisation of 'select': instead of
-- skipping an unnecessary effect, it chooses which of the two given effectful
-- functions to apply to a given argument; the other effect is unnecessary. It
-- is possible to implement 'branch' in terms of 'select', which is a good
-- puzzle (give it a try!).
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch x l r = fmap (fmap Left) x <*? fmap (fmap Right) l <*? r

-- | Branch on a Boolean value, skipping unnecessary effects.
ifS :: Selective f => f Bool -> f a -> f a -> f a
ifS i t e = branch (boolToEither <$> i) (const <$> t) (const <$> e)
  where
    boolToEither = bool (Right ()) (Left ())

-- | A lifted version of lazy Boolean OR.
(<||>) :: Selective f => f Bool -> f Bool -> f Bool
(<||>) x y = biselect f g x y
  where
    f :: Bool -> Either Bool (Bool -> Bool)
    f x = if x then Left True else Right id
    g :: Bool -> Either Bool Bool
    g x = if x then Left True else Right False

-- | A lifted version of lazy Boolean AND.
(<&&>) :: Selective f => f Bool -> f Bool -> f Bool
(<&&>) x y = biselect f g x y
  where
    f :: Bool -> Either Bool (Bool -> Bool)
    f x = if x then Right id else Left False
    g :: Bool -> Either Bool Bool
    g x = if x then Right True else Left False
