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
import Data.Void

-- -----------------------------------------------------------------------------
-- Selective applicative functors

-- | Selective applicative functors.
-- See https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf.
--
-- This instance uses a "curried" version of the 'biselect' from the paper.
class Applicative f => Selective f where
    biselect :: f (Either a (b -> c)) -> f (Either a b) -> f (Either a c)

select :: Selective f => f (Either a b) -> f (a -> b) -> f b
select x y = either id id <$> biselect (adjust <$> x) (Right <$> y)
  where
    adjust :: Either a b -> Either b ((a -> b) -> b)
    adjust (Left  a) = Right ($a)
    adjust (Right b) = Left b

-- | Pick one of the values according to the semantics of the Selective
-- instance. This is a semigroup as long as the arguments yeild the same values.
race :: Selective f => f a -> f a -> f a
race x y = either id absurd <$> biselect (Left <$> x) (Left <$> y)

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
(<||>) x y = either (const True) (const False) <$>
    biselect (boolToEitherX <$> x) (boolToEitherY <$> y)
  where
    boolToEitherX = bool (Right id) (Left ())
    boolToEitherY = bool (Right ()) (Left ())

-- | A lifted version of lazy Boolean AND.
(<&&>) :: Selective f => f Bool -> f Bool -> f Bool
(<&&>) x y = either (const False) (const True) <$>
    biselect (boolToEitherX <$> x) (boolToEitherY <$> y)
  where
    boolToEitherX = bool (Left ()) (Right id)
    boolToEitherY = bool (Left ()) (Right ())
