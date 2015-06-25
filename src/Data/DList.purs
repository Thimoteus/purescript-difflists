module Data.DList where

import Prelude hiding (cons)

import Data.Array (map)
import Data.Function (on)
import Data.Foldable
import Data.Monoid

import Test.QuickCheck

import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus

newtype DList a = DList (Array a -> Array a)

-- | ## instances

instance showDList :: (Show a) => Show (DList a) where
  show (DList f) = "DList " ++ show (f []) ++ ""

instance equalDList :: (Eq a) => Eq (DList a) where
  (==) = (==) `on` fromDList
  (/=) dl1 dl2 = not (dl1 == dl2)

instance ordDList :: (Ord a) => Ord (DList a) where
  compare = compare `on` fromDList

instance arbDList :: (Arbitrary a) => Arbitrary (DList a) where
  arbitrary = toDList <$> arbitrary

instance semigroupDList :: Semigroup (DList a) where
  (<>) = (><)

instance monoidDList :: Monoid (DList a) where
  mempty = DList id

instance foldableDList :: Foldable DList where
  foldr f b0 = foldr f b0 <<< fromDList
  foldl f b0 = foldl f b0 <<< fromDList
  foldMap f = foldMap f <<< fromDList

-- | the haskell port
-- | `(<$>) g = foldr (cons <<< g) mempty`
-- | is both slower and can exceed the maximum stack size
instance functorDList :: Functor DList where
  (<$>) g (DList h) = DList $ \ bs -> map g (h []) ++ bs

instance applyDList :: Apply DList where
  (<*>) (DList f) (DList x) = DList $ \ bs -> (f [] <*> (x [])) ++ bs

instance applicativeDList :: Applicative DList where
  pure = singleton

instance bindDList :: Bind DList where
  (>>=) m k = foldr ((<>) <<< k) mempty m

instance monadDList :: Monad DList

instance altDList :: Alt DList where
  (<|>) = (><)

instance plusDList :: Plus DList where
  empty = mempty

instance alternativeDList :: Alternative DList

instance monadplusDList :: MonadPlus DList

-- | ## Methods

-- | Returns a concatenation function
unDList :: forall a. DList a -> Array a -> Array a
unDList (DList f) = f

toDList :: forall a. [a] -> DList a
toDList xs = DList (xs ++)

fromDList :: forall a. DList a -> Array a
fromDList (DList f) = f []

singleton :: forall a. a -> DList a
singleton = DList <<< (:)

-- | DList concatenation
(><) :: forall a. DList a -> DList a -> DList a
(><) (DList f) (DList g) = DList (f <<< g)
infixr 5 ><

cons :: forall a. a -> DList a -> DList a
cons x dly = DList ((x :) <<< unDList dly)

snoc :: forall a. DList a -> a -> DList a
snoc dly x = DList (unDList dly <<< (x :))
