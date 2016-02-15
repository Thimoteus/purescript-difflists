module Data.DList where

import Prelude (class Monad, class Bind, class Applicative, class Apply, class Functor, class Semigroup, class Ord, class Eq, class Show, (<<<), (++), ($), append, (<*>), map, id, compare, eq, show)

import Data.List ((:), List(..), fromList)
import Data.Function (on)
import Data.Foldable (class Foldable, foldr, foldMap, foldl)
import Data.Monoid (class Monoid, mempty)
import Data.Unfoldable (class Unfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)

newtype DList a = DList (List a -> List a)

-- | ## instances

instance showDList :: (Show a) => Show (DList a) where
  show (DList f) = "DList " ++ show (f Nil) ++ ""

instance equalDList :: (Eq a) => Eq (DList a) where
  eq = eq `on` dlist2List

instance ordDList :: (Ord a) => Ord (DList a) where
  compare = compare `on` dlist2List

instance semigroupDList :: Semigroup (DList a) where
  append = (><)

instance monoidDList :: Monoid (DList a) where
  mempty = DList id

instance foldableDList :: Foldable DList where
  foldr f b0 = foldr f b0 <<< dlist2List
  foldl f b0 = foldl f b0 <<< dlist2List
  foldMap f = foldMap f <<< dlist2List

instance unfoldableDList :: Unfoldable DList where
  unfoldr f b0 = go $ f b0 where
    go Nothing = mempty
    go (Just (Tuple a b)) = cons a (go $ f b)

-- the haskell port
-- `(<$>) g = foldr (cons <<< g) mempty`
-- is both slower and can exceed the maximum stack size
instance functorDList :: Functor DList where
  map g (DList h) = DList $ \ bs -> map g (h Nil) ++ bs

instance applyDList :: Apply DList where
  apply (DList f) (DList x) = DList $ \ bs -> (f Nil <*> (x Nil)) ++ bs

instance applicativeDList :: Applicative DList where
  pure = singleton

instance bindDList :: Bind DList where
  bind m k = foldr (append <<< k) mempty m

instance monadDList :: Monad DList

instance altDList :: Alt DList where
  alt = append

instance plusDList :: Plus DList where
  empty = mempty

instance alternativeDList :: Alternative DList

instance monadplusDList :: MonadPlus DList

-- | ## Methods

-- | Returns a concatenation function
unDList :: forall a. DList a -> List a -> List a
unDList (DList f) = f

toDList :: forall f a. (Foldable f) => f a -> DList a
toDList = foldr cons mempty

fromDList :: forall f a. (Unfoldable f) => DList a -> f a
fromDList (DList f) = fromList $ f Nil 

list2DList :: forall a. List a -> DList a
list2DList xs = DList (xs ++)

dlist2List :: forall a. DList a -> List a
dlist2List (DList f) = f Nil

singleton :: forall a. a -> DList a
singleton = DList <<< (:)

-- | DList concatenation
concat :: forall a. DList a -> DList a -> DList a
concat (DList f) (DList g) = DList (f <<< g)

infixr 5 concat as ><

-- | O(1) consing
cons :: forall a. a -> DList a -> DList a
cons x dly = DList ((x :) <<< unDList dly)

-- | O(1) snocing
snoc :: forall a. DList a -> a -> DList a
snoc dly x = DList (unDList dly <<< (x :))
