module Data.DArray where

import Prelude

import Data.Array ((:))
import Data.Function (on)
import Data.Foldable
import Data.Monoid
import Data.Unfoldable
import Data.Maybe
import Data.Tuple

import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus

newtype DArray a = DArray (Array a -> Array a)

-- | ## instances

instance showDArray :: (Show a) => Show (DArray a) where
  show (DArray f) = "DArray " ++ show (f []) ++ ""

instance equalDArray :: (Eq a) => Eq (DArray a) where
  eq = eq `on` fromDArray

instance ordDArray :: (Ord a) => Ord (DArray a) where
  compare = compare `on` fromDArray

instance semigroupDArray :: Semigroup (DArray a) where
  append = (><)

instance monoidDArray :: Monoid (DArray a) where
  mempty = DArray id

instance foldableDArray :: Foldable DArray where
  foldr f b0 = foldr f b0 <<< fromDArray
  foldl f b0 = foldl f b0 <<< fromDArray
  foldMap f = foldMap f <<< fromDArray

instance unfoldableDList :: Unfoldable DArray where
  unfoldr f b0 = go $ f b0 where
    go Nothing = empty
    go (Just (Tuple a b)) = cons a (go $ f b)

-- | the haskell port
-- | `(<$>) g = foldr (cons <<< g) mempty`
-- | is both slower and can exceed the maximum stack size
instance functorDArray :: Functor DArray where
  map g (DArray h) = DArray $ \ bs -> map g (h []) ++ bs

instance applyDArray :: Apply DArray where
  apply (DArray f) (DArray x) = DArray $ \ bs -> (f [] <*> (x [])) ++ bs

instance applicativeDArray :: Applicative DArray where
  pure = singleton

instance bindDArray :: Bind DArray where
  bind m k = foldr (append <<< k) mempty m

instance monadDArray :: Monad DArray

instance altDArray :: Alt DArray where
  alt = append

instance plusDArray :: Plus DArray where
  empty = mempty

instance alternativeDArray :: Alternative DArray

instance monadplusDArray :: MonadPlus DArray

-- | ## Methods

-- | Returns a concatenation function
unDArray :: forall a. DArray a -> Array a -> Array a
unDArray (DArray f) = f

toDArray :: forall a. Array a -> DArray a
toDArray xs = DArray (xs ++)

fromDArray :: forall a. DArray a -> Array a
fromDArray (DArray f) = f []

singleton :: forall a. a -> DArray a
singleton = DArray <<< (:)

-- | DArray concatenation
(><) :: forall a. DArray a -> DArray a -> DArray a
(><) (DArray f) (DArray g) = DArray (f <<< g)
infixr 5 ><

cons :: forall a. a -> DArray a -> DArray a
cons x dly = DArray ((x :) <<< unDArray dly)

snoc :: forall a. DArray a -> a -> DArray a
snoc dly x = DArray (unDArray dly <<< (x :))
