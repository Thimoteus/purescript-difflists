## Module Data.DList

#### `DList`

``` purescript
newtype DList a
  = DList (List a -> List a)
```

##### Instances
``` purescript
instance showDList :: (Show a) => Show (DList a)
instance equalDList :: (Eq a) => Eq (DList a)
instance ordDList :: (Ord a) => Ord (DList a)
instance semigroupDList :: Semigroup (DList a)
instance monoidDList :: Monoid (DList a)
instance foldableDList :: Foldable DList
instance unfoldableDList :: Unfoldable DList
instance functorDList :: Functor DList
instance applyDList :: Apply DList
instance applicativeDList :: Applicative DList
instance bindDList :: Bind DList
instance monadDList :: Monad DList
instance altDList :: Alt DList
instance plusDList :: Plus DList
instance alternativeDList :: Alternative DList
instance monadplusDList :: MonadPlus DList
```

#### `unDList`

``` purescript
unDList :: forall a. DList a -> List a -> List a
```

## Methods
Returns a concatenation function

#### `toDList`

``` purescript
toDList :: forall f a. (Foldable f) => f a -> DList a
```

#### `fromDList`

``` purescript
fromDList :: forall f a. (Unfoldable f) => DList a -> f a
```

#### `list2DList`

``` purescript
list2DList :: forall a. List a -> DList a
```

#### `dlist2List`

``` purescript
dlist2List :: forall a. DList a -> List a
```

#### `singleton`

``` purescript
singleton :: forall a. a -> DList a
```

#### `(><)`

``` purescript
(><) :: forall a. DList a -> DList a -> DList a
```

_right-associative / precedence 5_

DList concatenation

#### `cons`

``` purescript
cons :: forall a. a -> DList a -> DList a
```

O(1) consing

#### `snoc`

``` purescript
snoc :: forall a. DList a -> a -> DList a
```

O(1) snocing


