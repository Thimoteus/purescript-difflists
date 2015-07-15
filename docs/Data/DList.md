## Module Data.DList

#### `DList`

``` purescript
newtype DList a
  = DList (Array a -> Array a)
```

##### Instances
``` purescript
instance showDList :: (Show a) => Show (DList a)
instance equalDList :: (Eq a) => Eq (DList a)
instance ordDList :: (Ord a) => Ord (DList a)
instance arbDList :: (Arbitrary a) => Arbitrary (DList a)
instance semigroupDList :: Semigroup (DList a)
instance monoidDList :: Monoid (DList a)
instance foldableDList :: Foldable DList
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
unDList :: forall a. DList a -> Array a -> Array a
```

## Methods
Returns a concatenation function

#### `toDList`

``` purescript
toDList :: forall a. Array a -> DList a
```

#### `fromDList`

``` purescript
fromDList :: forall a. DList a -> Array a
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

#### `snoc`

``` purescript
snoc :: forall a. DList a -> a -> DList a
```


