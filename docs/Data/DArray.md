## Module Data.DArray

#### `DArray`

``` purescript
newtype DArray a
  = DArray (Array a -> Array a)
```

##### Instances
``` purescript
instance showDArray :: (Show a) => Show (DArray a)
instance equalDArray :: (Eq a) => Eq (DArray a)
instance ordDArray :: (Ord a) => Ord (DArray a)
instance semigroupDArray :: Semigroup (DArray a)
instance monoidDArray :: Monoid (DArray a)
instance foldableDArray :: Foldable DArray
instance functorDArray :: Functor DArray
instance applyDArray :: Apply DArray
instance applicativeDArray :: Applicative DArray
instance bindDArray :: Bind DArray
instance monadDArray :: Monad DArray
instance altDArray :: Alt DArray
instance plusDArray :: Plus DArray
instance alternativeDArray :: Alternative DArray
instance monadplusDArray :: MonadPlus DArray
```

#### `unDArray`

``` purescript
unDArray :: forall a. DArray a -> Array a -> Array a
```

## Methods
Returns a concatenation function

#### `toDArray`

``` purescript
toDArray :: forall a. Array a -> DArray a
```

#### `fromDArray`

``` purescript
fromDArray :: forall a. DArray a -> Array a
```

#### `singleton`

``` purescript
singleton :: forall a. a -> DArray a
```

#### `(><)`

``` purescript
(><) :: forall a. DArray a -> DArray a -> DArray a
```

_right-associative / precedence 5_

DArray concatenation

#### `cons`

``` purescript
cons :: forall a. a -> DArray a -> DArray a
```

#### `snoc`

``` purescript
snoc :: forall a. DArray a -> a -> DArray a
```


