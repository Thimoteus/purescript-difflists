# purescript-difflists

A [difference list](https://wiki.haskell.org/Difference_list) supports fast concatenation on both sides.

## Installation

```
bower install purescript-difflists
```

## Module Documentation

### Module Data.DList

#### `DList`

``` purescript
newtype DList a
  = DList (Array a -> Array a)
```


#### `showDList`

``` purescript
instance showDList :: (Show a) => Show (DList a)
```

### instances

#### `equalDList`

``` purescript
instance equalDList :: (Eq a) => Eq (DList a)
```


#### `ordDList`

``` purescript
instance ordDList :: (Ord a) => Ord (DList a)
```


#### `arbDList`

``` purescript
instance arbDList :: (Arbitrary a) => Arbitrary (DList a)
```


#### `semigroupDList`

``` purescript
instance semigroupDList :: Semigroup (DList a)
```


#### `monoidDList`

``` purescript
instance monoidDList :: Monoid (DList a)
```


#### `foldableDList`

``` purescript
instance foldableDList :: Foldable DList
```


#### `functorDList`

``` purescript
instance functorDList :: Functor DList
```

the haskell port
`(<$>) g = foldr (cons <<< g) mempty`
is both slower and can exceed the maximum stack size

#### `applyDList`

``` purescript
instance applyDList :: Apply DList
```


#### `applicativeDList`

``` purescript
instance applicativeDList :: Applicative DList
```


#### `bindDList`

``` purescript
instance bindDList :: Bind DList
```


#### `monadDList`

``` purescript
instance monadDList :: Monad DList
```


#### `altDList`

``` purescript
instance altDList :: Alt DList
```


#### `plusDList`

``` purescript
instance plusDList :: Plus DList
```


#### `alternativeDList`

``` purescript
instance alternativeDList :: Alternative DList
```


#### `monadplusDList`

``` purescript
instance monadplusDList :: MonadPlus DList
```


#### `unDList`

``` purescript
unDList :: forall a. DList a -> Array a -> Array a
```

### Methods
Returns a concatenation function

#### `toDList`

``` purescript
toDList :: forall a. [a] -> DList a
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

DList concatenation

#### `cons`

``` purescript
cons :: forall a. a -> DList a -> DList a
```


#### `snoc`

``` purescript
snoc :: forall a. DList a -> a -> DList a
```
