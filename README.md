# purescript-difflists

A [difference list](https://wiki.haskell.org/Difference_list) supports fast appending on both sides. A difference list
is just a function that takes a new list and appends it to the "contained" list. This means appending is reduced to
function composition. This is useful if you're appending to the right side many times.

There are versions for arrays and lists in `Data.DArray` and `Data.DList` respectively.

## Benchotron benchmarks

![append](benchmark/images/append-array-darray-seq.png)
![append](benchmark/images/append-list-dlist.png)

For comparison, [here](http://imgur.com/a/qZwBF#7)
is a benchmark between lists and sequences.

## Installation

```
bower install purescript-difflists
```
