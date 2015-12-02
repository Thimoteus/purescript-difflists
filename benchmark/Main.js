// module Benchmark.Main

exports.randomArray = function(size) {
  return function() {
    var arr = [];
    for (var i = 0; i < size; i++) {
      arr.push(Math.random())
    }
    return arr;
  }
}

exports.randomArrays = function(size) {
  return function(count) {
    return function() {
      var coll = [];
      for (var i = 0; i < count; i++) {
        coll.push(exports.randomArray(size)());
      }
      return coll;
    }
  }
}

