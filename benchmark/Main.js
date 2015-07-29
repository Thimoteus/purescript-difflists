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
