let fs = require('fs');

let isPrime = function(number) {
  if(number === 1) return false;

  for(let i = 2; i < number; i++) {
    if(number % i === 0)
      return false;
  }
  return true;
};

let countPrimesAsync = function(number, callback) {
  let countPrimes = function(number) {
    let count = 0;
    for(let i = 1; i <= number; i++) {
      if(isPrime(i))
        count++;
    }           

    return count;
  };

  setTimeout(function() { callback(countPrimes(number)); },
    1);
};

let handleResponse = function(err, lines) {
  if(err)
    console.log("error reading file");
  else {
    let numbers = lines.toString().split("\n");
    
    let results = [];
    
    let processCount = function(number, count) {
      results.push(number + ":" + count);
      
      if(results.length === numbers.length)
        results.forEach(function(result) { console.log(result); });
    };

    for(let i = 0; i < numbers.length; i++) {
      countPrimesAsync(+numbers[i], processCount.bind(null, numbers[i]));
    }
  }    
};

fs.readFile('numbers.txt', handleResponse);