//Compare this to the code in 3_hard_to_compose
//To run, first type 
//npm install
//on the command line from under the 
//11_promise_all directory. Then run 
//node sample.js

let fs = require('fs-promise');

let isPrime = function(number) {
  if(number === 1) return false;

  for(let i = 2; i < number; i++) {
    if(number % i === 0)
      return false;
  }
  return true;
};

let countPrimes = function(number) {
  let count = 0;
  for(let i = 1; i <= number; i++) {
    if(isPrime(i))
      count++;
  }           

  return count;
};

let countPrimesAsync = function(number) {
  return new Promise(function(resolve, reject) {
    let invokeLater = function() { resolve(countPrimes(number)); };
    setTimeout(invokeLater, 1);
  });
};

let splitLines = function(lines) { return lines.toString().split('\n'); };

let numberAndPrimesCount = function(number) {
  return countPrimesAsync(number)
    .then(function(count) { return number + ":" + count; });
}

let processLines = function(lines) {
  return Promise.all(lines.map(numberAndPrimesCount));
};

let displayResults = function(results) {
  results.forEach(function(result) { console.log(result); });
};

fs.readFile('numbers.txt')
  .then(splitLines)
  .then(processLines)
  .then(displayResults)
  .catch(function(err) { console.log(err); });