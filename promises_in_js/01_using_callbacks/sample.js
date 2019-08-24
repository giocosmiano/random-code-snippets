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

let countPrimesAsync = function(number, callback) {
  setTimeout(function() { callback(countPrimes(number)); },
    1);
};
                              
let number = 250000;
                                         
console.log("Counting primes...");
let numberOfPrimes = countPrimes(number);
console.log("Call made");
console.log("result is " + numberOfPrimes);

console.log("Counting primes...");
countPrimesAsync(number, function(numberOfPrimes) {
  console.log("result is " + numberOfPrimes);  
});

console.log("Call made... we can do other things here if we like...");
