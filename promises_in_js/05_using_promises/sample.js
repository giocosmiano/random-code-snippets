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

  return new Promise(function(resolve, reject) {
    if(number > 0)
      setTimeout(function() { resolve(countPrimes(number)); }, 1);
    else
      reject("invalid input"); //or throw "invalid input";
  });
};

let reportPrimes = function(number) {
  countPrimesAsync(number)
    .then(function(count) {
      console.log("number of primes within " + number + " is " + count);
    })
    .catch(function(err) {
      console.log("for " + number + " - " + err);      
    });
};

reportPrimes(25000);
reportPrimes(-1);
