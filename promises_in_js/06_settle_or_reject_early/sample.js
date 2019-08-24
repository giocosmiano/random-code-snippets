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
    if(number > 0) {
      resolve(countPrimes(number));
      console.log("Promise resolved");
    }
    else {
      reject("invalid input"); //or throw "invalid input";
      console.log("Promise rejected");
    }
  });
};

let reportPrimes = function(number) {
  let promise = countPrimesAsync(number);

  console.log("Sign up with the promise to process reject or fulfillment");
  promise
    .then(function(count) {
      console.log("number of primes within " + number + " is " + count);
    })
    .catch(function(err) {
      console.log("for " + number + " - " + err);      
    });
};

reportPrimes(25000);
reportPrimes(-1);
