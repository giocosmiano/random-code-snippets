let isPrime = function(number) {
  for(let i = 2; i < number; i++) {
    if(number % i === 0) return false;
  }                                  
  
  return true;
};

let primes = rxjs.Observable.create(function(observer) {
  let count = 1;
  while(count < 10000) {
    if(isPrime(count))
      observer.next(count);
    count++;
  }
  
  observer.complete();
});

let onSubscribe = function() {
  let displayData = function(prime) {
    console.log(prime);
  };

  let noOp = function() {};
  let completed = function() {
    console.log("COMPLETED");
  };
  
  primes.subscribe(displayData, noOp, completed);
};