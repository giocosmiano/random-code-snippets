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
  let subscriber = {
    next: function(prime) { 
      console.log(prime); 
      if(prime > 5000) subscriber.unsubscribe();
    }
  };
  
  primes.subscribe(subscriber);
};