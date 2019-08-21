var isPrime = function(number) {
  for(var i = 2; i < number; i++) {
    if(number % i === 0) return false;
  }                                  
  
  return true;
} 

var primes = Rx.Observable.create(function(observer) {
  var count = 1;
  while(count < 100000) {
    if(isPrime(count))
      observer.next(count);
    count++;
  }
  
  observer.complete();
});

var subscribe = function() {
  var subscriber = {
    next: function(prime) { 
      console.log(prime); 
      if(prime > 50000) subscriber.unsubscribe();
    }
  };
  
  primes.subscribe(subscriber);
}