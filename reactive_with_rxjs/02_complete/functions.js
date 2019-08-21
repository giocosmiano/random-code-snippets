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
  var displayData = function(prime) {
    console.log(prime);
  };
  
  var noOp = function() {};
  var completed = function() {
    console.log("COMPLETED");
  };
  
  primes.subscribe(displayData, noOp, completed);
}