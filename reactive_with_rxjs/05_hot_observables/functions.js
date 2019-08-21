var isPrime = function(number) {
  for(var i = 2; i < number; i++) {
    if(number % i === 0) return false;
  }                                  
  
  return true;
} 

var primes = Rx.Observable.create(function(observer) {
  var number = 1;
  var nextPrime = function() {
    number++;
    while(!isPrime(number)) number++;
    
    observer.next(number);
    
    setTimeout(nextPrime, 100);
  }
  
  nextPrime();
}).share();

var subscribe = function(display) {
  primes.subscribe(function(prime) { 
    display.innerHTML = "" + prime;
  });
}