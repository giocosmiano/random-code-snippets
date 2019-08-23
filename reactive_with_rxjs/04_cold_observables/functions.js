let isPrime = function(number) {
  for(let i = 2; i < number; i++) {
    if(number % i === 0) return false;
  }                                  
  
  return true;
};

let primes = rxjs.Observable.create(function(observer) {
  let number = 1;
  let nextPrime = function() {
    number++;
    while(!isPrime(number)) number++;
    
    observer.next(number);
    
    setTimeout(nextPrime, 100);
  };
  
  nextPrime();
});

let onSubscribe = function(display) {
  primes.subscribe(function(prime) { 
    display.innerHTML = "" + prime;
  });
};