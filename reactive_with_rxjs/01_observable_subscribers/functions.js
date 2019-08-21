import { range } from 'rxjs';
import { map, filter } from 'rxjs/operators';

let isPrime = function(number) {
  for(let i = 2; i < number; i++) {
    if(number % i === 0) return false;
  }                                  
  
  return true;
};

let primes = Rx.Observable.create(function(observer) {
  let count = 1;
  while(count < 100000) {
    if(isPrime(count))
      observer.next(count);
    count++;
  }
});

let subscribe = function() {
  primes.subscribe(function(prime) {
    console.log(prime);
  });
};