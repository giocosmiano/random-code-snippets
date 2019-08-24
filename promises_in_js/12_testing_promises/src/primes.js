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

let countPrimesAsync = function(number) {
  return new Promise(function(resolve, reject) {
    let invokeLater = function() {  
      if(number < 1)
        reject('invalid number');
      else
        resolve(countPrimes(number)); 
    };

    setTimeout(invokeLater, 1);
  });
};

module.exports = countPrimesAsync;