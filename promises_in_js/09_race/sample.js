let createPromise = function(timems) {
  return new Promise(function(resolve, reject) {
    setTimeout(resolve.bind(null, timems), timems);
  });
};

let promise1 = createPromise(2000);
let promise2 = createPromise(1000);

Promise.race([promise1, promise2])
  .then(function(result) { console.log(result); })
  .catch(function(err) { console.log(err); });
  
  
  
let promise3 = createPromise(6000);
let promise4 = createPromise(7000);

let timeout = function(timems) {
  return new Promise(function(reject, resolve) {
    setTimeout(reject.bind(null, 'timeout'), timems);
  });
};

Promise.race([promise3, promise4, timeout(5000)])
  .then(function(result) { console.log(result); })
  .catch(function(err) { console.log(err); });  
  
let promise = Promise.race([])
  .then(function(result) { console.log(result);})
  .catch(function(err) { console.log(err); });
  
console.log(promise);