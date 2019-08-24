let createPromise = function(resolveIt) {
  return new Promise(function(resolve, reject){
     setTimeout(function() {
       if(resolveIt)
          resolve(1);
       else
         reject(0);
     }, 1000);
  });
};

let promise1 = createPromise(true);
let promise2 = createPromise(false);

console.log(promise1); //pending
console.log(promise2); //pending

setTimeout(function() {
  console.log(promise1); //fullfilled
  console.log(promise2); //rejected
}, 2000);

/*
Promise { <pending> }
Promise { <pending> }
Promise { 1 }
Promise { <rejected> 0 }
*/