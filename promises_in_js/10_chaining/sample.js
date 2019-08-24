let asyncFunc1 = function(input) {
  return new Promise(function(resolve, reject){
     setTimeout(resolve.bind(null, input), 1000);
  });
};

let asyncFunc2 = function(input) {
  return new Promise(function(resolve, reject){
     setTimeout(resolve.bind(null, input * 2), 1000);
  });
};

let asyncFunc3 = function(input) {
  return new Promise(function(resolve, reject){
     setTimeout(reject.bind(null, input * 2), 1000);
  });
};

asyncFunc1(1000)
  .then(function(result) {
    return result + 1;
  })
  .then(function(result) {
    console.log("result: " + result);
  })
  .catch(function(err) {
    console.log("Error: " + err);
  });

asyncFunc1(1000)
  .then(function(result) {
    throw('oops');
  })
  .then(function(result) {
    console.log("result: " + result);
  })
  .catch(function(err) {
    console.log("Error: " + err);
  });
  
asyncFunc1(1000)
  .then(function(result) {
    return asyncFunc2(result);
  })
  .then(function(result) {
    console.log("result: " + result);
  })
  .catch(function(err) {
    console.log("Error: " + err);
  });  

asyncFunc1(1000)
  .then(function(result) {
    return asyncFunc3(result);
  })
  .then(function(result) {
    console.log("result: " + result);
  })
  .catch(function(err) {
    console.log("Error: " + err);
  });