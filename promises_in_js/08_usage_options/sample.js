let call = function(func) {
  func()
   .then(function(result) { 
     console.log("result: " + result);
     //throw("ohoh"); //will be caught by the next catch if uncommented.
   })
   .catch(function(err) { console.log("error: " + err);});
}

let create1 = function() {
  return new Promise(function(resolve, reject){
    resolve(1);
  });
};

let create2 = function() {
  return Promise.resolve(10);
};

let create3 = function() {
  return Promise.reject(0);
};

call(create1);
call(create2);
call(create3);
                      
let call2 = function(func) {
  func()
    .then(function(result) {
      console.log("The result: " + result);
      //throw("ohoh"); //will be lost, if uncommented (unless the chain extends further)
    }, function(err) {
      console.log("The error: " + err);
    });
};

call2(create1);
call2(create2);
call2(create3);