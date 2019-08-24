let fs = require('fs');

let handleResponse = function(err, data) {
  if(err)
    console.log(err);
  else
    console.log("Got content");
};
                                               
console.log("Call with an invalid file");
fs.readFile("invalidfilename", handleResponse);

console.log("Call with a valid file");
fs.readFile("sample.js", handleResponse);

console.log("Call with no parameter");
try {
  fs.readFile();  
}catch(ex) {
  console.log("direct exception!");
}

//lacks consistency, sometimes the call may fail, sometimes, the callback may 
//have an error. What if the callback itself fails?