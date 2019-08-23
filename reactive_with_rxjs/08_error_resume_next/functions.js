let getKeyboardObservable = function(source) {
  let getKey = function(event) {
    let key = event.key;
    if(key === 'f' || key === 'F')
      throw new Error("error, don't do that!");
      
    return key;
  };

  // see Piping --> https://rxjs-dev.firebaseapp.com/guide/operators
  return rxjs.fromEvent(source, 'keyup')
           .pipe(rxjs.operators.map(getKey));
};

let onSubscribe = function() {
  let input = document.getElementById('input');
  let display = document.getElementById('display');
  
  input.placeholder = "type something here";

  let displayKey = function(key) {
    display.innerHTML += ("" + key);
  };

  let handleError = function(ex) {
    console.log('here');
    display.innerHTML = ex.message;
  };
              
  getKeyboardObservable(input)
    .onErrorResumeNext(getKeyboardObservable(input))
    .subscribe(displayKey, handleError);
};
