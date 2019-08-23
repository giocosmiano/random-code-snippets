let getKeyboardObservable = function(source) {
  let getKey = function(event) {
    let key = event.key;
    if(key === 'f' || key === 'F')
      throw "error";
      
    return key;
  };
  
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

  getKeyboardObservable(input).subscribe(displayKey);
};
