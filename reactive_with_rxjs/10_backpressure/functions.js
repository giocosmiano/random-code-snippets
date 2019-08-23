let getKeyboardObservable = function(source) {
  let getWord = function(event) {
    return event.target.value;
  };

  // see Piping --> https://rxjs-dev.firebaseapp.com/guide/operators
  return rxjs.fromEvent(source, 'keyup')
      .pipe(rxjs.operators.map(getWord));
};

let onSubscribe = function() {
  let input = document.getElementById('input');
  let display = document.getElementById('display');
  
  input.placeholder = "type something here";

  let convertCase = function(phrase) {
    let MAX = 50000;
    for(let i = 0; i < MAX; i++) {
      for(let j = 0; j < MAX; j++) {
        let temp = i + j;
      }
    }
    return phrase.toUpperCase(); 
  };
  
  let displayPhrase = function(phrase) { display.innerHTML = phrase; };
  
  let observable = getKeyboardObservable(input);

  // see Piping --> https://rxjs-dev.firebaseapp.com/guide/operators
  if(document.getElementById('debounce').checked)
    observable =
        observable.pipe(rxjs.operators.debounceTime(500));

  if(document.getElementById('throttle').checked)
    observable =
        observable.pipe(rxjs.operators.throttleTime(100));

    observable.pipe(rxjs.operators.map(convertCase))
              .subscribe(displayPhrase);
              
    document.getElementById('debounce').disabled = true;
    document.getElementById('throttle').disabled = true;
};
