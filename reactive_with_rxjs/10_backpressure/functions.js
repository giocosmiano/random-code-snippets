var getKeyboardObservable = function(source) {
  var getWord = function(event) {
    return event.target.value;
  };
  
  return Rx.Observable.fromEvent(source, 'keyup')
           .map(getWord);
}

var subscribe = function() {
  var input = document.getElementById('input');
  var display = document.getElementById('display');
  
  input.placeholder = "type something here";

  var convertCase = function(phrase) {
    var MAX = 50000;
    for(var i = 0; i < MAX; i++) {
      for(var j = 0; j < MAX; j++) {
        var temp = i + j;
      }
    }
    return phrase.toUpperCase(); 
  };
  
  var displayPhrase = function(phrase) { display.innerHTML = phrase; };
  
  var observable = getKeyboardObservable(input);

  if(document.getElementById('debounce').checked)
    observable = observable.debounceTime(500);

  if(document.getElementById('throttle').checked)
    observable = observable.throttleTime(100);
    
    observable.map(convertCase)
              .subscribe(displayPhrase);
              
    document.getElementById('debounce').disabled = true;
    document.getElementById('buffer').disabled = true;
}
