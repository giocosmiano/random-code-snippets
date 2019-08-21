var getKeyboardObservable = function(source) {
  var getKey = function(event) {
    var key = event.key;
    if(key === 'f' || key === 'F')
      throw "error";
      
    return key;
  };
  
  return Rx.Observable.fromEvent(source, 'keyup')
           .map(getKey);
}

var subscribe = function() {
  var input = document.getElementById('input');
  var display = document.getElementById('display');
  
  input.placeholder = "type something here";
                                                   
  var displayKey = function(key) {
    display.innerHTML += ("" + key);
  }
              
  getKeyboardObservable(input).subscribe(displayKey);
}
