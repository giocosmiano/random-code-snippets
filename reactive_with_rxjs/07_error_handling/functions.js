var getKeyboardObservable = function(source) {
  var getKey = function(event) {
    var key = event.key;
    if(key === 'f' || key === 'F')
      throw new Error("error, don't do that!");
      
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
  
  var handleError = function(ex) {
    display.innerHTML = ex.message;
  }
              
  getKeyboardObservable(input).subscribe(displayKey, handleError);
}
