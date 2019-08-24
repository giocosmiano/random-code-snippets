Take the example of XMLHttpRequest

var xhr = new XMLHttpRequest();
xhr.open(...);
xhr.send(...);

xhr.onreadystatechange = function() {
 ...callback...
}

The registration of callback may be too late in this case.

Promises resolve or reject, but stay in that state. We can get the result in leisure, no need to rush. No fear of losing.