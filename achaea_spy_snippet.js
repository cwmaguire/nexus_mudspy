function textSetup(){
  var cmInput = document.querySelector("div.inputLine div textarea");
  var shiftDelKeyDownHandler = function (event){
    if(event.key == 'Delete' && event.shiftKey){
      //console.log(`Shift+Delete: ${event.key}, ${event.shiftKey}`)
      cmInput.focus();
      return false;
    } else {
      //console.log(`${event.key}, ${event.shiftKey}`)
    };
  };

  document.addEventListener("keydown", shiftDelKeyDownHandler);

  var delKeyDownHandler = function (event){
    if(event.key == 'Delete'){
      //console.log(`Delete: ${event.key}, ${event.shiftKey}`)
      const txt = cmInput.value;
      cmInput.focus();
      cmInput.value = txt;
      return false;
    } else {
      //console.log(`${event.key}, ${event.shiftKey}`)
    };
  };

  document.addEventListener("keydown", delKeyDownHandler);
}

var mudSpySocket;

function openMudSpyWS(){
  const originalSend = WebSocket.prototype.send;

  mudSpySocket = new WebSocket("ws://localhost:8080/achaea");
  mudSpySocket.send = originalSend;

  mudSpySocket.onopen = function(e) {
    console.log("[open] MudSpy Connection established");
  };

  mudSpySocket.onmessage = function(event) {
    console.log(`[message] Data received from server: ${event.data}`);
    //let data = event.data;
  };

  mudSpySocket.onclose = function(event) {
    if (event.wasClean) {
      console.log(`[close] MudSpy Connection closed cleanly, code=${event.code} reason=${event.reason}`);
    } else {
      // e.g. server process killed or network down
      // event.code is usually 1006 in this case
      console.log('[close] MudSpy Connection died');
    }
  };

  mudSpySocket.onerror = function(error) {
    console.log(`[error] MudSpy: ${error.message}`);
  };

}

function setupWSCapture(){
  const originalSend = WebSocket.prototype.send;

  window.sockets = [];
  WebSocket.prototype.send = function(...args) {
    if (window.sockets.indexOf(this) === -1){
      window.sockets.push(this);
      window.sockets[0].addEventListener('message', handleAchaeaWSData);
    }
    return originalSend.call(this, ...args);
  };
}

function mudSpySend(input){
  mudSpySocket.send(input);
}


function handleAchaeaWSData(event) {
  var arry = event.data;
  mudSpySend(arry);

  let str = arrayToString(arry);
  console.log(`Data: ${str.slice(0, 10)}`);
}


function arrayToString(arry){
  uint8Array = new Uint8Array(arry);  // convert to types array
  let s = '';
  for (let i = 0; i < uint8Array.length; ++i){
      s += String.fromCharCode(uint8Array[i]);
  }
  return s;
}

openMudSpyWS();
setupWSCapture();
