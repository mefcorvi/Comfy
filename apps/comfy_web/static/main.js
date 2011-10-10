function addStatus(text) {
    var date = new Date();
    $('#status').prepend("<p>" + date + ": " + text + "</p>");
};

function sendMessage(msg) {
    document.ws.send(msg);
};

$(document).ready(function() {
		      var txtCommand = $('#command');
		      txtCommand.keydown(function(e) {
					     var keyCode = e.keyCode;
					     if (keyCode == 13) {
						 sendMessage(txtCommand.val());
						 txtCommand.val("");
					     }
					 });
		      
		      if ("WebSocket" in window) {
			  // browser supports websockets
			  var ws = new WebSocket("ws://localhost:8080/service");
			  document.ws = ws;
			  ws.onopen = function() {
			      // websocket is connected
			      addStatus("websocket connected!");
			      // send hello data to server.
			      ws.send("{msg, \"hello\"}");
			      addStatus("sent message to server: 'hello server'!");
			  };
			  
			  ws.onmessage = function (evt) {
			      var receivedMsg = evt.data;
			      addStatus("server sent the following: '" + receivedMsg + "'");
			  };
			  
			  ws.onclose = function() {
			      // websocket was closed
			      addStatus("websocket was closed");
			  };
		      } else {
			  // browser does not support websockets
			  addStatus("sorry, your browser does not support websockets.");
		      }
		  });