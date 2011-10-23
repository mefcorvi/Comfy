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
		      
		  });