var AuthorizeCommand = function(login, password) {
    require([
	login, "String",
	password, "String"
    ]);
    this._login = login;
    this._password = password;
};

AuthorizeCommand.prototype = {
    execute: function(socket, callback) {
	socket
	    .send(String.format("{login, \"{0}\", \"{1}\"}", this._login, this._password))
	    .awaitMessage("{ok,user_authorized}", function() { callback(true); })
	    .timeout(5000, function() { callback(false); });
    }
};

AuthorizeCommand.createClass("AuthorizeCommand");