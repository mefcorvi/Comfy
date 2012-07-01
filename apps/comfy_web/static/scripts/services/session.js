var Session = function() {
};

Session.prototype = {
    _socket: null,

    _createSocket: function() {
	this._socket = new Socket();
    
	this._socket.add_onOpen(function() {
	    console.log("Connection has been opened.");
	    this.set_isConnected(true);
	}, this);
    
	this._socket.add_onClose(function() {
	    console.log("Connection has been closed.");
	    this.set_isConnected(false);
	    callback(false);
	}, this);
    },

    /**
     * Подключаемся к серверу
     * @param string login
     * @param string password
     * @param function callback
     */
    connect: function(login, password, callback) {
	this._createSocket();
	this._socket.connect("ws://localhost:8080/service");
	this._socket.add_onOpen(function() {
	    this.executeCommand(new AuthorizeCommand(login, password), callback);
	}, this);
    },

    /**
     * Отключается от сервера
     */
    disconnect: function() {
	this._socket.close();
	this._socket = null;
    },

    /**
     * Выполняет комманду в текущей сессии
     */
    executeCommand: function(command, callback) {
	if (!callback) {
	    callback = Function.empty;
	}
	require([callback, "Function"]);
	command.execute(this._socket, callback);
    }
};

Session.get_instance = function() {
    if (!Session._instance) {
	Session._instance = new Session();
    }

    return Session._instance;
},

Auto.Properties(Session.prototype, [
    { name: 'isConnected', autoEvent: true } // состояние сессии
]);

Session.createClass('Session');