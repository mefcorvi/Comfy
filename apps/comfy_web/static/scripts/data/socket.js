var Socket = function() {
    this._messages = [];
    this._handlers = {};
};

Socket.prototype = {
    /**
     * Url web-socket сервера
     */
    _address: null,

    /**
     * Объект соединения
     */
    _connection: null,

    /**
     * Сообщения
     */
    _messages: null,

    /**
     * Обработчики сообщений
     */
    _handlers: null,

    awaitMessage: function(message, callback) {
	var messageContainer = {
	    message: new Pattern(message),
	    callback: callback,
	    handle: function(msg) {
		if (this.message.match(msg)) {
		    this.clearTimeout();
		    this.callback(msg, this.message.get_variables());
		}
	    },
	    timeout: function(time, callback) {
		this._timeout = setTimeout(callback, time);
	    },
	    clearTimeout: function() {
		clearTimeout(this._timeout);
		this._timeout = null;
	    }
	};

	this._messages.add(messageContainer);
	return messageContainer;
    },

    /**
     * Регистрирует обработчик событий
     */
    registerMessageHandler: function(pattern, callback) {
	if (!this._handlers[pattern]) {
	    this._handlers[pattern] = {
		pattern: new Pattern(pattern),
		handle: function(msg) {
		    if (this.pattern.match(msg)) {
			this.invokeCallbacks(msg, this.pattern.get_variables());
		    }
		},
		invokeCallbacks: function(msg, vars) {
		    for (var i = 0; i < this.callbacks.length; i++) {
			this.callbacks[i](msg, vars);
		    }
		},
		callbacks: []
	    };
	};

	this._handlers[pattern].callbacks.add(callback);
    },

    /**
     * Осуществляет подключение к серверу с указанным адресом
     */
    connect: function $connect(address) {
	this._address = address;

	if (window["MozWebSocket"]) {
	    window["WebSocket"] = window["MozWebSocket"];
	}

	if ("WebSocket" in window) {
	    this._connection = new WebSocket(this._address);
	    this._connection.onopen = this._onOpen.bind(this);
	    this._connection.onmessage = this._onMessage.bind(this);
	    this._connection.onclose = this._onClose.bind(this);
	} else {
	    throw new Error("Sorry, your browser does not support websockets.");
	}
    },

    /**
     * Отправляет сообщение серверу
     */
    send: function $send(msg) {
	if (!this.get_connected()) {
	    throw new Error("Cannot sent message because connection is closed.");
	}
	this._connection.send(msg);
	return this;
    },

    /**
     * Обработчик события открытия соединения
     */
    _onOpen: function $onOpen() {
	this.set_connected(true);
	this.raise_onOpen();
    },

    /**
     * Обработчик события получения сообщения
     */
    _onMessage: function $onMessage(evt) {
	var message = evt.data;
	var handled = false;

	for (var handlerKey in this._handlers) {
	    if (this._handlers.hasOwnProperty(handlerKey)) {
		if (this._handlers[handlerKey].handle(message)) {
		    handled = true;
		    break;
		}
	    }
	}

	if (!handled) {    
	    for (var i = 0; i < this._messages.length; i++) {
		if (this._messages[i].handle(message)) {
		    this._messages.removeAt(i);
		    handled = true;
		    break;
		}
	    }
	}

	this.raise_onMessage({ data: evt.data });
    },

    /**
     * Обработчик события закрытия соединения
     */
    _onClose: function $onClose() {
	this.set_connected(false);
	this.raise_onClose();
    }
};

Auto.Events(Socket.prototype, [
    'onOpen',
    'onMessage',
    'onClose'
]);

Auto.Properties(Socket.prototype, [
    { name: 'connected', autoEvent: true }
]);

Socket.createClass("Socket");