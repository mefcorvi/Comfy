({
    title: "Summary",
    orientation: 'vertical',
    data: [
	{ name: 'messages', value: [].makeObservable() },
	{ name: 'socket' }
    ],
    customFunctions: {
	'sendMsg': function(msg) {
	    var ws = this.get_data().get_socket();
	    ws.send(msg);
	    this.logMessage('sent', msg);
	},
	'logMessage': function(type, msg) {
	    var messages = this.get_data().get_messages();
	    messages.add({ date: new Date(), type: type, message: msg});
	}
    },
    controls: [
	{
	    type: 'textbox',
	    mode: 'multiline',
	    height: "60px",
	    margin: '0 0 0 15',
	    onKeyDown: function(sender, args) {
		if (args.keyCode == Keys.Up && this.getCaret() < 1 && this._history.length > 0 && this._historyPos > 0) {
		    this._historyPos--;
		    var lastMsg = this._history[this._historyPos];
		    this.set_text(lastMsg);
		}
		if (args.keyCode == Keys.Down && (this.getCaret() >= this.get_text().length)) {
		    if (this._historyPos < this._history.length - 1) {
			this._historyPos++;
			var lastMsg = this._history[this._historyPos];
			this.set_text(lastMsg);
		    } else {
			this.set_text("");
			this._historyPos = this._history.length;
		    }
		}
	    },
	    onEnterPressed: function(sender, args) {
		args.preventDefault();
		args.stopPropagation();
		var msg = this.get_text();
		this.get_window().sendMsg(msg);
		this.set_text('');
		this._history.push(msg);
		this._historyPos = this._history.length;
	    },
	    onLoad: function() {
		this._historyPos = -1;
		this._history = [];
	    }
	},
	{
	    type: 'scrollablePanel',
	    bindings: {
		'data:messages': 'log'
	    },
	    controls: [
		{
		    id: 'log',
		    type: 'repeater',
		    template: {
			type: 'panel',
			height: '?',
			orientation: 'horizontal',
			bindings: {
			    'date': 'date',
			    'type': 'type',
			    'message': 'msg'
			},
			controls: [
			    { type: 'label', id: 'date', valign: 'middle', format: '{0:T}', width: '100' },
			    { type: 'label', id: 'type', valign: 'middle', width: '50' },
			    { type: 'label', id: 'msg', valign: 'middle', width: '*' }
			]
		    }
		}
	    ]
	}
    ],
    onLoad: function() {
	if (window["MozWebSocket"])
	    window["WebSocket"] = window["MozWebSocket"];

	if ("WebSocket" in window) {
	    // browser supports websockets
	    var ws = new WebSocket("ws://localhost:8080/service");
	    this.get_data().set_socket(ws);

	    ws.onopen = function() {
		this.logMessage('info', "Socket connected!");
	    }.bind(this);

	    ws.onmessage = function (evt) {
		var receivedMsg = evt.data;
		this.logMessage('receive', receivedMsg);
	    }.bind(this);

	    ws.onclose = function() {
		// websocket was closed
		this.logMessage('Socket closed');
	    }.bind(this);
	} else {
	    // browser does not support websockets
	    this.logMessage('error', "Sorry, your browser does not support websockets.");
	}
    }
})

