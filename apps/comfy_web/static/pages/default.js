({
    title: "Summary",
    orientation: 'vertical',
    data: [
	{ name: 'messages', value: [].makeObservable() },
	{ name: 'session' }
    ],
    customFunctions: {
	'sendMsg': function(msg) {
	    var socket = Session.get_instance()._socket;
	    socket.send(msg);
	    this.logMessage('sent', msg);
	},
	'logMessage': function(type, msg) {
	    var messages = this.get_data().get_messages();
	    messages.add({ date: new Date(), type: type, message: msg});
	}
    },
    controls: [
	{
	    type: 'historyTextBox',
	    height: "60px",
	    margin: '0 0 0 15'
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
	var session = Session.get_instance();
	session.connect("Admin", "test", function(result) {
	    if (result) {
		console.log("Successfully authorized.");
	    }
	    session.executeCommand(new CreateDataSourceCommand("Tasks"));
	});
	session._socket.add_onMessage(function(sender, e) {
	    this.logMessage(e.data);
	}, this);
    }
})

