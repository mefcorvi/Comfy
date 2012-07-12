({
    title: "Summary",
    orientation: 'vertical',
    data: [
	{ name: 'messages', value: [].makeObservable() },
	{ name: 'tasks', value: [].makeObservable() },
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
	    type: 'scrollablePanel',
	    bindings: {
		'data:tasks': 'tasks'
	    },
	    controls: [
               {
		   id: 'tasks',
		   type: 'repeater',
		   height: '?',
		   template: {
		       type: 'panel',
		       height: '18px',
		       width: '100%',
		       orientation: 'horizontal',
		       bindings: {
			   'name': 'name',
			   'date': 'date'
		       },
		       controls: [
                           { id: 'name', type: 'label', width: '*', height: '18px' },
                           { id: 'date', type: 'label', width: '*', height: '18px' }
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
	    var ds = this.get_data().get_tasks();
	    session.executeCommand(new CreateDataSourceCommand("tasks"), function(id) {
		session.executeCommand(new AwaitDataSourceItemCommand(id), function(data) {
		    ds.add(data.doc);
		});
	    });
	}.bind(this));
	session._socket.add_onMessage(function(sender, e) {
	    if (window.SocketDebug) {
		console.info(e.data);
	    }
	}, this);
    }
})

