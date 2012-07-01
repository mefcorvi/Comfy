var AddNewTaskCommand = function(name) {
    this._task = {
	name: name
    };
};

AddNewTaskCommand.prototype = {
    execute: function(socket, callback) {
	socket
	    .send(String.format("{command, add_new_task, \"{0}\"}", [this.get_json().replaceAll("\"", "\\\"")]))
	    .awaitMessage("{ok, task_created}", function() {
		callback(true);
	    })
	    .timeout(5000, function() {
		callback(false);
	    });
    },

    get_json: function() {
	return $.toJSON(this._task);
    }
};