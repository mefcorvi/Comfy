var AwaitDataSourceItemCommand = function(id) {
    require([id, "String"]);
    this._id = id;
};

AwaitDataSourceItemCommand.prototype = {
    execute: function(socket, callback) {
	socket
	    .registerMessageHandler("{datasource_updated, <Id>, <<Data>>}", function(msg, vars) {
		if (vars.Id == this._id && callback) {
		    callback($.evalJSON(vars.Data));
		}
	    }.bind(this));
    }
};