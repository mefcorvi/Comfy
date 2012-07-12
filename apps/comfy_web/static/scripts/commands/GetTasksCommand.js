var CreateDataSourceCommand = function(dataSourceName) {
    require([dataSourceName, "String"]);
    this._dataSourceName = dataSourceName;
};

CreateDataSourceCommand.prototype = {
    execute: function(socket, callback) {
	socket
	    .send(String.format("{create_datasource,\"{0}\"}", [this._dataSourceName]))
	    .awaitMessage("{datasource_loaded, <Pid>}", function(msg, vars) {
		if (callback) {
		    callback(vars.Pid);
		}
	    });
    }
};

CreateDataSourceCommand.createClass("CreateDataSourceCommand");