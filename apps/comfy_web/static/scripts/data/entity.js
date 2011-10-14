var Entity = function() {
    this.__type = this.get_type().get_name() + ':#Phoenix.WebServices.Contracts.Data';
    this._guid = Guid.New();
};

Entity.prototype = {
    set_id: function(value) {
        if(this._id === value)
            return;
        
        var oldValue = this._id;
        this._id = value;
        
        for(var i = 0; i < this.__dependentSets.length; i++) {            
            this['get_' + this.__dependentSets[i]](null, false).set_keyValue(value);
        }
        
        this.raise_idChanged({
                        newValue: value,
                        oldValue: oldValue
                    });
    },

    deepLoad: function(callback) {
        if (!this.__fullyLoaded) {
            Repository.DeepLoad(this, function() {
			    this.__fullyLoaded = true;

                if (callback)
                    callback.bind(this)();
		    }.bind(this));
        } else if (callback) {
            if (callback)
                callback.bind(this)();
        }
    },
    
    copyFrom: function(entity) {
        for (var property in entity) {
            if (this[property] && this[property].autoSetter && entity[property].autoSetter) {
                var getter = property.replace(property.startsWith('_set_') ? '_set_' : 'set_', 'get_');

                this[property](entity[getter]());
            }
        }
    },
    
    __dependentSets: [],
    
    isNew: function() {
        return !this._id;
    },

    get_type: function() {
        return Type.getInstanceType(this);
    },
    
    dispose: function() { },

    hashCode: function() {
        if (!this.__hashCode) {
            if (this._id) {
                this.__hashCode = this.__type + ":#" + this._id;
            } else {
                this.__hashCode = this.__type + ":#" + Math.random();
            }
        }
        
        return this.__hashCode;
    }
};

Auto.Properties(Entity.prototype, [
	{ name: 'id', autoEvent: true, defaultValue: 0 },
	{ name: 'version', autoEvent: true, defaultValue: '' },
	{ name: 'guid' }
]);

Entity.createClass('Entity');