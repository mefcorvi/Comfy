var ApplicationContext = function() {
    this._items = {};
};

ApplicationContext.prototype = {
    _items: null,
    
    register: function(key, callback) {
        this._items[key] = callback;
    },
    
    get: function(key) {
        var item = this._items[key];
        
        if (item && isFunction(item)) {
            this._items[key] = item();
            return this._items[key];
        }
        
        return item;
    },
    
    add: function(key, value) {
        this._items[key] = value;
    }
};

ApplicationContext.createClass('ApplicationContext');