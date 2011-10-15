Type.createNamespace('History');

History.BaseObserver = function(itemName, relatedObject, page) {
    if (!page) {
        throw new Error('Page is null');
    }

    this._page = page;
    this._itemName = itemName;
    this._data = relatedObject;
    
    page.add_onParamChanged(function(sender, args) {       
        if (args.key == this._itemName) {
            this.paramChanged(this, { value: args.newValue });
        }
    }.bind(this));

    this.init();
};

History.BaseObserver.canObserve = function(obj, type) {
    return true;
};

History.BaseObserver.prototype = {
    init: function() {
        var window = this._page,
            itemName = this._itemName,
            data = this._data;

        data['add_' + itemName + 'Changed'](function(sender, args) {                                                
            window.set_param(itemName, args.newValue);
        }, this);

        var paramValue = window.get_param(itemName);

        if (paramValue) {
            data['set_' + itemName](paramValue);
        }
    },

    paramChanged: function(sender, args) {
        this._data['set_' + this._itemName](args.value);
    }
};

History.BaseObserver.createClass('History.BaseObserver');