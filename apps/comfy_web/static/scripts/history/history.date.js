Type.createNamespace('History');

History.Date = function(itemName, relatedObject, page) {
    History.Date.constructBase(this, [itemName, relatedObject, page]);
};

History.Date.canObserve = function(obj, type) {
    return (isDefined(obj) && (obj instanceof Date)) || type === 'Date';
};

History.Date.prototype = {
    _value: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__dsChanged, this);
        this._value = curValue;
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    __dsChanged: function(sender, args) {
        this._value = args.newValue;
        this._updatePageParam();
    },

    paramChanged: function(sender, args) {
        this._loadFromHistory(args.value);
    },

    _loadFromHistory: function(paramValue) {
        var oldValue = this._getParamValue();

        if (oldValue === paramValue) {
            return;
        }

        var value = new Date(paramValue);

        if (!value) {
            value = null;
        }

        this._data['set_' + this._itemName](value);
    },

    _getParamValue: function() {
        var value = this._value;
        return value ? value.format('d') : '';
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.Date);
History.Date.createClass('History.Date', History.BaseObserver);