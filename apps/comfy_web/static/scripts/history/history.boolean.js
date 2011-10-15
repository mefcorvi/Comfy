Type.createNamespace('History');

History.Boolean = function(itemName, relatedObject, page) {
    History.Boolean.constructBase(this, [itemName, relatedObject, page]);
};

History.Boolean.canObserve = function(obj, type) {
    return (isDefined(obj) && (typeof(obj) == 'boolean')) || type === 'Boolean';
};

History.Boolean.prototype = {
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

        var value = isDefined(paramValue) ? Boolean.parse(paramValue) : false;
        this._data['set_' + this._itemName](value);
    },

    _getParamValue: function() {
        var value = this._value;

        // value on this step can be not only boolean
        // because of this we use that strange convertation to string
        return isDefined(value) ? (value ? 'true' : 'false') : '';
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.Boolean);
History.Boolean.createClass('History.Boolean', History.BaseObserver);