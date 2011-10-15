Type.createNamespace('History');

History.ObservableList = function(itemName, relatedObject, page) {
    History.ObservableList.constructBase(this, [itemName, relatedObject, page]);
};

History.ObservableList.canObserve = function(obj, type) {
    return (isDefined(obj) && obj.__observable) || type === 'Observable';
};

History.ObservableList.prototype = {
    _item: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__itemChanged, this);

        if (isDefined(curValue)) {
            this._item = curValue;
            this._attachHandlersForItem(curValue);
        }
        
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    _attachHandlersForItem: function(item) {
        if (isDefined(item)) {
            item.add_changed(this._updatePageParam, this);
        }
    },

    _detachHandlersFromItem: function(item) {
        if (isDefined(item)) {
            item.remove_changed(this._updatePageParam, this);
        }
    },

    __itemChanged: function(sender, args) {
        var item = args.newValue;
        this._item = item;
        this._detachHandlersFromItem(args.oldValue);
        this._attachHandlersForItem(item);

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

        var decode = function(value) {
            return value.replaceAll('&z;', ',').replaceAll('&amp;', '&');
        };

        var params = paramValue.split(',');

        for (var i = 0; i < params.length; i++) {
            params[i] = decode(params[i]);
        }

        var item = [].makeObservable();
        this._data['set_' + this._itemName](item);
        item.add(params);
    },

    _getParamValue: function() {
        var item = this._item;
        var paramValue = '';

        if (isNullOrUndefined(item)) {
            return null;
        }

        var encode = function(value) {
            return value.replaceAll('&', '&amp;').replaceAll(',', '&z;');
        }

        var params = [];

        for (var i = 0; i < item.length; i++) {
            params.add(encode(item[i].toString()));
        }

        return params.join(',');
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.ObservableList);
History.ObservableList.createClass('History.ObservableList', History.BaseObserver);