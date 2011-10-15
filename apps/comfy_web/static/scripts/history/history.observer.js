Type.createNamespace('History');

History.Observer = {
    _types: [],

    registerObserver: function(observer) {
        History.Observer._types.add(observer);
    },

    _getObserverForValue: function(value, itemType) {
        var types = History.Observer._types;
        
        for (var i = 0; i < types.length; i++) {
            if (types[i].canObserve(value, itemType)) {
                return types[i];
            }
        }

        return History.DefaultObserver;
    },

    init: function(itemName, itemType, relatedObject, page) {
        var value = relatedObject['get_' + itemName]();
        var page = isDefined(page) ? page : Application.get_currentPage();

        return new (this._getObserverForValue(value, itemType))(itemName, relatedObject, page);
    }
};
