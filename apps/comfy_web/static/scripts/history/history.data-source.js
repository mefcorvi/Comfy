Type.createNamespace('History');

History.DataSource = function(itemName, relatedObject, page) {
    History.DataSource.constructBase(this, [itemName, relatedObject, page]);
};

History.DataSource.canObserve = function(obj, type) {
    return (isDefined(obj) && obj.__isDataSource) || type === 'DataSource';
};

History.DataSource.prototype = {
    _ds: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__dsChanged, this);

        if (isDefined(curValue)) {
            this._ds = curValue;
            this._attachHandlersForDs(curValue);
        }
        
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    _attachHandlersForDs: function(ds) {
        if (isDefined(ds)) {
            var isPaginated = !!ds.__isPaginated;
            var isFiltered = !!ds.__isFiltered;
            var isOrdered = !!ds.__isOrdered;

            if (isFiltered) {
                ds.get_filter().add_filterChanged(this._updatePageParam, this);
                ds.add_searchQueryChanged(this._updatePageParam, this);
            }
        
            if (isPaginated) {
                ds.add_pageChanged(this._updatePageParam, this);
            }

            if (isOrdered) {
                ds.add_orderByChanged(this._updatePageParam, this);
                ds.add_sortDirectionChanged(this._updatePageParam, this);
            }
        }
    },

    _detachHandlersFromDs: function(ds) {
        if (isDefined(ds)) {
            var isPaginated = !!ds.__isPaginated;
            var isFiltered = !!ds.__isFiltered;
            var isOrdered = !!ds.__isOrdered;

            if (isFiltered) {
                ds.remove_filterChanged(this._updatePageParam, this);
                ds.remove_searchQueryChanged(this._updatePageParam, this);
            }
        
            if (isPaginated) {
                ds.remove_pageChanged(this._updatePageParam, this);
            }

            if (isOrdered) {
                ds.remove_orderByChanged(this._updatePageParam, this);
                ds.remove_sortDirectionChanged(this._updatePageParam, this);
            }
        }
    },

    __dsChanged: function(sender, args) {
        var ds = args.newValue;
        this._ds = ds;
        this._detachHandlersFromDs(args.oldValue);
        this._attachHandlersForDs(ds);

        this._updatePageParam();
    },

    paramChanged: function(sender, args) {
        this._loadFromHistory(args.value);
    },

    _parseParams: function(paramValue) {
        var paramValues = Base64.decode(paramValue).split('&');
        paramValues.shift();

        var params = {};

        for (var i = 0; i < paramValues.length; i++) {
            var item = paramValues[i];
            var keyValue = item.split('=');
            var key = keyValue[0];
            var value = keyValue[1];

            params[key] = value;
        }

        return params;
    },

    _loadFromHistory: function(paramValue) {
        var oldValue = this._getParamValue();

        if (oldValue === paramValue) {
            return;
        }

        var params = this._parseParams(paramValue);

        var decode = function(value) {
            return value.replaceAll('&eq;', '=').replaceAll('&amp;', '&');
        };

        if (!params['et']) {
            return;
        }

        var ds = [].makeDataSource(params['et']);

        if (params['if'] == 1) {
            var filter = params['f'] || '';
            ds = ds.filterBy(decode(filter));
            
            if (filter) {
            }

            var searchQuery = params['sq'];

            if (searchQuery) {
                ds.set_searchQuery(decode(searchQuery));
            }
        }

        if (params['ip'] == 1) {
            ds = ds.paginate(15).simple();
            var page = params['p'];

            if (page) {
                ds.set_page(page*1);
            }
        }

        if (params['io'] == 1 && params['ob']) {
            ds = ds.orderBy(params['ob']);
            ds.set_sortDirection(params['sd']);
        }

        this._data['set_' + this._itemName](ds);
    },

    _getParamValue: function() {
        var ds = this._ds;
        var paramValue = '';

        if (isNullOrUndefined(ds)) {
            return null;
        }

        var encode = function(value) {
            return value.replaceAll('&', '&amp;').replaceAll('=', '&eq;'); // TODO: global search
        }

        var entityType = ds._entityType.get_name();
        var isPaginated = !!ds.__isPaginated;
        var isFiltered = !!ds.__isFiltered;
        var isOrdered = !!ds.__isOrdered;

        paramValue += '&et=' + encode(entityType);

        if (isFiltered) {
            paramValue += '&if=1';
            var filter = ds.get_filterString();

            if (filter) {
                paramValue += '&f=' + encode(filter);
            }

            var searchQuery = ds.get_searchQuery();

            if (searchQuery) {
                paramValue += '&sq=' + encode(searchQuery);
            }
        }

        if (isPaginated) {
            paramValue += '&ip=1';
            var page = ds.get_page()*1;
            paramValue += '&p=' + page;
        }
        
        if (isOrdered) {
            paramValue += '&io=1';
            var orderBy = ds.get_orderBy().join(',');
            var sortDirection = ds.get_sortDirection();
            paramValue += '&ob=' + orderBy;
            paramValue += '&sd=' + sortDirection;
        }

        return Base64.encode(paramValue);
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.DataSource);
History.DataSource.createClass('History.DataSource', History.BaseObserver);