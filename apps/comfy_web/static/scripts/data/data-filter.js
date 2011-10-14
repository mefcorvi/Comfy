var CustomFiltersList = function() {
    this._filters = {};
};

CustomFiltersList.prototype = {
    clear: function() {
        this._filters = {};
    },

    get: function(key) {
        return this._filters[key];
    },

    set: function(key, value) {
        var oldValue = this._filters[key];
        this._filters[key] = value;
        this.raise_filterChanged({ key: key, newValue: value, oldValue: oldValue });
    },

    remove: function(key) {
        delete this._filters[key];
        this.raise_filterChanged({ key: key, removed: true });
    },

    count: function() {
        return Object.keysCount(this._filters);
    },

    init: function(parts) {
        for (var i = 0; i < parts.length; i++) {
            var regex = /^(\S+)\(([^)]*?)\)$/i;
            var matches = regex.exec(parts[i]);

            if (!matches || matches.length != 3) {
                this.set(parts[i], undefined);
            } else {
                var args = matches[2].split(',').select(function(obj) { return obj.trim(); });
                this.set(matches[1], args);
            }
        }
    },

    toString: function() {
        var filter = "",
            customFilters = this._filters;

        for (var prop in customFilters) {
            if (customFilters.hasOwnProperty(prop)) {
                var value = customFilters[prop];

                if (isUndefined(value)) {
                    filter += prop + ";";
                } else {
                    filter += prop + "(" + customFilters[prop] + ");"
                }
            }
        }

        return filter;
    }
};

Auto.Events(CustomFiltersList.prototype, [
    'filterChanged'
]);

var DataFilter = function(stringFilter) {
    this._query = "";
    this._customFilters = new CustomFiltersList();
    
    if (stringFilter) {
        this.initFromString(stringFilter);
    }

    this._customFilters.add_filterChanged(function() {
        this._updateFilter();
    }, this);
};

DataFilter.prototype = {
    get_customFilters: function() {
        return this._customFilters;
    },

    get_query: function() {
        return this._query;
    },

    set_query: function(newQuery) {
        var newQuery = newQuery || '';

        if (newQuery === this._query) {
            return;
        }

        this._query = newQuery;
        this._updateFilter();
    },

    _clear: function() {
        this._query = "";
        this._customFilters.clear();
        this._updateFilter();
    },

    initFromString: function(filter) {
        this.__initializing = true;
        this._clear();
        var parts = filter.split(';');
        this._query = parts.pop().trim();
        this._customFilters.init(parts);
        this.__initializing = false;
        this._updateFilter();
    },

    _updateFilter: function() {
        if (this.__initializing) {
            return;
        }

        var oldFilter = this._oldFilter,
            newFilter = this.toString();

        if (newFilter === oldFilter) {
            return;
        }

        this._oldFilter = newFilter;
        this.raise_filterChanged({ newValue: newFilter, oldValue: oldFilter });
    },

    toString: function() {
        return this._customFilters.toString() + this._query;
    }
};

Auto.Events(DataFilter.prototype, [
    'filterChanged'
]);

DataFilter.createClass('DataFilter');