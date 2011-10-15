FilteredDataSource = {
    set_filter: function (filter) {
        if (isString(filter)) {
            filter = new DataFilter(filter);
        }

        if (!filter instanceof DataFilter) {
            throw new Error('Filter must be of DataFilter type');
        }

        var oldValue = this._filter;
        this._filter = filter;
        this.raise_filterChanged({ newValue: filter, oldValue: oldValue });
    },

    set_searchQuery: function (searchQuery, update) {
        if (typeof searchQuery !== 'string') {
            throw new Error('Search query should be a string');
        }

        var oldSearchQuery = this._searchQuery;
        this._searchQuery = searchQuery;
        this.raise_searchQueryChanged({ newValue: searchQuery, oldValue: oldSearchQuery });

        if (update) {
            this.load();
        }
    },

    _addIdsToFilter: function (filter, items) {
        filter = filter + ' AND Id In (';

        for (var i = 0; i < items.length; i++) {
            filter += (i > 0 ? ',' : '') + items[i]._id;
        }

        filter += ')';

        return filter;
    },

    _isServerFilter: function (filter) {
        filter = filter.trim();
        return filter.endsWith(';');
    },

    _isAnyFilter: function (filter) {
        return filter === '*' || filter.trim() === '';
    },

    _sift: function (items, callback) {        
        var filter = this.get_filterString();

        if (!this._isAnyFilter(filter)) {
            if (!this._isServerFilter(filter)) {
                filter = this._addIdsToFilter(filter, items);
            }
        }
        else { // 'Any' filter
            if ( !this._searchQuery || this._searchQuery === "") {
                callback(items);
                return;
            }
        }

        this._tempFilter = filter;

        this.overallLoad (
            function(result, context) {
                this._tempFilter = null;
                callback(result);               
            }.bind(this),
            true // subsidiary
        );   
    }
};

Auto.Property(FilteredDataSource, { name: 'filter', autoEvent: true });
Auto.Property(FilteredDataSource, { name: 'searchQuery', autoEvent: true });

DataSource.filterBy = function(filter, onSuccess) {
    if(this.__isFiltered)
        return;

    Trait.Apply(this, FilteredDataSource, {
            asOriginal: [ '_sift' ]
        }
    );
    
    if (filter) {
        this.set_filter(filter);
    } else {
        this.set_filter(new DataFilter());
    }

    this._tempFilter = null;
    
    if(onSuccess)
        this.load(onSuccess);
    
    this.__isFiltered = true;
    
    return this;
};