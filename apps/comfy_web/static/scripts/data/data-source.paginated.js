PaginatedDataSource = {
    set_page: function(value, onSuccess) {
        if(!this.__paginateType)
            throw new Error('Pagination type not selected');
        
        if (this._page === value) {
            return;
        }

        var oldValue = this._page;
        this._page = value;
        this.__dropPageOnLoad = false;

        if (this.__loaded) {
            this.load(onSuccess);
        }

        this.raise_pageChanged({ oldValue: oldValue, newValue: value });
    },
    
    set_pageSize: function(value, onSuccess) {
        if(!this.__paginateType)
            throw new Error('Pagination type not selected');
    
        this._pageSize = value;
        
        if(this.__paginateType === 'simple')            
            this.load(onSuccess);
    },

    _onPreLoad: function() {
        if (this.__dropPageOnLoad) {
            this._page = 1;
        }
    },
    
    _onLoadSuccess: function(result, context, onSuccess) {
         if(!this.__paginateType)
            throw new Error('Pagination type not selected');

        if(this.__paginateType === 'simple') {
            this.updateCount();
            arguments.callee.original.apply(this, [result, context, onSuccess]);
            return;
        }

        this.add(result, { notTrackable : true });

        if(onSuccess)
            onSuccess(result, context);
    },
    
    _applySifted: function(entities, sifted) {
//TODO: Research possibilities of normal imlementing

//        debugger;

//        if(this.__paginateType === 'simple') {
//            arguments.callee.original.apply(this, [entities, sifted]);
//            return;
//        }
//    
        var toRemove = entities.where(function(entity) {
            return this.contains(entity) && !sifted.contains(entity);                
        }, this);
        
        if(toRemove.length > 0) {
            this.load();                
            return;
        }
    },

    nextPage: function(onSuccess) {
        this.set_page(this._page + 1, onSuccess);
    },
    
    prevPage: function(onSuccess) {
        this.set_page(this._page - 1, onSuccess);
    }
};

Auto.Properties(PaginatedDataSource, [
    { name: 'page', autoEvent: true },
    'pageSize'        
]);

DataSource.paginate = function(pageSize, onSuccess) {
    if(this.__isPaginated)
        return;

    Trait.Apply(this, PaginatedDataSource, {
        asOriginal: ['_onLoadSuccess', '_applySifted']
    });
    
    this._page = 1;
    this._pageSize = pageSize || 10;
    
    this.simple = function() {
        this.__paginateType = 'simple';
    
        delete this.progressive;
        delete this.simple;
        
        if(onSuccess)
            this.load(onSuccess);
            
        return this;
    }.bind(this);
    this.progressive = function() {
        this.__paginateType = 'progressive';
     
        delete this.progressive;
        delete this.simple;
        
        if(onSuccess)
            this.load(onSuccess);
            
        return this;
    }.bind(this);

    this.__isPaginated = true;

    if (this._filter instanceof DataFilter) {
        this._filter.add_filterChanged(function(sender, args) {
            this.__dropPageOnLoad = true;
        }, this);
    }
    
    if (this.add_searchQueryChanged) {
        this.add_searchQueryChanged(function(sender, args) {
            this.__dropPageOnLoad = true;
        }, this);
    }
    
    return this;
};