DataSource = {
    init: function (entityType, childTypes) {
        if (this.__initialized)
            return;

        if (!entityType)
            throw new Error('Entity type for DataSource not selected');

        this._entityType = entityType;
        this._childTypes = childTypes;

        this.__initialized = true;
        this.__loaded = false;
    },

    get_filter: function() {
        return this._filter;
    },

    get_filterString: function() {
        var filter = this.get_filter();
        return (filter ? filter.toString() : null) || '*';
    },

    set_filterString: function(newFilter) {
        var filter = this.get_filter();

        if (!filter) {
            throw new Error('Filter is not instantiated');
        }

        filter.initFromString(newFilter);
    },

    _onPreLoad: function() {
    },

    overallLoad: function(onSuccessFilter, isSubsidiary) {
        this._onPreLoad();
        var filterValue = isSubsidiary ? this._tempFilter : this.get_filterString();

        if (this.__rid) {
            Repository.Abort(this.__rid);
            this.__rid = null;
        }

        this.__rid = Repository.Filter(            
            this._entityType.get_name(),
            {
                filter: filterValue,
                searchQuery: this._searchQuery || '',
                orderBy: this._orderBy || null,
                sortDirection: this._sortDirection || null,
                page: this._page || null,
                pageSize: this._pageSize || null
            },
            function (result, context) {
                this.__rid = null;
                onSuccessFilter(result, context);
            }.bind(this),
            {
                requestCaller: this,
                subsidiary: isSubsidiary
            }
        );
    },

    load: function (onSuccess) {
        if (this.__loadTimeout) {
            clearTimeout(this.__loadTimeout);
        }

        this.__loadTimeout = setTimeout(function() {
            this.overallLoad (
                function(result, context) {
                    if (!this.__loaded) {
                        this.__attachHandlers();
                        this.__loaded = true;
                    }

                    this._onLoadSuccess(result, context, onSuccess);                
                }.bind(this),
                false // subsidiary
            );
        }.bind(this), 0);
        
        return this;        
    },

    isLoaded: function () {
        return this.__loaded;
    },

    updateCount: function (onSuccess) {
        Repository.Count(
            this._entityType.get_name(),
            {
                filter: this.get_filterString(),
                searchQuery: this._searchQuery || ''
            },
            function (result, context) {
                if (onSuccess)
                    onSuccess(result);

                this.set_count(result);
            } .bind(this)
        );
    },

    add: function (data) {        
        var items = data instanceof Array ? data : [data];

        for (var i = 0; i < items.length; i++)
            this.splice(this._findPosition(items[i]), 0, items[i]);
    },

    updateFrom: function (entities) {
        this._sift(entities, function (sifted) {
            this._applySifted(entities, sifted);
        } .bind(this));
    },

    // overridable by dataSource traits

    _findPosition: function (item) {
        return this.length;
    },

    _sift: function (items, callback) {
        callback(items);
    },

    _onLoadSuccess: function (result, context, onSuccess) {        
        this.synchronize(result, null, { notTrackable: true });

        if (onSuccess)
            onSuccess(result, context);
    },

    _applySifted: function (entities, sifted) {
        var toRemove = entities.where(function (entity) {
            return this.contains(entity) && !sifted.contains(entity);
        }, this);

        if (toRemove.length > 0)
            this.remove(toRemove, { notTrackable: true });

        var toAdd = sifted.where(function (item) { return !this.contains(item); }, this);

        if (toAdd.length > 0)
            this.add(toAdd, { notTrackable: true });
    },

    // ********************************

    get_entityType: function () {
        return this._entityType;
    },

    ensureTypeIs: function (entityType) {
        if (this._entityType !== entityType) {
            throw new Error('[DataSource] Type check assertion. Expected type is "' + entityType + '". Actual type is "' + this._entityType + '".');
        }
    },

    __attachHandlers: function () {
        Repository['add_' + this._entityType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
        Repository['add_' + this._entityType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
        Repository['add_' + this._entityType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);

        if (this._childTypes && this._childTypes instanceof Array) {
            for (var i = 0; i < this._childTypes.length; i++) {
                var childType = this._childTypes[i];
                Repository['add_' + childType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
                Repository['add_' + childType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
                Repository['add_' + childType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);
            }
        }
    },

    __detachHandlers: function () {
        Repository['remove_' + this._entityType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
        Repository['remove_' + this._entityType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
        Repository['remove_' + this._entityType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);

        if (this._childTypes) {
            for (var i = 0; i < this._childTypes.length; i++) {
                var entityType = this._childTypes[i];
                Repository['remove_' + entityType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
                Repository['remove_' + entityType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
                Repository['remove_' + entityType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);
            }
        }
    },

    __entitySavedHandler: function (sender, args, context) {
        this._onEntitySave(sender, args, context);
    },
    __entityLoadedHandler: function (sender, args, context) {
        if (!context || (context.requestCaller !== this && !context.subsidiary))
            this._onEntityLoad(sender, args, context);
    },
    __entityDeletedHandler: function (sender, args, context) {
        this._onEntityDelete(sender, args, context);
    },

    _onEntitySave: function (sender, args) {
        this.updateFrom(args.entities);
    },

    _onEntityLoad: function (sender, args) {        
        this.updateFrom(args.entities);
    },

    _onEntityDelete: function (sender, args) {
        for (var i = 0; i < args.entities.length; i++) {
            if (this.contains(args.entities[i]))
                this.remove(args.entities[i]);
        }
    },

    dispose: function () {
        if (this.__rid) {
            Repository.Abort(this.__rid);
            this.__rid = null;
        }

        if(this.__loaded) {
            this.__detachHandlers();
        }

        if (this.__events) {
            delete this.__events;
        }
    }
};

Array.prototype.makeDataSource = function(entityTypeName, callback, childTypes) {
    if(this.__isDataSource)
        return;
        
    var type = Type.getType(entityTypeName);
    
    if (!type) {
        throw new Error('valid entity type name is required for dataSource');
    }

    for (var i = 0; i < this.length; i++) {
        if (this[i].get_type() !== type) {
            throw new Error('Datasource type mistmatch in source array.')
        }
    }
        
    this.makeObservable();
    
    Trait.Apply(this, DataSource, {
        asOriginal: ['add']
    });
    
    this.init(type, childTypes);
    this.__isDataSource = true;
    
    if(callback)
        this.load(callback);
    
    return this;
};

Auto.Properties(DataSource, [
    { name: 'count', autoEvent: true }
]);