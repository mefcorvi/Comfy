EntitySet = {
    init: function() {
        if (this.get_keyProperty()) {
            Repository['add_' + this._entityType.toCamelCase() + 'Saved'](this.__repository_entitySaved, this);
            Repository['add_' + this._entityType.toCamelCase() + 'Loaded'](this.__repository_entityLoaded, this);
        }

        Repository['add_' + this._entityType.toCamelCase() + 'Deleted'](this.__repository_entityDeleted, this);
    },
    
    __repository_entitySaved: function(sender, args) {
        this._synchronizeEntities(args.entities);
    },
    
    __repository_entityLoaded: function(sender, args) {
        this._synchronizeEntities(args.entities);
    },
    
    __repository_entityDeleted: function(sender, args) {
        var found = [];

        for (var i = 0; i < this.length; i++) {
            for (var j = 0; j < args.entities.length; j++) {
                if (args.entities[j] === this[i]) {
                    found.add(this[i]);
                    break;
                }
            }
        }

        if(found.length > 0)
            this.remove(found);
    },
    
    set_keyValue: function(value) {
        if(!this._keyProperty)
            return;
    
        if(this._keyValue === value)
            return;
            
        this._keyValue = value;
        
        if(!value)
            this.clear();
        
        var filtered = Repository.FilterCached(this._entityType, this.__entityFilter.bind(this));
        
        this.synchronize(filtered,  {notTrackable: true});
    },
    
    __entityFilter: function(entity) {
        return entity['get_' + this._keyProperty]() === this._keyValue;
    },
    
    _synchronizeEntities: function(entities) {
        var toRemove = entities.where(function(entity) {
            return !this.__entityFilter(entity) && this.contains(entity);
        }, this);
        
        this.remove(toRemove,  {notTrackable: true});
        
        var toAdd = entities.where(function(entity) {
            return this.__entityFilter(entity) && !this.contains(entity);
        }, this);
                
        this.add(toAdd, {notTrackable: true});
    },

    dispose: function() {
        if(!this._keyProperty)
            return;
    
        Repository['remove_' + this._entityType.toCamelCase() + 'Saved'](this.__repository_entitySaved, this);
        Repository['remove_' + this._entityType.toCamelCase() + 'Loaded'](this.__repository_entityLoaded, this);
        Repository['remove_' + this._entityType.toCamelCase() + 'Deleted'](this.__repository_entityDeleted, this);
    }
};

Auto.Properties(EntitySet, [
    'entityType',
    'keyProperty',
    'keyValue'
]);

Array.prototype.makeEntitySet = function(owner, entityType, keyProperty, keyValue) {
    if(this.__isEntitySet)
        return;

    var type = Type.getType(entityType);
    if(!type)
        throw new Error('Valid entity type name is required for EntitySet');
        
    this.makeObservable();
    this.makeTracking(entityType);

    Trait.Apply(this, EntitySet);
    
    this.set_entityType(entityType);
    this.__isEntitySet = true;
    
    this.set_keyProperty(keyProperty);
    
    this.init();
    
    if(keyValue)
        this.set_keyValue(keyValue);
        
    return this;
};