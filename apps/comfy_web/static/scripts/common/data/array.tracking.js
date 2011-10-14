Tracking = {
    isTrackingDto: function(dto) {
        return dto.__type.startsWith('TrackingCollection_');
    },

    add: function(input, args) {
        this.items.add(input);
    
        if(args && args.notTrackable)
            return;
    
        var items = input instanceof Array ? input : [input];
        
        items.forEach(function(item) {
            if(this.removed.contains(item))
                this.removed.remove(item);
            else
                this.added.add(item);
        }, this);
    },    
    
    remove: function(input, args) {
        this.items.remove(input);
    
        if(args && args.notTrackable)
            return;
            
        var items = input instanceof Array ? input : [input];
    
        items.forEach(function(item) {
            if(this.added.contains(item))
                this.added.remove(item);
            else
                this.removed.add(item); 
        }, this);
    }
};

Array.prototype.makeTracking = function(entityType) {
    if(this.__tracking)
       return this;
    
    Trait.Apply(this, Tracking);
    
    this.added = [];
    this.items = [];
    this.removed = [];
    
    this.__type = entityType ? 'TrackingCollection_' + entityType + ':#Phoenix.Infrastructure.Collections' : 'TrackingCollection:#Phoenix.Infrastructure.Collections';
    
    for(var i = 0; i < this.length; i++)
        this.items.add(this[i]);
    
    this.__tracking = true;
    
    return this;
};