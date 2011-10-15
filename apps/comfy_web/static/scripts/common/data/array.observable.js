Observable = {
    add: function(input, args) {
        var items = (input instanceof Array) ? input : [input];        
               
        if (items.length == 0) {
            return;
        }
        
        this.raise_added(this.__createAddedArguments(items));
        this.raise_changed({ 
                added: items,
                removed: []
            }, args);
    },
    
    insert: function(index, input, args) {
        var items = (input instanceof Array) ? input : [input];
        
        if (items.length == 0) {
            return;
        }

        this.raise_added(this.__createAddedArguments(items));
        this.raise_changed({ 
                added: items,
                removed: []
            }, args);
    },
    
    remove: function(input, args) {
        var items = (input instanceof Array) ? input : [input];
    
        if (items.length == 0) {
            return;
        }

        this.raise_removed({items: items});
        this.raise_changed({ 
                added: [],
                removed: items
            }, args);
    },
    
    exclude: function(input, args) {
        var items = args;
    
        if (items.length == 0) {
            return;
        }

        this.raise_removed({items: items});
        this.raise_changed({ 
                added: [],
                removed: items
            }, args);
    },

    clear: function(args) {
        if(this.length === 0)
            return;
    
        var items = [];
        
        for(var i=0; i < this.length; i++ )
            items.add(this[i]);

        this.splice(0, this.length);
        //arguments.callee.original();
        
        this.raise_removed({items: items});
        this.raise_changed({ 
                added: [],
                removed: items
            }, args);
    },

    /**
    * It used in attach/detach functions to observe another collection.
    */
    __attachDelegate: function(sender, args) {
        this.remove_changed(this.__attachDelegate, sender);

        if (args.added.length > 0) {
            this.add(args.added);
        }
        
        if (args.removed.length > 0) {
            this.remove(args.removed);
        }
        
        this.add_changed(this.__attachDelegate, sender);
    },

    /**
    * Synchronize with @observable collection and subscribe to changing event of target collection.
    * When target collection is updated, current collection will be updated too.
    * @param synchronize: it's true by default
    */
    attach: function(observable, synchronize) {
        if (!isDefined(synchronize) || synchronize) {
            this.synchronize(observable);
        }

        observable.add_changed(this.__attachDelegate, this);
        this.add_changed(this.__attachDelegate, observable);
    },
    
    /**
    * Unsubscribe from changing event of target collection.
    */
    detach: function(observable) {
        observable.remove_changed(this.__attachDelegate, this);
        this.remove_changed(this.__attachDelegate, observable);
    },

    __exclusiveAttachDelegate: function(sender, args) {
        this.remove_changed(this.__exclusiveAttachDelegate, sender);

        if (args.added.length > 0) {
            for (var i = 0; i < args.added.length; i++) {
                if (this.contains(args.added[i])) {
                    this.remove(args.added[i]);
                }
            }
        }
        
        if (args.removed.length > 0) {
            for (var i = 0; i < args.removed.length; i++) {
                if (!this.contains(args.removed[i])) {
                    this.add(args.removed[i]);
                }
            }
        }
        
        this.add_changed(this.__exclusiveAttachDelegate, sender);
    },

    /**
    * Works like the "attach" method but when one item removed from the first collection it moves to the second one
    */
    exclusiveAttach: function(observable) {
        for (var i = observable.length - 1; i >= 0; i--) {
            if (this.contains(observable[i])) {
                observable.remove(observable[i]);
            }
        }

        observable.add_changed(this.__exclusiveAttachDelegate, this);
        this.add_changed(this.__exclusiveAttachDelegate, observable);
    },

    /**
    * Unsubscribe from changing event of target collection.
    */
    exclusiveDetach: function(observable) {
        observable.remove_changed(this.__exclusiveAttachDelegate, this);
        this.remove_changed(this.__exclusiveAttachDelegate, observable);
    },

    __createAddedArguments: function(items) {
        var args = {
            items: [],
            after: []
        };
        
        var processed = [];

        for (var i = 0; i < items.length; i++) {            
            processed.add({item: items[i], index: this.indexOf(items[i])});
        }

        processed.sort(function(a, b) {
            return a.index == b.index ? 0 : (a.index > b.index ? 1 : -1);
        });
        
        for (var i = 0; i < processed.length; i++) {
            args.items.add(processed[i].item);
            args.after.add(processed[i].index === 0 ? null : this[processed[i].index - 1]);
        }
        
        return args;
    }
};

Auto.Events(Observable, [
    'added',
    'removed',
    'changed'
]);

Auto.ApplyEventDisposing(Observable);

Array.prototype.makeObservable = function() {
    if(this.__observable)
       return this; 
    
    Trait.Apply(this, Observable, { asOriginal: ['clear'] });
    this.__observable = true;
    
    return this;
};