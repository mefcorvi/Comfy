var BaseListDataControl = {
    _get_freeControls: function() {
        if (!this._freeControls) {
            this._freeControls = [];
        }
        
        return this._freeControls;
    },

    get_childsHash: function() {
        if (!this._childsHash) {
            this._childsHash = new Hashtable();
        }
        
        return this._childsHash;
    },

    set_dataSource: function(value) {
        if (this._dataSource === value) {
            return;
        }
        
        if (this._dataSource) {
            this._detachForwardHandlers(this._dataSource);
            this.clear();
        }
        
        var oldValue = this._dataSource;
        this._dataSource = value;
        
        if (this._dataSource) {
            this._attachForwardHandlers(value);

            this._attachHandlers();
        
            var newControls = [];
        
            for (var i = 0; i < value.length; i++) {
                newControls.add(this._createItem(value[i]));
            }
        
            this.controls.addRange(newControls);
        }

        this._checkEmptiness();

        this.raise_dataSourceChanged({newValue: value, oldValue: oldValue});
    },

    isEmpty: function() {
        if (!this._dataSource || this._dataSource.length == 0) {
            return true;
        }

        if (this._get_freeControls().length == this.controls.length) {
            return true;
        }

        return false;
    },
    
    /**
    * Returns child control which datasource equals to @dataItem
    */
    findByDataItem: function(dataItem) {
        return this.get_childsHash().get(dataItem);
    },
    
    _checkEmptiness: function() {
        if (this._emptyDataTemplate) {
            if (!(this._emptyDataTemplate instanceof Template)) {
                throw new Error('Empty data template must be an instance of Template');
            }
        
            if (!this._dataSource || this._dataSource.length == 0) {
                if (!this.__emptyDataControl) {
                    var control = this._emptyDataTemplate.instantiate();
                    control.container = this;
                    this.__emptyDataControl = control;
                    
                    this.controls.add(control);
                }
            } else if (this.__emptyDataControl) {
                this.__emptyDataControl.free();
                this.controls.remove(this.__emptyDataControl);
                this.__emptyDataControl = null;
            }
        }
    },
    
    _attachHandlers: function() {
        this._dataSource.add_added(this._dataSource_itemAdded, this);
        this._dataSource.add_removed(this._dataSource_itemRemoved, this);
    },

    _detachHandlers: function() {
        this._dataSource.remove_added(this._dataSource_itemAdded, this);
        this._dataSource.remove_removed(this._dataSource_itemRemoved, this);
    },
    
    _get_anchorElement: function() {
        return null;
    },
    
    _dataSource_itemAdded: function(sender, args) {
        this._checkEmptiness();

        var controls = [];
        var options = [];
        
        for (var i = 0; i < args.items.length; i++) {
            var prevDataItem = args.after[i];
            var prevControl = prevDataItem ? this.get_childsHash().get(prevDataItem) : this._get_anchorElement();
            
            controls.add(this._createItem(args.items[i]));
            options.add({ prevControl: prevControl });
        }
        
        this.controls.addRange(controls, options);
    },
    
    _dataSource_itemRemoved: function(sender, args) {
        this._removeItems(args.items);
        this._checkEmptiness();
    },
    
    _createItem: function(dataItem) {
        var freeControls = this._get_freeControls();
        var control;
        
        if (freeControls.length > 0) {
            control = freeControls.pop();
            //this.controls.add(control);
            control.set_dataSource(dataItem);
        } else {
            control = this.createItem(dataItem);
        }
    
        this.__onItemCreated(control, dataItem);
        this.get_childsHash().put(dataItem, control);

        return control;
    },
    
    _removeItem: function(dataItem) {
        var control = this.get_childsHash().get(dataItem);
        this.get_childsHash().remove(dataItem);
        
        this.controls.remove(control);

        if (true || this.options.cacheDisabled) {
            control.free();
        } else {
            control.set_dataSource(null);
            var freeControls = this._get_freeControls();
            freeControls.push(control);
        }
    },

    __onItemCreated: function(control, dataItem) {
        if (isFunction(this.options.onItemCreated)) {
            this.options.onItemCreated.bind(this)(this, { control: control, dataItem: dataItem });
        }
    },
    
    _removeItems: function(dataItems) {
        var freeControls = this._get_freeControls();
        var controls = [];

        for (var i = 0; i < dataItems.length; i++) {
            var dataItem = dataItems[i];
            
            var control = this.get_childsHash().get(dataItem);
            controls.add(control);
            this.get_childsHash().remove(dataItem);
        
            if (false && !this.options.cacheDisabled) {
                freeControls.push(control);
            }
        }

        this.controls.removeRange(controls);
        
        if (true || this.options.cacheDisabled) {
            for (var i = 0; i < controls.length; i++) {
                controls[i].free();
            }
        }
    },
    
    free: function() {
        if (this.__free) {
            return;
        }

        if (this.raise_onFree) {
            this.raise_onFree();
        }
    
        if (this._dataSource) {
            this._detachHandlers();

            this._detachForwardHandlers(this._dataSource);

            this._dataSource = null;
        }
        
        var keys = this.get_childsHash().keys();
        this._removeItems(keys);
        
        this.__emptyDataControl = null;
        
        var freeControls = this._get_freeControls();
        
        var controls = [];
        
        for (var i = 0; i < freeControls.length; i++) {
            if (!freeControls[i].__free) {
                controls.add(freeControls[i]);
            }
        }
               
        for (var i = 0; i < controls.length; i++) {
            controls[i].free();
        }
        
        this._freeControls.clear();
        
        //Type.getInstanceType(this).callBase(this, 'free');
    },
    
    clear: function() {
        if (this._dataSource) {
            this._detachHandlers();
            this._dataSource = null;
        }
        
        var keys = this.get_childsHash().keys();
        this._removeItems(keys);
        
        this._checkEmptiness();
    }
};