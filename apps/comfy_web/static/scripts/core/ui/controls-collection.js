var ControlsCollection = {
    _owner: null,
    _controlsInFlow: null,
    _itemOptions: null,
    eventsEnabled: null,

    // ======== Properties ========
    _addItem: function (item, options) {
        var collection = this;

        for (var i = collection.length - 1; i >= 0; i--) {
            if (collection[i] === item) {
                this._unregisterControl(item);
                collection.removeAt(i);
                this._itemOptions.removeAt(i);
                break;
            }
        }

        this._registerControl(item);
        var idx = collection.length;

        // find a new place if previous control is passed
        if (options) {
            if (!isUndefined(options.prevControl)) {
                if (options.prevControl === null) {
                    idx = 0;
                } else {
                    for (var i = 0; i < collection.length; i++) {
                        if (collection[i] === options.prevControl) {
                            idx = i + 1;
                            break;
                        }
                    }
                }
            }
        }

        while (idx > 0 && this._itemOptions[idx - 1] && this._itemOptions[idx - 1].alwaysLast) {
            idx--;
        }

        while (idx < collection.length && this._itemOptions[idx] && this._itemOptions[idx].alwaysFirst) {
            idx++;
        }

        collection.insert(idx, item);
        this._itemOptions.insert(idx, options);

        if (this.eventsEnabled) {
            this.raise_added({
                control: item,
                prevControl: idx > 0 ? collection[idx - 1] : null,
                nextControl: idx < collection.length - 1 ? collection[idx + 1] : null
            });
        }
        
        this._controlsInFlow = null;
    },

    _registerControl: function (control) {
        var owner = this._owner;

        if (control.id) {
            if (owner[control.id]) {
                throw new Error('[ControlsCollection]: Conflict with property "' + control.id + '" was occured or control with that id had registered already');
            }

            owner[control.id] = control;
        }
    },

    _unregisterControl: function (control) {
        var owner = this._owner;

        if (control.id) {
            delete owner[control.id];
        }
    },

    // ======== Methods ========
    add: function (control, options) {
        this._addItem(control, options);

        if (this.eventsEnabled) {
            this.raise_modified();
        }
    },

    addRange: function (controls, optionsArr) {
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];
            var options = optionsArr ? optionsArr[i] : null;

            this._addItem(control, options);
        }

        if (this.eventsEnabled) {
            this.raise_modified();
        }
    },

    remove: function (control) {
        this._unregisterControl(control);

        var idx = this.indexOf(control);

        if (idx >= 0) {
            this.splice(idx, 1);
            this._itemOptions.removeAt(idx);
        } else {
            throw new Error('Control not found in child collection');
        }

        if (this.eventsEnabled) {
            this.raise_removed({ control: control });
            this.raise_modified();
        }
        
        this._controlsInFlow = null;
    },

    removeRange: function (controls) {
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];

            this._unregisterControl(control);

            var idx = this.indexOf(control);

            if (idx >= 0) {
                this.splice(idx, 1);
                this._itemOptions.removeAt(idx);
            } else {
                throw new Error('Control not found in child collection');
            }

            this.raise_removed({ control: control });
        }

        if (this.eventsEnabled) {
            this.raise_modified();
        }
        
        this._controlsInFlow = null;
    },

    clear: function () {
        for (var i = 0; i < this.length; i++) {
            var control = this[i];
            this.raise_removed({ control: control });
            this._unregisterControl(control);
        }

        this._controlsInFlow = null;

        if (this.eventsEnabled) {
            this.raise_modified();
        }
    },

    contains: function (control) {
        return this.indexOf(control) >= 0;
    },

    getControlsInFlow: function () {
        if (!this._controlsInFlow) {
            var result = [];

            for (var i = 0; i < this.length; i++) {
                if (this[i].isInDocumentFlow()) {
                    result.add(this[i]);
                }
            }

            this._controlsInFlow = result;
        }

        return this._controlsInFlow;
    },

    first: function () {
        return this[0] || null;
    },

    last: function () {
        return this[this.length - 1] || null;
    }
};

Auto.Events(ControlsCollection, [
    'added',
    'removed',
    'modified'
]);

Array.prototype.makeControlsCollection = function(owner) {
    if (this.__controlsCollection) {
       return this; 
    }
    
    Object.extend(this, ControlsCollection);
    this.__controlsCollection = true;
    this._itemOptions = [];
    this._owner = owner;
    this.eventsEnabled = true;
    
    return this;
};