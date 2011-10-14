Phoenix.UI.DataGrid.Selectable = {
    set_dataSource: function() {
        this._selectedItems.clear();
    },
    
    _dataSource_itemAdded: function(sender, args) {
        this._checkHeaderSelection();
        this.updateSelection(args.items, []);
    },

    _dataSource_itemRemoved: function(sender, args) {
        var toRemove = this._selectedItems.where(function(item) { return args.items.contains(item); } );
        
        //this._selectedItems.remove(toRemove);
        
        this._checkHeaderSelection();
        this.updateSelection([], args.items);
    },
    
    _buildRow: function(properties, id, cssClass, allowBinding, cellControlGetter, rowProperties, createdRow) {
        var cell = this._createEmptyCell({ cellType: id });
        cell.id = 'selectionCell';
        cell.width = '26px';
        
        if(createdRow.id !== 'newItemRow' && (createdRow.id !== 'header' || this._selectionMode !== 'single')) {
            var checkBox = {
                id: 'checkBox',
                type: 'checkbox',
                width: '13px',
                height: '14px',
                halign: 'center',
                valign: 'middle',
                bindings: {},
                onChanged: this._itemSelectionChanged.bind(this)
            };
            
            cell.controls.add(checkBox);
        }
        createdRow.controls.insert(0, cell);
    },
    
    _itemSelectionChanged: function(sender) {
        var row = sender.parent.parent;
        var selectionState = sender.get_state();        

        if(row.id === 'header') {
            this.changeAllSelection(selectionState);            
            return;
        }

        if (this._selectionMode == 'single') {
            this._selectedItems.clear();
        }
        
        if(selectionState) {
            this._selectedItems.add(row.get_dataSource());
        } else if (this._selectionMode == 'multiple') {
            this._selectedItems.remove(row.get_dataSource());
        }
        
        this._checkHeaderSelection();
    },
    
    updateSelection: function(added, removed) {
        var items = this._selectedItems;

        this.forEachRow(function(row) {
            if(!row.selectionCell)
                return;

            var checkBox = row.selectionCell.checkBox;
            var ds = row.get_dataSource();

            if (items.contains(ds)) {
                checkBox.set_state(CheckBoxStates.checked);
            } else {
                checkBox.set_state(CheckBoxStates.empty);
            }
        });
    },

    changeAllSelection: function(state) {
        var items = [];

        this.forEachRow(function(row) {
            if(!row.selectionCell)
                return;
        
            var checkBox = row.selectionCell.checkBox;
            
            if(checkBox.get_state() !== state)
                items.add(row.get_dataSource());
            
            checkBox.set_state(state);
        });
        
        if(state)
            this._selectedItems.add(items);
        else
            this._selectedItems.remove(items);
    },

    _selectedChanged: function(sender, args) {
        this.updateSelection();
    },

    // TODO: I think we should detach handlers on disposing too
    set_selectedItems: function(value) {
        if (this._selectedItems === value) {
            return;
        }

        this._detachSelectionHandlers();
        this._selectedItems = value;
        this._attachSelectionHandlers();
    },

    _detachSelectionHandlers: function() {
        if (this._selectedItems && this._selectedItems.__observable) {
            this._selectedItems.remove_changed(this._selectedChanged, this);
        }
    },

    _attachSelectionHandlers: function() {
        if (this._selectedItems) {
            this._selectedItems.add_changed(this._selectedChanged, this);
        }
    },
    
    _checkHeaderSelection: function() {
        if (this._selectionMode === 'multiple') {
            if (this._selectedItems.length == 0) {
                this._header.selectionCell.checkBox.set_state(CheckBoxStates.empty);
            } else if (this._selectedItems.length !== this._dataSource.length) {
                this._header.selectionCell.checkBox.set_state(CheckBoxStates.gray);
            }
            else {
                this._header.selectionCell.checkBox.set_state(CheckBoxStates.checked);
            }
        }
    }
};

Auto.Properties(Phoenix.UI.DataGrid.Selectable, [
    'selectedItems'
]);

Phoenix.UI.DataGrid.prototype.makeSelectable = function(options) {
    Trait.Apply(this, Phoenix.UI.DataGrid.Selectable);
    
    this.set_selectedItems([].makeObservable());
    this._selectionMode = options.selectionMode || 'multiple';
};