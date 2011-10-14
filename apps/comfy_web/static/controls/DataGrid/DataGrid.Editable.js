Phoenix.UI.DataGrid.Editable = {
    _buildRow: function(properties, id, cssClass, allowBinding, cellControlGetter, rowProperties, createdRow) {        
        var cell = this._createEmptyCell();
        cell.id = 'editCell';
        cell.cssClass += ' editCell'
        cell.width = '25px';
        
        
        if(createdRow.id !== 'header' && createdRow.id !== 'newItemRow')
            cell.domHandlers = {
                mouseover: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)                                
                        return;
                        
                    sender.addCssClass('hover');
                }.bind(this),
                
                mouseout: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)
                        return;
                
                    sender.removeCssClass('hover');
                    sender.removeCssClass('pressed');
                }.bind(this),
                
                mousedown: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)
                        return;
                
                    sender.addCssClass('pressed');
                }.bind(this),
                
                mouseup: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)
                        return;
                
                    sender.removeCssClass('pressed');
                }.bind(this),

                click: function(sender, args) {
                    args.stopPropagation();
                
                    if((this.__editRow && sender.parent !== this.__editRow) ||
                        this._newItemRow)
                        this.__clearActiveStates();                    
                    
                    if(!this.__editRow)
                        this.__toEditMode(sender.parent);
                    else
                        this.__endEdit(true);
                        
                }.bind(this)
            };

        createdRow.controls.add(cell);
    },
    
    __toEditMode: function(row) {
        this.__editRow = row;
    
        var ds = row.get_dataSource();
        
        for(var prop in this.options.columnProperties) {
            if(this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                continue;
                
            var cell = row[prop + 'Cell'];
            
            cell.controls.first().hide();
            
            var editControl = this.__createEditFrom(this.options.columnProperties[prop]);
            cell.controls.add(editControl, {prevControl: null});
            
            editControl.set_dataSource(ds['get_' + prop]());
        }
        
        var editCell = row.editCell;
        
        editCell.addCssClass('editMode');
        
        this.get_window().attachDomHandler('click', this.__onWindowClick_Bound);
    },
    
    __endEdit: function(applyChanges) { 
        var ds = this.__editRow.get_dataSource();

        if(applyChanges) {
            var hasErrors = false;
        
            for(var prop in this.options.columnProperties) {
                if(this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                    continue;

                var cell = this.__editRow[prop + 'Cell'];
                var editControl = cell.controls.first();

                if(editControl.validate) {
                    editControl.validate();
                    
                    if(!editControl.get_isValid())
                        hasErrors = true;
                }
            }
            
            if(hasErrors)
                return;
            
            for(var prop in this.options.columnProperties) {
                if(this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                    continue;
                
                var cell = this.__editRow[prop + 'Cell'];
                var editControl = cell.controls.first();
                
                ds['set_' + prop](editControl.get_dataSource());
            }
        }
        
        for(var prop in this.options.columnProperties) {
            if(this.options.columnProperties[prop].readOnly  || this.options.columnProperties[prop].custom)
                continue;
                
            var cell = this.__editRow[prop + 'Cell'];
            var editControl = cell.controls.first();
        
            editControl.free();  
            cell.controls.remove(editControl);  
                    
            cell.controls.last().show();
        }
        
        this.__editRow.editCell.removeCssClass('editMode');
        this.__editRow = null;
    },
    
    __createEditFrom: function(columnProperty) {
        if(columnProperty.editTemplate)
            return new Template(columnProperty.editTemplate).instantiate();

        var editControl =  ControlsFactory.create('textBox');

        var options = {
            width: columnProperty.width || '100%',
            validation: columnProperty.validation
        };

        editControl.initFromOptions(options);

        return editControl;
    },
    
    _dataSource_itemRemoved: function(sender, args) {
        if(this.__editRow && args.items.contains(this.__editRow.get_dataSource()))
            this.__editRow = null;
    }
};

Phoenix.UI.DataGrid.prototype.makeEditable = function() {
    Trait.Apply(this, Phoenix.UI.DataGrid.Editable);
};