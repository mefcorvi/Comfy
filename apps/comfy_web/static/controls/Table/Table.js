Type.createNamespace('Phoenix.UI');

Phoenix.UI.Table = function() {
    Phoenix.UI.Table.constructBase(this);
};

Phoenix.UI.Table.prototype = {
    initFromOptions: function(options) {
        if (!options.columns) {
            throw new Error('[Table]: Columns meta information isn\'t passed');
        }
        
        // check columns and create templates
        for (var i = 0; i < options.columns.length; i++) {
            var column = options.columns[i];
            
            if (!column.template) {
                throw new Error('[Table.Columns]: Template isn\'t passed');                
            }
            
            column.template = new Template(column.template);
        }
        
        this.set_columns(options.columns);
    
        Phoenix.UI.Table.callBase(this, "initFromOptions", [ options ]);
        
        this._appendHeader();
    },
    
    _appendHeader: function() {
        var row = ControlsFactory.create('container');
        row.initFromOptions({ tag: 'tr', cssClass: 'table_header_row' });
        
        for (var i = 0; i < this.options.columns.length; i++) {
            var column = this.options.columns[i];
            var cellContainer = ControlsFactory.create('container');
            cellContainer.initFromOptions({ tag: 'th', cssClass: 'table_header_cell' });
            
            var label = ControlsFactory.create('label');
            label.initFromOptions({ text: column.title });
            
            cellContainer.appendControl(label);
            row.appendControl(cellContainer);           
        }
        
        this.controls.add(row);
    },
    
	instantiateInDom: function(domElement) {
	    this.domElement = DOM.create('table', domElement, { cellPadding: 0, cellSpacing: 0 });
	    this._tbody = DOM.create('tbody', this.domElement);
        Phoenix.UI.Table.callBase(this, "instantiateInDom", [ domElement ]);
	},
    
    get_childsContainer: function() {
        return this._tbody;
    },
    
	createItem: function(dataItem) {
        var row = ControlsFactory.create('container');
        row.initFromOptions({ tag: 'tr', cssClass: 'table_row' });
        
        // instantiate cells templates in columns
        var columns = this.get_columns();
        
        for (var i = 0, len = columns.length; i < len; i++) {
            var column = columns[i];
            var cellContainer = ControlsFactory.create('container');
            cellContainer.initFromOptions({ tag: 'td', cssClass: 'table_cell' });
            
            if (column.cssClass) {
                cellContainer.addCssClass(column.cssClass);
            }
            
            row.controls.add(cellContainer);
            
            var cell = column.template.instantiate(dataItem, cellContainer);
            cell.container = this;
            
            cellContainer.controls.add(cell);
        }
	    
	    return row;
	}
};

Trait.Apply(Phoenix.UI.Table.prototype, BaseListDataControl);

Auto.Properties(Phoenix.UI.Table.prototype, [
    { name: 'columns' }
]);

Phoenix.UI.Table.createClass('Phoenix.UI.Table', Control);
ControlsFactory.registerControl('table', Phoenix.UI.Table);
