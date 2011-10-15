Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.GridLayout = function (control) {
    Nimble.Core.LayoutEngines.GridLayout.constructBase(this, [ control ]);
    this._columnsOptions = (control.options['columns'] || [ '*' ]).makeObservable();
    this._rowsOptions = (control.options['rows'] || [ '*' ]).makeObservable();
    this._columns = [];
    this._rows = [];
    this._hspacing = control.options['hspacing']*1 || 0;
    this._vspacing = control.options['vspacing']*1 || 0;
    this.updateRowsAndColumns();

    this._rowsOptions.add_changed(this._initRows, this);
    this._columnsOptions.add_changed(this._initColumns, this);
};

Nimble.Core.LayoutEngines.GridLayout.prototype = {
    get_rows: function() {
        return this._rowsOptions;
    },

    get_columns: function() {
        return this._columnsOptions;
    },

     _getTargetRect: function(child) {
        var gridRow = this._getRowId(child);
        var gridColumn = this._getColumnId(child);

        return (function(obj) {
            return {
                get_width: function() {
                    return new DimensionUnit(obj._columns[gridColumn].innerWidth);
                },

                get_height: function() {
                    return new DimensionUnit(obj._rows[gridRow].innerHeight);
                },

                get_innerWidth: function() {
                    return obj._columns[gridColumn].innerWidth;
                },

                get_innerHeight: function() {
                    return obj._rows[gridRow].innerHeight;
                },

                get_offsetLeft: function() {
                    return obj._columns[gridColumn].offsetLeft;
                },
                
                get_offsetTop: function() {
                    return obj._rows[gridRow].offsetTop;
                }
            }
        })(this);
     },

     updateRowsAndColumns: function() {
        this._initRows();
        this._initColumns();
     },

     _initRows: function() {
        if (!isArray(this._rowsOptions)) {
            throw new Error('Rows is undefined in grid');
        }

        this._rowsStrechLevel = 0;

        for (var i = 0; i < this._rowsOptions.length; i++) {
            this._rows[i] = {
                height: new DimensionUnit(this._rowsOptions[i]),
                innerHeight: 0,
                offsetTop: 0
            }

            this._rowsStrechLevel += this._rows[i].height.getStretchLevel();
        }
     },

     _initColumns: function() {
        if (!isArray(this._columnsOptions)) {
            throw new Error('Columns is undefined in grid');
        }

        this._columnsStrechLevel = 0;

        for (var i = 0; i < this._columnsOptions.length; i++) {
            this._columns[i] = {
                width: new DimensionUnit(this._columnsOptions[i]),
                innerWidth: 0,
                offsetLeft: 0
            }

            this._columnsStrechLevel += this._columns[i].width.getStretchLevel();
        }
     },

     _getChilds: function() {
        return this._control.controls.getControlsInFlow();
     },

     _getChildsByColumnId: function(columnId) {
        var columnControls = [];
        var control = this._control;
        var columnId = columnId * 1;

        if (columnId < 0 || columnId > this._columns.length) {
            throw new Error('Wrong column identifier');
        }

        var childs = this._getChilds();

        for (var i = 0; i < childs.length; i++) {
            var child = childs[i];

            if (this._getColumnId(child) === columnId) {
                columnControls.add(child);
            }
        }

        return columnControls;
     },

     _calculateColumnAutoSizeWidth: function(columnId) {
        var controls = this._getChildsByColumnId(columnId);
        var maxWidth = 0;

        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];

            var width = control.get_outerWidth();

            if (control.get_width().isStretched()) {
                width = control.options.minWidth || 0;
            }

            if (width > maxWidth) {
                maxWidth = width;
            }
        }

        return maxWidth;
     },

     _updateColumnsClientWidth: function() {
        var stretchBase = 0,
            clientWidth =  this._control.get_width().isAutoSize() ? 0 : (this._control.get_innerWidth() - this._hspacing * (this._columns.length - 1)),
            usedWidth = 0,
            lastStretched = null;

        // First pass: fixed size columns
        for (var i = 0; i < this._columns.length; i++) {
            var columnWidth = this._columns[i].width;
            var width = 0;
            var isLast = i == this._columns.length - 1;

            if (columnWidth.isStretched()) {
                lastStretched = i;
                continue;
            } else if (columnWidth.isAutoSize()) {
                width = this._calculateColumnAutoSizeWidth(i);
                //throw new Error('Grid layout is not support auto-size');
            } else {
                width = Math.round(columnWidth.percent * clientWidth + columnWidth.pixels);
            }

            this._columns[i].innerWidth = width;
            usedWidth += width;
        }
        
        // Second pass: stretch
        if (this._columnsStrechLevel > 0) {
            stretchBase = (clientWidth - usedWidth) / this._columnsStrechLevel;
        }

        for (var i = 0; i < this._columns.length; i++) {
            var columnWidth = this._columns[i].width;
            
            if (columnWidth.isStretched()) {
                var width = 0;

                if (lastStretched != i) {
                    width = Math.round(columnWidth.getStretchLevel() * stretchBase);
                    usedWidth += width;
                } else {
                    width = clientWidth - usedWidth;
                }

                this._columns[i].innerWidth = Math.max(width, 0);
            }
            
            this._columns[i].offsetLeft = (i > 0 ? (this._columns[i - 1].innerWidth + this._columns[i - 1].offsetLeft + this._hspacing) : 0);
        }
     },

     _getRowId: function(child) {
        var gridRow = isDefined(child.options['grid.row']) ? (child.options['grid.row']*1 - 1) : 0;
        
        if (gridRow > this._rows.length || gridRow < 0) {
            throw new Error('"' + (gridRow + 1) + '" is not valid row number');
        }

        return gridRow;
     },

     _getColumnId: function(child) {
        var gridColumn = isDefined(child.options['grid.column']) ? (child.options['grid.column']*1 - 1) : 0;
        
        if (gridColumn > this._columns.length || gridColumn < 0) {
            throw new Error('"' + (gridColumn + 1) + '" is not valid column number');
        }

        return gridColumn;
     },

     _getChildsByRowId: function(rowId) {
        var rowControls = [];
        var control = this._control;
        var rowId = rowId * 1;

        if (rowId < 0 || rowId > this._rows.length) {
            throw new Error('Wrong row identifier');
        }

        var childs = this._getChilds();

        for (var i = 0; i < childs.length; i++) {
            var child = childs[i];

            if (this._getRowId(child) === rowId) {
                rowControls.add(child);
            }
        }

        return rowControls;
     },

     _calculateRowAutoSizeHeight: function(rowId) {
        var controls = this._getChildsByRowId(rowId);
        var maxHeight = 0;

        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];
            var height = control.get_outerHeight();

            if (control.get_height().isStretched()) {
                height = control.options.minHeight || 0;
            }

            if (height > maxHeight) {
                maxHeight = height;
            }
        }

        return maxHeight;
     },

     _updateRowsClientHeight: function() {
        var stretchBase = 0,
            clientHeight = this._control.get_height().isAutoSize() ? 0 : (this._control.get_innerHeight() - this._vspacing * (this._rows.length - 1)),
            usedHeight = 0,
            lastStretched = null;

        // First pass: fixed size rows
        for (var i = 0; i < this._rows.length; i++) {
            var rowHeight = this._rows[i].height;
            var height = 0;

            if (rowHeight.isStretched()) {
                lastStretched = i;
                continue;
            } else if (rowHeight.isAutoSize()) {
                height = this._calculateRowAutoSizeHeight(i);
            } else {
                height = Math.round(rowHeight.percent * clientHeight + rowHeight.pixels);
            }

            this._rows[i].innerHeight = height;
            usedHeight += height;
        }

        // Second pass: stretch
        if (this._rowsStrechLevel > 0) {
            stretchBase = (clientHeight - usedHeight) / this._rowsStrechLevel;
        }

        for (var i = 0; i < this._rows.length; i++) {
            var rowHeight = this._rows[i].height;
            
            if (rowHeight.isStretched()) {
                if (lastStretched != i) {
                    height = Math.round(rowHeight.getStretchLevel() * stretchBase);
                    usedHeight += height;
                } else {
                    height = clientHeight - usedHeight;
                }

                this._rows[i].innerHeight = Math.max(height, 0);
            }

            this._rows[i].offsetTop = (i > 0 ? (this._rows[i - 1].innerHeight + this._rows[i - 1].offsetTop + this._vspacing) : 0);
        }
     },

     recalculateClientWidth: function() {
        var isAutoSize = this._control.get_width().isAutoSize();

        if (!isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientWidth");
        }

        this._updateColumnsClientWidth();

        if (isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientWidth");
        }
     },

    _getChildClientMeasure: function(child, measure) {
        if (!child['get_' + measure]().isAutoSize()) {
            if (measure == 'height') {
                this._updateRowsClientHeight();
            }

            if (measure == 'width') {
                this._updateColumnsClientWidth();
            }
        }

        return Nimble.Core.LayoutEngines.GridLayout.callBase(this, "_getChildClientMeasure", [ child, measure ]);
    },
     
     recalculateClientHeight: function() {
        var isAutoSize = this._control.get_height().isAutoSize();

        if (!isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientHeight");
        }

        this._updateRowsClientHeight();

        if (isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientHeight");
        }
     },

    _isLastStretched: function(child, measure) {
        return true;
    },

    _getChildStretchedMeasure: function(child, measure) {
        var rect = this._getTargetRect(child);
        var getters = this._getGetters(measure);
            
        return rect[getters['getInnerMeasure']]() - child.get_margin()[getters['getMeasure']]();
    },
    
    _getChildsWidth: function() {
        var innerWidth = 0;

        for (var i = 0; i < this._columns.length; i++) {
            innerWidth += this._columns[i].innerWidth + this._hspacing;
        }

        var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();

        return innerWidth - this._hspacing + offset; // 3 columns, 2 spacings
    },

    _getChildsHeight: function() {
        var innerHeight = 0;

        for (var i = 0; i < this._rows.length; i++) {
            innerHeight += this._rows[i].innerHeight + this._vspacing;
        }

        var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();

        return innerHeight - this._vspacing + offset; // 3 rows, 2 spacings
    },

    get_domWidth: function() {
        this._updateColumnsClientWidth();
        return this._getChildsWidth();
    },
    
    get_domHeight: function() {
                this._updateRowsClientHeight();
        return this._getChildsHeight();
    },

    get_childPosition: function(childControl) {
        var rect = this._getTargetRect(childControl);
        var pos = { x: rect.get_offsetLeft(), y: rect.get_offsetTop() };
        var halign = childControl.options.halign;
        var valign = childControl.options.valign;

        if (halign && halign != 'left') {
            var dw = rect.get_innerWidth() - childControl.get_clientWidth();
            pos.x += halign == 'center' ? Math.round(dw / 2) : dw;
        }

        if (valign && valign != 'top') {
            var dh = rect.get_innerHeight() - childControl.get_clientHeight();
            pos.y += valign == 'middle' ? Math.round(dh / 2) : dh;
        }

        return pos;
    },

    postUpdate: function() {
        var controls = this._getChilds();

        for (var i = 0; i < controls.length; i++) {
            this._updateChildDom(controls[i]);
        }
    },

    updateDom: function() {
        Nimble.Core.LayoutEngines.GridLayout.callBase(this, "updateDom");
        this._control.domElement.style.position = 'relative';
    },

    _updateChildDom: function(child) {
        var position = child.get_layoutEngine().get_position();

        child.domElement.style.position = 'absolute';
        child.domElement.style.left = (position.x + this._control.get_padding().left) + 'px';
        child.domElement.style.top = (position.y + this._control.get_padding().top) + 'px';
    }
};

Nimble.Core.LayoutEngines.GridLayout.createClass('Nimble.Core.LayoutEngines.GridLayout', Nimble.Core.LayoutEngines.BaseLayout);
Nimble.Core.LayoutEngines.LayoutFactory.register('grid', Nimble.Core.LayoutEngines.GridLayout);