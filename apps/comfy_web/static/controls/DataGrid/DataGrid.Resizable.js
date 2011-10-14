Phoenix.UI.DataGrid.Resizable = {
    _border: { none: 0, left: 1, right: 2 },

    _resizableBorderSize: 5,

    _resizableColumnsSizes: {
        indexes: []
    },

    _buildRow: function (properties, id, cssClass, allowBinding, cellControlGetter, rowProperties, createdRow) {
        if (createdRow.id !== 'header')
            return;

        var that = this;

        var forPropFunc = function (propName) {
            var cellName = propName + 'Cell';
            var cell = createdRow.controls.single(function (c) { return c.id === cellName });

            cell.__hoveredBorder = that._border.none;

            cell.domHandlers = {
                mousemove: function (sender, args) {
                    if (this._isResizing) {
                        sender.addCssClass('resizable');
                        return;
                    }

                    var border = this._getHoveredBorder(sender, args);

                    if (border === sender.__hoveredBorder)
                        return;

                    if (border !== this._border.none) {
                        sender.addCssClass('resizable');
                    } else {
                        sender.removeCssClass('resizable');
                    }

                    sender.__hoveredBorder = border;
                } .bind(that),

                mousedown: function (sender, args) {
                    if (this._isResizing)
                        return;

                    if (sender.__hoveredBorder === this._border.none)
                        return;

                    this._beginResize(propName, sender.__hoveredBorder, args);
                } .bind(that)
            };

            that._resizableColumnsSizes['add_' + propName + 'WidthChanged'](function (sender, args) {
                var headerControl = this.header[propName + 'Cell'];

                this.get_template().controls.single(function (control) {
                    return control.id === propName + 'Cell';
                }).width = args.newValue.pixels;

                headerControl.set_width(args.newValue);
                this.header.update(headerControl);

                this.forEachRow(function (row, index) {
                    var control = row[propName + 'Cell'];

                    control.set_width(args.newValue);
                    row.update(control);
                }, this);
            }, that);
        };

        for (var prop in properties) {
            forPropFunc(prop);
        }
    },

    _getHoveredBorder: function (item, hoverArgs) {
        var itemWidth = item.get_clientWidth();

        if (0 <= hoverArgs.offsetX && hoverArgs.offsetX <= this._resizableBorderSize)
            return this._border.left;

        var delta = itemWidth - hoverArgs.offsetX;

        if (0 <= delta && delta <= this._resizableBorderSize)
            return this._border.right;

        return this._border.none;
    },

    _beginResize: function (columnName, border, args) {
        this._isResizing = true;
        this._resizeArgs = args;

        var columnIndex = this._resizableColumnsSizes.indexes.indexOf(columnName);
        var columnToResize = border === this._border.left ?
                this._getLeftVisibleColumnFrom(columnIndex - 1) :
                columnName;

        var onMouseMove = function (sender, args) {
            args.preventDefault();
            args.stopPropagation();

            if (columnToResize) {
                var delta = args.screenX - this._resizeArgs.screenX;

                var currentSize = this._resizableColumnsSizes['get_' + columnToResize + 'Width']();

                if (currentSize === undefined) {
                    currentSize = this.header[columnToResize + 'Cell'].get_width();
                }

                if (currentSize.pixels < -delta)
                    return;

                var newValue = currentSize.add(new DimensionUnit(delta));
                this._resizableColumnsSizes['set_' + columnToResize + 'Width'](newValue);

                this._resizeArgs = args;
            }
        } .bind(this);

        var onEndResize = function (sender, args) {
            this._isResizing = false;

            this.header.detachDomHandler('mousemove', onMouseMove);
            this.header.detachDomHandler('mouseup', onEndResize);
            //this.header.detachDomHandler('mouseout', onEndResize);

            if (columnToResize)
                this.raise_columnResized({ columnName: columnToResize, width: this._resizableColumnsSizes['get_' + columnToResize + 'Width']() });
        } .bind(this);

        this.header.attachDomHandler('mousemove', onMouseMove);
        this.header.attachDomHandler('mouseup', onEndResize);
        //this.header.attachDomHandler('mouseout', onEndResize);
    },

    _getLeftVisibleColumnFrom: function (index) {
        while (index >= 0 && this._hiddenColumns.contains(this._resizableColumnsSizes.indexes[index]))
            index--;

        return index >= 0 ? this._resizableColumnsSizes.indexes[index] : null;
    }
};

Auto.Event(Phoenix.UI.DataGrid.Resizable, 'columnResized');

Phoenix.UI.DataGrid.prototype.makeResizable = function (options) {
    if (this.__resizable)
        return;

    Trait.Apply(this, Phoenix.UI.DataGrid.Resizable);

    for (var prop in options.columnProperties) {
        Auto.Property(this._resizableColumnsSizes, { name: prop + 'Width', autoEvent: true });
        this._resizableColumnsSizes.indexes.add(prop);
    }

    this.__resizable = true;
};