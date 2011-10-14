Type.createNamespace('Phoenix.UI');

Phoenix.UI.DataGrid = function() {
    Phoenix.UI.DataGrid.constructBase(this);
};

Phoenix.UI.DataGrid.prototype = {
    defaultOptions: {
        'width': '*'
    },
    
    initFromOptions: function (options) {
        if (!options.columnProperties)
            throw new Error('Column properties must be specified for dataGrid');

        options.padding = '0 3px 0 0';
        options.layout = 'stack';
        options.orientation = 'vertical';

        this.set_hiddenColumns((options.hiddenColumns || []).makeObservable());

        this.width = options.width || '25px';
        this.rowHeight = options.rowHeight || '25px';
        this.rowWidth = options.width == '?' ? '?' : '*';
        this.headerHeight = options.headerHeight || '25px';

        this.addCssClass('dataGrid');

        if (options.selectable)
            this.makeSelectable(options);

        if (options.editable)
            this.makeEditable(options);

        if (options.resizable)
            this.makeResizable(options);

        options.template = this._createRowTemplate(options.rowProperties, options.columnProperties);
        Phoenix.UI.DataGrid.callBase(this, "initFromOptions", [options]);

        this.set_header(this._createHeader(options.columnProperties));

        if (options.newItemResolver) {
            this.set_newItemPanelTemplate(options.newItemPanelTemplate || this._createNewItemPanelTemplate(options.columnProperties));

            this.set_newItemPanel(this._createNewItemPanel());
        }

        this.__onWindowClick_Bound = this.__onWindowClick.bind(this);
    },

    set_dataSource: function (value) {
        Phoenix.UI.DataGrid.callBase(this, "set_dataSource", [value]);
        
        if (value.__isOrdered) {
            this._setSortOrderMarks(value);
        }

        if (value && value.length > 0) {
            this._styleRows();
        }
    },

    _setSortOrderMarks: function(value) {
        if(value._orderByAndSortDirection !== undefined) {
            for (var i = 0; i < value._orderByAndSortDirection.length; i++) {
                var orderItem = value._orderByAndSortDirection[i];
                var orderPropName = orderItem.value.substr(1);
                
                for (var j = 0; j < this.header.options.controls.length; j++) {
                    var headerLabel = this.header.controls[j].headerLabel;

                    if (!headerLabel) {
                        continue;
                    }

                    if (orderPropName === headerLabel.options.sortName) {
                        if (orderItem.order === 'asc') {
                            headerLabel.set_cssClass('sort_up');
                        } else {
                            headerLabel.set_cssClass('sort_down');
                        }
                    }
                }
            }
        }
    },

    _clearSortOrderMarks: function(value) {
        for (var j = 0; j < this.header.options.controls.length; j++) {
            var headerLabel = this.header.controls[j].headerLabel;

            if (!headerLabel) {
                continue;
            }

            if(headerLabel.options == null) {
                continue;
            }

            if(headerLabel.options.sortName === undefined) {
                headerLabel.set_cssClass('');
            } else {
                headerLabel.set_cssClass('sort_no');
            }
        }
    },
        
    _dataSource_itemAdded: function (sender, args) {
        Phoenix.UI.DataGrid.callBase(this, "_dataSource_itemAdded", [sender, args]);

        this._styleRows();
    },

    _dataSource_itemRemoved: function (sender, args) {
        Phoenix.UI.DataGrid.callBase(this, "_dataSource_itemRemoved", [sender, args]);

        if (this._dataSource.length > 0)
            this._styleRows();
    },

    _createHeader: function (properties) {
        this.properties = properties;
        var row = this._buildRow(properties, 'header', 'dataGrid_header', false,
            function (propName, propValue) {
                return function(){
                    return {
                        id: 'headerLabel',
                        type: 'label',
                        width: '*',
                        height: '*',
                        text: propValue.headerText || '',
                        cssClass: propValue.sortName !== undefined ? 'sort_no' : '',
                        sortName: propValue.sortName,
                        domHandlers: propValue.sortName !== undefined ? {
                            click: function (element, arg) {
                                this._changeSortingOrder(element, propValue.sortName, arg.ctrlKey);
                                this.get_dataSource().load();
                            }.bind(this)
                        }: {}
                    };
                }.bind(this)();
            }.bind(this));

        row.height = this.headerHeight;

        var control = ControlsFactory.create('panel');
        control.initFromOptions(row);

        return control;
    },

    _getSortingOrderByItemName: function(itemName, dataSource) {
        for(var i = 0;i < dataSource._orderByAndSortDirection.length; i++) {
            if(dataSource._orderByAndSortDirection[i].value.substr(1) === itemName) {
                return dataSource._orderByAndSortDirection[i].order;
            }
        }
        return '';
    },
        
    _changeSortingOrder: function(element, itemName, isMultiColumnSorting) {
        var dataSource = this.get_dataSource();
        
        if (!dataSource.__isOrdered) {
            return;
        }

        var currentOrder = this._getSortingOrderByItemName(itemName,dataSource);

        if (isMultiColumnSorting) {
            switch (currentOrder) {
                case '':
                    element.set_cssClass('sort_up');
                    dataSource._orderByAndSortDirection.push({ value: '_' + itemName, order: 'asc' });
                    break;
                case 'asc':
                    element.set_cssClass('sort_down');
                    this._changeDataSourceSortingOrder(dataSource, itemName, 'desc');
                    break;
                case 'desc':
                    element.set_cssClass('sort_up');
                    this._changeDataSourceSortingOrder(dataSource, itemName, 'asc');
                    break;
            }
        } else {
            this._clearSortOrderMarks(dataSource);
            dataSource._orderByAndSortDirection = new Array();
            
            switch(currentOrder) {
                case 'asc':
                    element.set_cssClass('sort_down');
                    dataSource._orderByAndSortDirection.push({ value: '_' + itemName, order: 'desc' });
                    break;
                case 'desc':
                case '':
                    element.set_cssClass('sort_up');
                    dataSource._orderByAndSortDirection.push({ value: '_' + itemName, order: 'asc' });
                    break;
            }
        }

        var sortingString = this._composeSortingString(dataSource._orderByAndSortDirection);

        if (sortingString !== '') {
            dataSource.set_orderBy(sortingString);
        }
    },

    _changeDataSourceSortingOrder: function(dataSource, itemName, operationType) {
        for(var i=0;i<dataSource._orderByAndSortDirection.length;i++) {
            if(dataSource._orderByAndSortDirection[i].value.substr(1) === itemName) {
                switch (operationType) {
                    case '':
                        dataSource._orderByAndSortDirection.splice(i,1);
                        break;
                    case 'asc':
                        dataSource._orderByAndSortDirection[i].order = 'asc';
                        break;
                    case 'desc':
                        dataSource._orderByAndSortDirection[i].order = 'desc';
                        break;
                }
                break;
            }
        }
    },

    _composeSortingString: function(orderByAndSortDirection) {
        var orderArray = new Array();
        for(var i=0;i<orderByAndSortDirection.length;i++) {
            orderArray.push(orderByAndSortDirection[i].value.substr(1) + ' ' + orderByAndSortDirection[i].order);
        }
        return orderArray.join();
    },     

    _createRowTemplate: function (rowProperties, properties) {
        return this._buildRow(properties, null, null, true, function (propName, propValue) {
            return propValue.template || {
                type: 'label',
                height: this.rowHeight == '?' ? '?' : '*',
                width: '*',
                text: '',
                multiline: propValue.multiline,
                format: propValue.format
            };
        }.bind(this), rowProperties);
    },

    _buildRow: function (properties, id, cssClass, allowBinding, cellControlGetter, rowProperties) {

        var that = this;

        var row = this._createEmptyRow(rowProperties);

        if (id != null && id != undefined)
            row.id = id;

        if (cssClass)
            row.cssClass += ' ' + cssClass;

        for (var name in properties) {
            if (typeof properties[name] === 'function')
                continue;

            properties[name].cellType = id;

            var cell = this._createEmptyCell(properties[name]);
            cell.id = name + 'Cell';
            cell.controls.add(cellControlGetter(name, properties[name]));

            if (this._hiddenColumns.contains(name))
                cell.visible = false;

            row.controls.add(cell);

            if (allowBinding) {
                var bindingName = properties[name].custom ? '*' : name;

                if (!row.bindings[bindingName]) {
                    row.bindings[bindingName] = function (sender, args) {
                        var functions = arguments.callee.__bindingsArray;

                        for (var i = 0; i < functions.length; i++) {
                            functions[i].apply(this, [sender, args]);
                        }
                    };
                }

                if (!row.bindings[bindingName].__bindingsArray) {
                    row.bindings[bindingName].__bindingsArray = [];
                }

                row.bindings[bindingName].__bindingsArray.add((function (n) {
                    return function (sender, args) {
                        this[n + 'Cell'].controls.last().set_dataSource(args.newValue);

                        if (properties[n].showTooltip) {
                            this[n + 'Cell'].set_tooltip(args.newValue);
                        }
                    };
                })(name));
            }
        }

        return row;
    },

    _createEmptyRow: function (rowOptions) {
        var row = {
            type: 'panel',
            orientation: 'horizontal',
            cssClass: 'dataGrid_row',
            width: this.rowWidth,
            height: this.rowHeight,
            controls: [],
            bindings: {}
        };

        if (rowOptions) {
            for (var prop in rowOptions) {
                if (rowOptions.hasOwnProperty(prop)) {
                    row[prop] = rowOptions[prop];
                }
            }
        }

        return row;
    },

    _createEmptyCell: function (options) {
        var cell = {
            type: 'panel',
            cssClass: 'dataGrid_cell',
            height: '100%',
            padding: '3px',
            border: (options && options.cellType == 'header') ? '1px 1px 1px 0px' : '1px 1px 0px 0px',
            controls: [],
            bindings: {}
        };

        if (options) {
            if (options.width)
                cell.width = options.width;

            if (options.minWidth)
                cell.minWidth = options.minWidth;

            if (options.maxWidth)
                cell.maxWidth = options.maxWidth;

            if (options.cssClass)
                cell.cssClass += ' ' + options.cssClass;
        }

        return cell;
    },

    _createNewItemPanelTemplate: function (properties) {
        var row = this._buildRow(properties, 'newItemRow', null, true, function (propName, propValue) {
            var control = propValue.createTemplate || propValue.editTemplate || {
                type: propValue.readOnly || propValue.custom ? 'label' : 'textBox',
                width: '95%',
                text: '',

                validation: propValue.validation
            };

            if (propValue.custom)
                control.bindings = {};

            return control;
        });

        var editCell = row.controls.last();

        if (editCell.id !== 'editCell')
            throw new Error('Edit mode must be active for new row creation');

        editCell.cssClass += ' editMode';

        editCell.domHandlers = {
            mouseover: function (sender, args) {
                sender.addCssClass('hover');
            } .bind(this),

            mouseout: function (sender, args) {
                sender.removeCssClass('hover');
                sender.removeCssClass('pressed');
            } .bind(this),

            mousedown: function (sender, args) {
                sender.addCssClass('pressed');
            } .bind(this),

            mouseup: function (sender, args) {
                sender.removeCssClass('pressed');
            } .bind(this),

            click: function (sender, args) {
                this._newItem_onEditComplete();
            } .bind(this)
        };

        return new Template(row);
    },

    _createNewItemPanel: function () {
        var panel = ControlsFactory.create('panel');

        panel.initFromOptions({
            height: '25px',
            cssClass: 'dataGrid_addItemPanel',
            controls: [
                {
                    type: 'label',
                    text: 'Add new item',
                    cssClass: 'plusImage'
                }
            ],
            domHandlers: {
                click: function (sender, args) {
                    args.stopPropagation();

                    var dataItem = this.options.newItemResolver();

                    var editRow = this._newItemPanelTemplate.instantiate(dataItem, this);

                    this.__clearActiveStates();
                    this.set_newItemRow(editRow);
                    this.get_window().attachDomHandler('click', this.__onWindowClick_Bound);
                } .bind(this),
                mouseenter: function (sender, arguments) {
                    sender.addCssClass('hover')
                },
                mouseleave: function (sender, arguments) {
                    sender.removeCssClass('hover')
                }
            }
        });

        return panel;
    },

    _newItem_onEditComplete: function () {
        var dataItem = this._newItemRow.get_dataSource();

        var hasErrors = false;

        for (var prop in this.options.columnProperties) {
            if (this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                continue;

            var control = this._newItemRow[prop + 'Cell'].controls.first();

            if (control.validate) {
                control.validate();

                if (!control.get_isValid())
                    hasErrors = true;
                else
                    dataItem['set_' + prop](control.get_dataSource());
            }
            else {
                dataItem['set_' + prop](control.get_dataSource());
            }
        }

        if (hasErrors)
            return;

        this.set_newItemRow(null);
        this.raise_newItemCreated(dataItem);
    },

    forEachRow: function (action) {
        var startIndex = this._header ? 1 : 0;
        var endIndex = this._newItemPanel ? this.controls.length - 2 : this.controls.length - 1;

        if (endIndex < startIndex)
            return;

        for (var i = startIndex; i <= endIndex; i++) {
            var row = this.controls[i];

            action(row, i - startIndex);
        }
    },

    set_header: function (value) {
        if (this._header === value)
            return;

        if (this._header) {
            this._header.free();
            this.controls.remove(this._header);
        }

        this._header = value;

        if (!value)
            return;

        this.controls.add(value, { prevControl: null, alwaysFirst: true });
    },

    set_newItemPanel: function (value) {
        if (this._newItemPanel === value)
            return;

        if (this._newItemPanel) {
            this._newItemPanel.free();
            this.controls.remove(this._newItemPanel)
        }

        this._newItemPanel = value;

        if (!value)
            return;

        this.controls.add(value, { prevControl: this.controls[this.controls.length] - 1 });
    },

    set_newItemRow: function (row) {
        if (this._newItemRow === row)
            return;

        if (this._newItemRow) {
            this.controls.remove(this._newItemRow);
            this._newItemRow.free();
            this._newItemRow = null;
        }

        if (!row) {
            this._newItemPanel.show(true);
        }
        else {
            this._newItemPanel.hide();

            row.parent = this;
            this.controls.add(row);
        }

        this._newItemRow = row;
    },

    set_hiddenColumns: function (value) {
        if (value === this._hiddenColumns)
            return;

        var oldValue = this._hiddenColumns;

        if (oldValue) {
            if (value)
                oldValue.synchronize(value);

            oldValue.remove_changed(this.__hiddenColumns_Changed, this);
        }

        this._hiddenColumns = value;

        if (this._hiddenColumns)
            this._hiddenColumns.add_changed(this.__hiddenColumns_Changed, this);
    },

    _get_anchorElement: function () {
        return this._header || null;
    },

    _styleRows: function () {
        this.forEachRow(function (row, rowIndex) {
            if (rowIndex % 2) {
                row.removeCssClass('odd');
            } else {
                row.addCssClass('odd');
            }
        });
    },

    __onWindowClick: function (sender, args) {
        if (
           (this._newItemRow && !jQuery.contains(this._newItemRow.domElement, args.target)) ||
           (this.__editRow && !jQuery.contains(this.__editRow.domElement, args.target))
          )
            this.__clearActiveStates();
    },

    __clearActiveStates: function () {
        if (this._newItemRow) {
            this.set_newItemRow(null);
            this.get_window().detachDomHandler('click', this.__onWindowClick_Bound);
        }

        if (this.__editRow) {
            this.__endEdit(false);
            this.get_window().detachDomHandler('click', this.__onWindowClick_Bound);
        }
    },

    __hiddenColumns_Changed: function (sender, args) {
        args.added.forEach(function (columnName) {
            this._changeColumnVisibility(columnName, false);
        }, this);

        args.removed.forEach(function (columnName) {
            this._changeColumnVisibility(columnName, true);
        }, this);

        this.update();
    },

    _changeColumnVisibility: function (columnName, visible) {      
        this.get_template().controls.single(function (control) {
            return control.id === columnName + 'Cell';
        }).visible = visible;

        this.header[columnName + 'Cell'].set_visible(visible);
        if (visible)
            this.header.update(this.header[columnName + 'Cell']);

        this.forEachRow(function (row) {
            row[columnName + 'Cell'].set_visible(visible);
            if(visible)
                row.update(row[columnName + 'Cell']);
        } .bind(this));
    }
};

Auto.Events(Phoenix.UI.DataGrid.prototype, [
    'newItemCreated'
]);

Auto.Properties(Phoenix.UI.DataGrid.prototype, [
    'header',
    'selectable',
    'newItemPanel',
    'newItemPanelTemplate',
    'newItemRow',
    'hiddenColumns'
]);

Phoenix.UI.DataGrid.createClass('Phoenix.UI.DataGrid', Phoenix.UI.Repeater);
ControlsFactory.registerControl('dataGrid', Phoenix.UI.DataGrid);
