Type.createNamespace('Phoenix.UI');

Phoenix.UI.ListView = function() {
    Phoenix.UI.ListView.constructBase(this);
};

Phoenix.UI.ListView.prototype = {
    defaultOptions: {
        disabledCssClass: 'listview_disabled',
        width: '*',
        height: '*',  
        bindings: {
            '*' : 'selectedValue'
        }
    },

    set_dataSource: function(value) {
        Phoenix.UI.ListView.callBase(this, "set_dataSource", [ value ]);
    },

    getChildControlByValue: function(value) {
        return this._getChildControlByDataItem(this._getDataItemByValue(value));
    },

    _getChildControlByDataItem: function(dataItem) {
        if (!dataItem) {
            return null;
        }

        return this._getRepeater().findByDataItem(dataItem);
    },

    _getRepeater: function() {
        return this._scrollable._repeater;
    },

    _setSelectedItem: function(dataItem) {
        var selectedValue = this._getValueOfItem(dataItem);
        this.set_selectedValue(selectedValue);
    },

    _getValueOfItem: function(dataItem) {
        return dataItem;
    },

    _getDataItemByValue: function(value) {
        for (var i = 0; i < this._items.length; i++) {
            if (this._getValueOfItem(this._items[i]) == value) {
                return this._items[i];
            }
        }

        return null;
    },

    set_selectedValue: function(value) {
        if (this._selectedValue === value) {
            return;
        };

        var oldValue = this._selectedValue;
        this._selectedValue = value;

        var oldControl = this.getChildControlByValue(oldValue);
        var newControl = this.getChildControlByValue(value);

        if (oldControl) {
            oldControl.removeCssClass('item_selected');
        }

        if (newControl) {
            newControl.addCssClass('item_selected');
        }

        this.raise_selectedValueChanged({ newValue: value, oldValue: oldValue });
    },
    
    initFromOptions: function(options) {
        var thisObj = this;
        this.addCssClass('listview');

        if (isFunction(options.onItemClick)) {
            this.add_onItemClick(options.onItemClick, this);
        }

        options.template.domHandlers = {
            'mouseover': function() {
                this.addCssClass('item_hovered');
            },
            'mouseout': function() {
                this.removeCssClass('item_hovered');
            },
            'click': function() {
                var ds = this.get_dataSource();
                thisObj._setSelectedItem(ds);
                thisObj.raise_onItemClick({ item: ds, control: this });
            }
        };

        options.controls = [
            {
                id: '_scrollable',
                type: 'scrollablePanel',
                controls: [
                    {
                        id: '_repeater',
                        type: 'repeater',
                        height: '?',
                        template: options.template,
                        onLoad: function() {
                            this._link = Auto.Property.CreateOneWayLink(thisObj, this, "items", "dataSource");
                        },
                        onFree: function() {
                            this._link.dispose();
                        }
                    }
                ]
            }
        ];

        Phoenix.UI.ListView.callBase(this, "initFromOptions", [ options ]);
    }
};

Auto.Properties(Phoenix.UI.ListView.prototype, [
    { name: 'selectedValue', autoEvent: true },
    { name: 'items', autoEvent: true }
]);

Auto.Events(Phoenix.UI.ListView.prototype, [
    'onItemClick'
]);

Phoenix.UI.ListView.createClass('Phoenix.UI.ListView', Phoenix.UI.Panel);
ControlsFactory.registerControl('listView', Phoenix.UI.ListView);
