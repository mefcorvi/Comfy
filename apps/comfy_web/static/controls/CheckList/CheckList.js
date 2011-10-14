///
/// Warning! It seems that control is nowhere used
///

Type.createNamespace('Phoenix.UI');

Phoenix.UI.CheckList = function () {
    Phoenix.UI.CheckList.constructBase(this);
};

Phoenix.UI.CheckList.prototype = {
    initFromOptions: function (options) {
        var thisObj = this;

        options.layout = 'stack';
        options.orientation = 'vertical';

        options.bindings = {
            '*': 'container',
            '*.changed': function (sender, args) {
                this._processSelection();
            }
        };

        options.displayProperty = options.displayProperty || 'name';

        options.controls = [
            {
                id: 'container',
                type: 'panel',
                layout: 'stack',
                orientation: 'vertical',
                height: '100%',
                bindings: {
                    '*': 'itemsPanel'
                },
                controls: [
                    {

                        type: 'scrollablePanel', id: 'itemsPanel', width: '100%', height: '100%-29',
                        bindings: { '*': 'items' },

                        controls: [
                            { type: 'repeater', id: 'items', width: '100%',
                                template: {
                                    type: 'panel', width: '100%', height: '24',
                                    customFunctions: {
                                        select: function () { this.checkBox.set_state(1); },
                                        deselect: function () { this.checkBox.set_state(0); }
                                    },

                                    bindings: {
                                        '*': function (sender, args) {
                                            var value = args.newValue;
                                            this.checkBox.set_text(value['get_' + thisObj.options.displayProperty]());
                                        }
                                    },
                                    controls: [
                                        { type: 'checkbox', id: 'checkBox', width: '100%', height: 24,
                                            onChanged: function (sender) {
                                                if (this.get_state() === CheckBoxStates.checked)
                                                    thisObj.get_selectedItems().add(this.parent.get_dataSource(), { source: thisObj });
                                                else
                                                    thisObj.get_selectedItems().remove(this.parent.get_dataSource(), { source: thisObj });
                                            }
                                        }
                                    ]
                                }
                            }
                        ]
                    },
                    {
                        type: 'searchPane',
                        onSearch: function (sender, args) {
                            var filterQuery = args.query || '';
                            this.parent.get_dataSource().set_searchQuery(filterQuery, true);
                        }
                    }
                ]
            }
        ];

        this.add_selectedItemsChanged(this.__selectedItemsChanged.bind(this));

        Phoenix.UI.CheckList.callBase(this, "initFromOptions", [options]);
    },

    __selectedItemsChanged: function (sender, args) {
        if (args.oldValue && args.oldValue.__observable)
            args.oldValue.remove_changed(this.__selectedItems_onChanged, this);

        this._processSelection();

        if (args.newValue && args.newValue.__observable)
            args.newValue.add_changed(this.__selectedItems_onChanged, this);
    },

    __selectedItems_onChanged: function (sender, args, context) {
        if (context && context.source && context.source === this)
            return;

        var controls = this.container.itemsPanel.items.get_childsHash().values();

        controls
            .where(function (control) { return args.added.contains(control.get_dataSource()); })
            .forEach(function (control) { control.select(); })

        controls
            .where(function (control) { return args.removed.contains(control.get_dataSource()); })
            .forEach(function (control) { control.deselect(); })
    },

    _processSelection: function () {
        if (!this._dataSource || this._dataSource.length == 0)
            return;

        this._clearSelections();

        if (!this._selectedItems || this._selectedItems.length == 0) {
            return;
        }

        var repeater = this.container.itemsPanel.items;

        this._selectedItems.forEach(function (item) {
            var found = repeater.findByDataItem(item);

            if (found)
                found.select();
        });
    },

    _clearSelections: function () {
        this.container.itemsPanel.items.get_childsHash().values().forEach(function (item) {
            item.deselect();
        });
    },

    instantiateInDom: function (domElement) {
        Phoenix.UI.CheckList.callBase(this, "instantiateInDom", [domElement]);
    }
};

Auto.Properties(Phoenix.UI.CheckList.prototype, [
    { name: 'selectedItems', autoEvent: true }
]);

Phoenix.UI.CheckList.createClass('Phoenix.UI.CheckList', Phoenix.UI.Panel);
ControlsFactory.registerControl('checkList', Phoenix.UI.CheckList);