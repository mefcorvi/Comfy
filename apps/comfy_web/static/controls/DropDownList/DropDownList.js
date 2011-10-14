Type.createNamespace('Phoenix.UI');

Phoenix.UI.DropDownList = function() {
    Phoenix.UI.DropDownList.constructBase(this);
    this.set_actionsBar([].makeObservable());
    this.add_selectedValueChanged(function() {
        this._updateSelectedItem();
    }, this);
};

Phoenix.UI.DropDownList.selectedItemCssClass = "dropDownItem_selected";

Phoenix.UI.DropDownList.prototype = {
    _textProperty: null,
    _valueProperty: null,
    _isShowed: false,
    _actionsBar: null,
    _stretchToMaxItem: false,
    _isDisabledBefore: false,
    _waitingForRightItem: false,
    _autoSelectDefaultValue: false,

    defaultOptions: {
        width: '*',
        height: 20,
        'border': '1',
        disabledCssClass: 'dropDownList_disabled',
        bindings: {
            '*': 'selectedValue'
        }
    },

    set_enabled: function (value) {
        Phoenix.UI.DropDownList.callBase(this, "set_enabled", [value]);

        if (!value && this._isShowed) {
            this.hideDropDown();
        }
    },

    initFromOptions: function (options) {
        var thisObj = this;
        var showActionPane = false;
        this._autoSelectDefaultValue = isNullOrUndefined(options.autoSelectDefaultValue) ? true : options.autoSelectDefaultValue;

        this.get_actionsBar().add_changed(this.__actionsBarChanged, this);

        this.addCssClass('dropDownList');
        this._textProperty = options.textProperty;
        this._valueProperty = options.valueProperty || "*";
        this._stretchToMaxItem = options.width == '?';
        this._notSelectedControl = null;

        if (isFunction(options.onChanged)) {
            this.add_onChanged(options.onChanged, this);
        }

        if (isFunction(options.onCommand)) {
            this.add_onCommand(options.onCommand, this);
        }

        if (isFunction(options.onClose)) {
            this.add_onClose(options.onClose, this);
        }

        if (isFunction(options.onOpen)) {
            this.add_onOpen(options.onOpen, this);
        }

        if (options.template) {
            this._template = options.template;
            this._bindingProperty = options.bindingProperty || '*';
        } else {
            if (options.bindingsProperty) {
                throw new Error("The \"bindingProperty\" cannot used when template was not defined");
            }

            /*
            if (!options.textProperty) {
                throw new Error("The \"textProperty\" is not specified");
            }*/

            this._bindingProperty = options.textProperty || '*';
            this._template = { type: 'label', padding: '3', width: '?', height: '22px' };
        }

        if (options.selectedItemTemplate) {
            this._selectedItemTemplate = options.selectedItemTemplate;
            this._selectedItemTemplate.id = 'dropdownItem';
        }

        if (isArray(options.actions)) {
            showActionPane = true;
        }

        this._template.id = 'dropdownItem';
        this._listHeight = options.listHeight*1 || 150;

        if (options.items && (options.items instanceof Array)) {
            var setDs = function () {
                this.set_items(options.items.makeObservable());
                this.remove_onLoaded(setDs);
            }

            this.add_onLoaded(setDs, this);
        }

        options.controls = [
            {
                id: 'dropDownPanel',
                type: 'panel',
                height: '?',
                visible: false,
                border: '1px',
                cssClass: 'dropDownList_container ' + (options.cssClass + '_container' || ''),
                onLoad: function () {
                    document.body.appendChild(this.domElement);
                },
                onFree: function () {
                    document.body.removeChild(this.domElement);
                },
                controls: [
                    {
                        id: 'scrollable',
                        type: 'scrollablePanel',
                        height: '?',
                        maxHeight: this._listHeight,
                        controls: [
                            {
                                type: 'repeater',
                                height: '?',
                                id: 'list',
                                cacheDisabled: true,
                                templateSelector: this._getTemplate.bind(this),
                                onLoad: function () {
                                    if (options.notSelectedItem) {
                                        thisObj.set_notSelectedItem(options.notSelectedItem);
                                    }
                                }
                            }
                        ]
                    }

                ]
            }
        ];

        Phoenix.UI.DropDownList.callBase(this, "initFromOptions", [options]);

        if (options.actionsBar) {
            this.get_actionsBar().add(options.actionsBar);
        }
    },

    // #region Actions bar

    __getIdForAction: function(actionName) {
        return 'action' + actionName.hashCode();
    },

    __actionsBarChanged: function(sender, args) {
        if (!this.get_actionsPane()) {
            this._initActionPane();
        }

        var actionsPane = this.get_actionsPane();

        for (var i = 0; i < args.added.length; i++) {
            var options = {
                id: this.__getIdForAction(args.added[i]),
                height: '23',
                padding: '3',
                border: '0 1 0 0',
                width: '*',
                type: 'link',
                text: args.added[i],
                onClick: (function(thisObj, actionName) {
                    return function() {
                        thisObj.raise_onCommand({ command: actionName });
                    };
                })(this, args.added[i])
            }

            var control = ControlsFactory.create('link');
            control.initFromOptions(options);

            actionsPane.controls.add(control);
        }

        for (var i = 0; i < args.removed.length; i++) {
            var control = actionsPane[this.__getIdForAction(args.removed[i])];
            actionsPane.controls.remove(control);
            control.free();
        }
    },

    get_actionsPane: function() {
        return this.dropDownPanel.actionPane;
    },

    _initActionPane: function () {
        var actionPane = {
            id: 'actionPane',
            type: 'panel',
            height: '?',
            border: '0 1 0 0',
            cssClass: 'actionPane',
            controls: []
        };

        var pane = ControlsFactory.create('panel');
        pane.initFromOptions(actionPane);

        this.dropDownPanel.controls.add(pane);
    },

    // #endregion

    /**
    * Returns template for the specified item
    * @args DataItem item - element of datasource
    * @args bool withHandlers - if it is true then onhover and onclick handlers will be attached
    */
    _getTemplate: function (item, withHandlers) {
        var dropDownList = this;
        var withHandlers = isNullOrUndefined(withHandlers) ? true : withHandlers;

        var obj = {
            type: 'panel',
            height: '?',
            cssClass: 'dropDownItem',
            controls: [],
            bindings: {}
        };

        var selectedValue = this.get_selectedValue(),
            selectedItem = this._getItemByValue(selectedValue),
            isShowedItem = !withHandlers && item && (item === selectedItem || (selectedItem === null && item === this._notSelectedItem)),
            template = null;

        if (isFunction(this.options.templateSelector)) {
            template = this.options.templateSelector(item, isShowedItem);
        }

        if (!template) {
            template = this._template;

            if (isShowedItem && this._selectedItemTemplate) {
                template = this._selectedItemTemplate;
            }
        }

        if (isShowedItem) {
            obj.height = '18px';
        }

        obj.controls.add(template);

        if (withHandlers) {
            obj.onLoad = function () {
                $(this.domElement)
                .hover(
                    function () { this.addCssClass('dropDownItem_hover'); } .bind(this),
                    function () { this.removeCssClass('dropDownItem_hover'); } .bind(this)
                )
                .click(function () {
                    var dataItem = this.get_dataSource(),
                        value = dropDownList._getValueOfItem(dataItem);
                    dropDownList.set_selectedValue(value);
                    dropDownList.hideDropDown();
                } .bind(this));
            };
        }

        obj.bindings[this._bindingProperty] = '*';

        return obj;
    },

    _getDataItemText: function (dataItem) {
        if (!this._textProperty) {
            return dataItem;
        }

        if (isFunction(dataItem['get_' + this._textProperty])) {
            return dataItem['get_' + this._textProperty]();
        }

        return dataItem[this._textProperty] || null;
    },

    set_notSelectedItem: function(dataItem) {
        this._notSelectedItem = dataItem;

        if (this._notSelectedControl) {
            this._notSelectedControl.free();
            this._getItemsRepeater().controls.remove(this._notSelectedControl);
            this._notSelectedControl = null;
        }

        if (dataItem) {
            this._attachNotSelectedItemControl();
        }

        if (!this.isEmpty()) {
            this._updateSelectedItem();
        }
    },

    _attachNotSelectedItemControl: function() {
        if (this._notSelectedItem) {
            var repeater =  this._getItemsRepeater();
            var notSelectedTemplate = new Template(this._getTemplate(this._notSelectedItem, true));
            this._notSelectedControl = notSelectedTemplate.instantiate(this._notSelectedItem, repeater);
            repeater.controls.add(this._notSelectedControl, { alwaysFirst: true, prevControl: null });
        } else {
            throw new Error("Not selected item is undefined");
        }
    },

    _getItemsRepeater: function () {
        return this.dropDownPanel.scrollable.list;
    },

    _getDropDownItem: function (dataItem) {
        if(dataItem) {
            var dropDownItem = this._getItemsRepeater().findByDataItem(dataItem);
            return (dataItem == this._notSelectedItem && this.supportsNotSelectedItem()) ? this._notSelectedControl : dropDownItem;
        }
        return null;
    },

    _doForAllItemControls: function(processFunc) {
        for (var i = 0; i < this._getItemsRepeater().controls.length; i++) {
            processFunc( this._getItemsRepeater().controls[i]);
        }
    },

    _removeAllItemControlsSelectionClasses: function() {
        this._doForAllItemControls(function(itemControl) { itemControl.removeCssClass(Phoenix.UI.DropDownList.selectedItemCssClass); });
    },

    /**
    * Finds the data item by its value
    */
    _getItemByValue: function(value) {
        var ds = this.get_items();

        if (!ds) {
            return null;
        }

        for (var i = 0; i < ds.length; i++) {
            var item = ds[i];

            if (this._getValueOfItem(item) === value) {
                return item;
            }
        }

        return null;

    },

    _getDropDownItemByValue: function(value) {
        var selectedItem = this._getItemByValue(value);
        return this._getDropDownItem(selectedItem);
    },

    /**
    * Returns value of data item
    */
    _getValueOfItem: function(item) {
        if (item === this._notSelectedItem) {
            return null;
        }

        var valueProperty = this._valueProperty;

        if (valueProperty == "*") {
            return item;
        }

        var getter = 'get_' + valueProperty;

        if (isFunction(item[getter])) {
            return item[getter]();
        } else if (item[valueProperty]) {
            return item[valueProperty];
        } else {
            throw new Error("Cannot get value of item. Unknown property \"" + valueProperty + "\" is declared in options of a dropdown list");
        }
    },

    /**
    * Returns selected data item
    */
    getSelectedDataItem: function() {
        return this._getItemByValue(this.get_selectedValue());
    },

    /**
    * Sets the dataItem as selected
    */
    setSelectedDataItem: function(dataItem) {
        this.set_selectedValue(this._getValueOfItem(dataItem));
    },

    scrollToItem: function (dataItem) {
        var control = this._getDropDownItem(dataItem);

        if (!control) {
            this.dropDownPanel.scrollable.scrollToTop();
            return;
        }

        var offsetTop = control.domElement.offsetTop;
        this.dropDownPanel.scrollable.set_scrollY(offsetTop);
    },

    findDataItemByText: function (namePart) {
        var ds = this.get_items();
        var namePart = namePart.toLowerCase();

        for (var i = 0; i < ds.length; i++) {
            var itemName = this._getDataItemText(ds[i]).toString().toLowerCase();

            if (itemName.startsWith(namePart)) {
                return ds[i];
            }
        }

        return null;
    },

    set_items: function (value) {
        if (this._items === value) {
            return;
        }

        oldValue = this._items;

        if (this._items) {
            this.clear();
        }

        this._items = value;
        this._getItemsRepeater().set_dataSource(value);

        if (this._items) {
            this._items.add_changed(this._items_itemChanged, this);
        }

        this._updateSelectedItem();
        this.__cachedMaxWidth = null;
        this.dropDownPanel.set_width('*');
        this.raise_itemsChanged({ newValue: value, oldValue: oldValue });
    },

    _items_itemChanged: function (sender, args) {
        this.__cachedMaxWidth = null;
        this.dropDownPanel.set_width('*');
        this._updateSelectedItem();
    },

    _getMaxItemWidth: function() {
        if (this.__cachedMaxWidth) {
            return this.__cachedMaxWidth;
        }

        var controls = this._getItemsRepeater().controls;
        var maxWidth = 0;

        for (var i = 0; i < controls.length; i++) {
            var width = controls[i].get_domWidth();
            
            if (maxWidth < width) {
                maxWidth = width;
            } 
        }

        this.__cachedMaxWidth = maxWidth;

        return maxWidth;
    },

    isEmpty: function() {
        var items = this.get_items();
        return !items || items.length == 0;
    },

    supportsNotSelectedItem: function() {
        return !!this._notSelectedItem;
    },

    _shouldWaitForItem: function() {
        var selectedValue = this.get_selectedValue(),
            selectedDataItem = this.getSelectedDataItem();

        var shouldUseNotSelected = selectedValue === null && this.supportsNotSelectedItem();

        if (!isUndefined(selectedValue) && !shouldUseNotSelected && !selectedDataItem && !this.isEmpty()) {
            return true;
        }
        
        if (this._waitingForRightItem) {
            this._cancelWaitForItem();
        }
    },

    _waitForItem: function() {
        if (!this._waitingForRightItem) {
            this._isDisabledBefore = !this.get_enabled();
            this._waitingForRightItem = true;
            this.set_enabled(false);
        }

        if (this.supportsNotSelectedItem()) {
            this._useNotSelectedItem();
        } else if (!this.isEmpty()) {
            this._removeActiveItem();
        }
    },

    _cancelWaitForItem: function() {
        this.set_enabled(this._isDisabledBefore ? false : true);
        this._waitingForRightItem = false;
    },

    _shouldSetDefaultItem: function() {
        var selectedDataItem = this.getSelectedDataItem();

        return !selectedDataItem && this._autoSelectDefaultValue && !this.supportsNotSelectedItem() && !this.isEmpty();
    },

    _setDefaultItem: function() {
        var items = this.get_items();
        this.setSelectedDataItem(items.first());
    },

    _shouldUseNotSelectedItem: function() {
        var selectedDataItem = this.getSelectedDataItem();

        return !selectedDataItem && this.supportsNotSelectedItem();
    },

    _useNotSelectedItem: function() {
        this._setActiveItem(this._notSelectedItem);
        this._setSelectedStyleToDataItem(this._notSelectedItem);
    },
    
    _updateSelectedItem: function() {
        if (this._shouldWaitForItem()) {
            this._waitForItem();
            return;
        }
        
        if (this._shouldUseNotSelectedItem()) {
            this._useNotSelectedItem();
            return;
        }
        
        if (this._shouldSetDefaultItem()) {
            this._setDefaultItem();
            return;
        }

        var selectedDataItem = this.getSelectedDataItem();
        this._setSelectedStyleToDataItem(selectedDataItem);
        this._setActiveItem(selectedDataItem);
    },

    _setSelectedStyleToDataItem: function(dataItem) {
        var selectedDropDownItem = this._getDropDownItem(dataItem);

        if (selectedDropDownItem && selectedDropDownItem.hasCssClass(Phoenix.UI.DropDownList.selectedItemCssClass)) {
            return;
        }

        this._removeAllItemControlsSelectionClasses();
        if (selectedDropDownItem) {
            selectedDropDownItem.addCssClass(Phoenix.UI.DropDownList.selectedItemCssClass);
        }
    },

    /**
    * Removes currently displayed item
    */
    _removeActiveItem: function() {
        if (this._selectedItemControl) {
            this._selectedItemControl.free();
            this.controls.remove(this._selectedItemControl);
            this._selectedItemControl = null;
        }
    },

    /**
    * Display an dataItem as a selected item of the dropdown list
    */
    _setActiveItem: function(dataItem) {
        if (this._selectedItemControl && this._selectedItemControl.get_dataSource() === dataItem) {
            return;
        }
        
        this._removeActiveItem();

        if (dataItem) {
            var templateOptions = this._getTemplate(dataItem, false);
            templateOptions.margin = '0 0 18 0'; // item shouldn't be placed above dropdown button's image
            this._selectedItemControl = new Template(templateOptions).instantiate(dataItem, this);
            this.controls.add(this._selectedItemControl);
        }
    },

    /**
    * Opens the dropdown list
    */
    showDropDown: function () {
        if (this._isShowed || !this.get_enabled()) {
            return;
        }

        this.__oldSelectedValue = this._selectedValue;

        var $element = $(this.domElement);
        var pos = $element.offset();

        var listPosition = pos;
        listPosition.top += this.get_clientHeight() - 1;

        this.raise_onOpen({ listPosition: listPosition });

        var style = this.dropDownPanel.domElement.style;
        var maxWidth = this._getMaxItemWidth() + 28;

        if (this._stretchToMaxItem || maxWidth > this.get_innerWidth()) {
            this.dropDownPanel.set_width(maxWidth);
        }

        this.dropDownPanel.show();
        style.left = listPosition.left + "px";
        style.top = listPosition.top + "px";
        style.zIndex = "70000";

        this.addCssClass('dropDownList_active');

        if (!this._globalClickCallback) {
            this._globalClickCallback = function (ev) {
                if (this.dropDownPanel.domElement !== ev.target && !$.contains(this.dropDownPanel.domElement, ev.target)) {
                    this.hideDropDown();
                }
            } .bind(this);

            this._globalKeyPressCallback = function (ev) {
                if (ev.keyCode == '13' || ev.keyCode == '10' || ev.keyCode == '27' || ev.keyCode == '9') {
                    this.hideDropDown();
                    return;
                }

                var pressedKey = String.fromCharCode(ev.charCode || ev.keyCode);

                this._searchString = this._searchString || '';
                var item = this.findDataItemByText(this._searchString + pressedKey);

                if (item) {
                    this._searchString = this._searchString + pressedKey;
                    this.scrollToItem(item);
                    this.setSelectedDataItem(item);
                }

                if (this._searchClearTimer) {
                    window.clearTimeout(this._searchClearTimer);
                }

                this._searchClearTimer = window.setTimeout(function () {
                    this._searchString = '';
                } .bind(this), 500);

            }.bind(this);
        }

        window.setTimeout(function () {
            $(document.body).click(this._globalClickCallback); // !! we are in a click handler, if we wouldn't set timeout then clickCallback will be called at this line
            $(document).keyup(this._globalKeyPressCallback);
        } .bind(this), 200);

        this._isShowed = true;
        this.scrollToItem(this.getSelectedDataItem());
    },

    update: function() {
        if (this._stretchToMaxItem && !this.__free) {
            this.set_width(this._getMaxItemWidth() + 28);
        }

        Phoenix.UI.DropDownList.callBase(this, "update");
    },

    hideDropDown: function () {
        if (this._isShowed) {
            this.removeCssClass('dropDownList_active');
            var style = this.dropDownPanel.domElement.style;
            style.left = "-1000px";
            style.top = "-1000px";
            this.dropDownPanel.hide();
            this._isShowed = false;
            $(document.body).unbind('click', this._globalClickCallback);
            $(document).unbind('keyup', this._globalKeyPressCallback);
            this.raise_onClose();
            
            var oldValue = this.__oldSelectedValue;

            if (this._selectedItem !== oldValue) {
                this.raise_onChanged({ oldValue: oldValue, newValue: this.get_selectedValue() });
            }
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('div', domElement);

        $(this.domElement)
            .hover(
                function () {
                    if (this.get_enabled()) {
                        this.addCssClass('dropDownList_hover');
                    }
                } .bind(this),
                function () { this.removeCssClass('dropDownList_hover'); } .bind(this)
            )
            .click(function (event) {
                if (this._isShowed) {
                    this.hideDropDown();
                } else {
                    this.showDropDown();
                }
            } .bind(this));

        DOM.disableSelection(this.domElement);
        Phoenix.UI.DropDownList.callBase(this, "instantiateInDom", [domElement]);
    },

    clear: function () {
        if (this._items) {
            this._items.remove_changed(this._items_itemChanged, this);
        }

        this._items = null;
        this._getItemsRepeater().clear();
    },

    free: function () {
        this.clearEvents();
        this.clear();
        this.set_selectedValue(null);
        this.get_actionsBar().remove_changed(this.__actionsBarChanged, this);
        Phoenix.UI.DropDownList.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.DropDownList.prototype, [
    { name: 'selectedValue', autoEvent: true },
    { name: 'items', autoEvent: true },
    { name: 'actionsBar', autoEvent: true }
]);

Auto.Events(Phoenix.UI.DropDownList.prototype, [
    'onChanged',
    'onOpen',
    'onCommand',
    'onClose'
]);

Phoenix.UI.DropDownList.createClass('Phoenix.UI.DropDownList', Control);
ControlsFactory.registerControl('dropDownList', Phoenix.UI.DropDownList);
