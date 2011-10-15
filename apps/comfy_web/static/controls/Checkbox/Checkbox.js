Type.createNamespace('Phoenix.UI');

var CheckBoxStates = {
    empty: 0,
    checked: 1,
    gray: 2
};

Phoenix.UI.Checkbox = function() {
    Phoenix.UI.Checkbox.constructBase(this);
    this._state = CheckBoxStates.empty;
};

Phoenix.UI.Checkbox.prototype = {
    _text: null,
    _selectedCssClass: null,
    _isTriState: null,

    defaultOptions: {
        disabledCssClass: 'checkbox_disabled',
        width: '?',
        height: '16px',
        bindings: {
            '*': 'state'
        }
    },

    initFromOptions: function (options) {
        this.addCssClass('checkbox');
        this._selectedCssClass = 'checkbox_selected';
        this._grayCssClass = 'checkbox_gray';
        this._text = options.text;
        this._state = options.state || CheckBoxStates.empty;
        this._isTriState = !!options.isTriState;

        if (isFunction(options.onChanged)) {
            this.add_onChanged(options.onChanged, this);
        }

        Phoenix.UI.Checkbox.callBase(this, "initFromOptions", [options]);
    },

    set_text: function (value) {
        if (this._text === value) {
            return;
        }

        this._text = value;
        this._initTextNode();

        if (this._textNode) {
            this._textNode.data = value;
        }
    },

    set_state: function (value) {
        var oldValue = this._state;

        if (value === true) {
            value = CheckBoxStates.checked;
        }

        if (value === false || isNullOrUndefined(value)) {
            value = CheckBoxStates.empty;
        }

        if (oldValue === value) {
            return;
        }

        value = value*1;

        this._state = value;
        this._applyState();
        this.raise_stateChanged({ newValue: value, oldValue: oldValue });
    },

    _applyState: function () {
        if (this.domElement) {
            if (this._state == CheckBoxStates.checked) {
                this.addCssClass(this._selectedCssClass);
            } else {
                this.removeCssClass(this._selectedCssClass);
            }
            
            if (this._state == CheckBoxStates.gray) {
                this.addCssClass(this._grayCssClass);
            } else {
                this.removeCssClass(this._grayCssClass);
            }
        }
    },

    _initTextNode: function () {
        if (this.domElement && this._text && !this._textNode) {
            this._textNode = DOM.createTextNode(this._text, this.domElement);
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('div', domElement);
        this.checkNode = DOM.create('span', this.domElement);
        this._initTextNode();

        $(this.domElement).click(function (ev) {
            if (this._enabled) {
                this.setNextState();
            }
        }.bind(this));

        this._applyState();

        DOM.disableSelection(this.domElement);

        Phoenix.UI.Checkbox.callBase(this, "instantiateInDom", [domElement]);
    },

    setNextState: function () {
        switch (this._state) {
            case CheckBoxStates.empty:
                this.set_state(this._isTriState ? CheckBoxStates.gray : CheckBoxStates.checked);
                break;
            case CheckBoxStates.gray:
                this.set_state(CheckBoxStates.checked);
                break;
            case CheckBoxStates.checked:
                this.set_state(CheckBoxStates.empty);
                break;
        }
        this.raise_onChanged();
    },

    free: function() {
        DOM.remove(this.checkNode);
        delete this.checkNode;
        Phoenix.UI.Checkbox.callBase(this, "free");
    }
};

Auto.Events(Phoenix.UI.Checkbox.prototype, [
    'onChanged'
]);

Auto.Properties(Phoenix.UI.Checkbox.prototype, [
    { name: 'text' },
    { name: 'state', autoEvent: true }
]);

Phoenix.UI.Checkbox.createClass('Phoenix.UI.Checkbox', Control);
ControlsFactory.registerControl('checkbox', Phoenix.UI.Checkbox);
