Type.createNamespace('Phoenix.UI');

Phoenix.UI.ColorPicker = function() {
    Phoenix.UI.ColorPicker.constructBase(this);
    this._isShowed = false;
};

Phoenix.UI.ColorPicker.prototype = {
    defaultOptions: {
        bindings: {
            '*' : 'color'
        }
    },

    _jscolor: null,
    _isShowed: null,

    set_color: function(value) {
        if (this._color === value) {
            return;
        }

        this._color = value;
        
        if (this._jscolor) {
            var result = this._jscolor.fromString(value);

            if (!result) {
                throw new Error(value + ' is not valid color');
            }
        }
    },

    initFromOptions: function (options) {
        Phoenix.UI.ColorPicker.callBase(this, "initFromOptions", [options]);

        this.addCssClass('color_picker_control');

        if (isFunction(options.onChanged)) {
            this.add_onChanged(options.onChanged, this);
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create("a", domElement);

        this._jscolor = new jscolor.color(this.domElement, {
            valueElement: null,
            styleElement: this.domElement,
            pickerMode: 'HVS',
            pickerZIndex: DepthManager.getNewZIndex()
        });

        this._jscolor.fromString(this.get_color());

        if (!this._globalClickCallback) {
            this._globalClickCallback = function(ev) {
                var element = jscolor.picker.boxB;

                if (element !== ev.target && !$.contains(element, ev.target)) {
                    var color = '#' + this._jscolor.toString();
                    this.set_color(color);
                    this.raise_onChanged({ newValue: color });

                    this._jscolor.hidePicker();
                    $(document.body).unbind('click', this._globalClickCallback);
                }
            }.bind(this);
        }

        $(this.domElement).click(function() {
            window.setTimeout(function() {
                $(document.body).click(this._globalClickCallback);
            }.bind(this), 200);

            this._isShowed = true;
            this._jscolor.pickerZIndex = DepthManager.getNewZIndex();
            this._jscolor.showPicker();
        }.bind(this));

        Phoenix.UI.ColorPicker.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Properties(Phoenix.UI.ColorPicker.prototype, [
    { name: 'color' }
]);

Auto.Events(Phoenix.UI.ColorPicker.prototype, [
    'onChanged'
]);

Phoenix.UI.ColorPicker.createClass('Phoenix.UI.ColorPicker', Control);
ControlsFactory.registerControl('colorPicker', Phoenix.UI.ColorPicker);