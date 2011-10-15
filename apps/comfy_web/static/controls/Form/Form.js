Type.createNamespace('Phoenix.UI');

Phoenix.UI.Form = function() {
    Phoenix.UI.Form.constructBase(this);
};

Phoenix.UI.Form.prototype = {
    initFromOptions: function (options) {
        this.addCssClass('form_control');
        this._labelWidth = options.labelWidth || '30%';
        options.vspacing = isDefined(options.spacing) ? options.spacing : 2;
        options.hspacing = 5;

        options.layout = 'grid';
        options.columns = [ this._labelWidth, '*' ];
        options.rows = [];

        var controls = [];

        for (var i = 0; i < options.controls.length; i++) {
            var control = options.controls[i];
            control['grid.column'] = 2;
            control['grid.row'] = i + 1;
            control.width = control.width || '*';

            options.rows.add(control['height'] || '?');

            if (control['form.label']) {
                var label = this._createLabel(control['form.label'], control['id']);
                label['grid.column'] = 1;
                label['grid.row'] = i + 1;
                controls.add(label);
            }
        }

        options.controls.add(controls);

        Phoenix.UI.Form.callBase(this, "initFromOptions", [options]);
    },

    hideRow: function(relatedId) {
        var label = this['label_' + relatedId];
        this[relatedId].set_height(0);
        label.set_height(0);
        var rowId = label.options['grid.row'];
        this.get_layoutEngine().get_rows()[rowId - 1] = 0;
        this.get_layoutEngine().updateRowsAndColumns();
        this.update();
    },

    focus: function() {
        var firstControl = this.findControl(function(control) {
            return control.isFocusable();
        });
        
        if (firstControl) {
            firstControl.focus();
        }
    },

    _createLabel: function(text, relatedId) {
        var thisObj = this;

        return {
            id: relatedId ? 'label_' + relatedId : undefined,
            type: 'label',
            width: '*',
            valign: 'middle',
            height: '18',
            cssClass: 'form_label',
            text: text
        };
    }
};

Phoenix.UI.Form.createClass('Phoenix.UI.Form', Phoenix.UI.Panel);
ControlsFactory.registerControl('form', Phoenix.UI.Form);