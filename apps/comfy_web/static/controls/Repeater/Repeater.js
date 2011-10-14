Type.createNamespace('Phoenix.UI');

Phoenix.UI.Repeater = function() {
    Phoenix.UI.Repeater.constructBase(this);
};

Phoenix.UI.Repeater.prototype = {
    firstItemCssClass: null,
    _emptyDataTemplate: null,

    initFromOptions: function (options) {
        this.set_template(options.template);
        this.firstItemCssClass = options.firstItemCssClass;

        if (options.templateSelector) {
            options.cacheDisabled = true;
        }

        if (options.orientation == 'horizontal') {
            options.width = options.width || '?';
            options.height = options.height || '*';
        } else {
            options.width = options.width || '*';
            options.height = options.height || '?';
        }

        if (options.emptyDataTemplate) {
            this._emptyDataTemplate = new Template(options.emptyDataTemplate);
        } else if (options.emptyDataText) {
            this._emptyDataTemplate = new Template({
                type: 'label',
                width: '?',
                height: '28px',
                text: options.emptyDataText,
                cssClass: 'data_not_found_label',
                padding: '5px'
            });
        }

        Phoenix.UI.Repeater.callBase(this, "initFromOptions", [options]);
        this._checkEmptiness();
    },

    _getTemplate: function(dataItem) {
        if (isFunction(this.options.templateSelector)) { // TODO: Should we rename that property?
            var result = new Template(this.options.templateSelector(dataItem));

            if (result) {
                return result;
            }
        }

        return new Template(this.get_template());
    },

    createItem: function (dataItem) {
        var template = this._getTemplate(dataItem);
        var control = template.instantiate(dataItem, this);
        control.container = this;

        if (this.firstItemCssClass && this.get_childsHash().size() == 0) {
            control.addCssClass(this.firstItemCssClass);
        }

        this.raise_itemCreated({ item: control, data: dataItem });

        return control;
    }
};

Trait.Apply(Phoenix.UI.Repeater.prototype, BaseListDataControl, {
    asOriginal: ['set_dataSource']
});
//Object.extend(Phoenix.UI.Repeater.prototype, BaseListDataControl);

Auto.Events(Phoenix.UI.Repeater.prototype, [
    'itemCreated'
]);

Auto.Properties(Phoenix.UI.Repeater.prototype, [
    { name: 'template' }
]);

Phoenix.UI.Repeater.createClass('Phoenix.UI.Repeater', Phoenix.UI.Panel);
ControlsFactory.registerControl('repeater', Phoenix.UI.Repeater);
