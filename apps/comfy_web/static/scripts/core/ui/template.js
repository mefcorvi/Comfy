Template = function(options) {
    if(!options)
        throw new Error('Template options must be specified');
        
    this.options = options;
};

Template.prototype = {
    _domInstantiation: null,

    instantiate: function(dataItem, parent) {
        var controlType = this.options.type || 'container';
        var container = ControlsFactory.create(controlType);
        
        container.parent = parent;
        var options = Object.copy(this.options, true);
        container.initFromOptions(options);

        if (dataItem) {
            if (!parent) {
                console.warn('Ensure you passed the parent when you was passing dataItem to template');
            }
            container.set_dataSource(dataItem);
        }

        return container;
    }
};

Template.createClass('Template');