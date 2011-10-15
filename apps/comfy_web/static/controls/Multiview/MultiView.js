Type.createNamespace('Phoenix.UI');

Phoenix.UI.MultiView = function() {
    Phoenix.UI.MultiView.constructBase(this);
};

Phoenix.UI.MultiView.prototype = {
    initFromOptions: function(options) {
        if (!options.views) {
            throw new Error('[Multiview] Cannot find "views" property in options');
        }
        
        options.controls = [];
        options.layout = "stack";
        options.orientation = "vertical";
        
        if (isFunction(options.onActiveViewChanged)) {
            this.add_onActiveViewChanged(options.onActiveViewChanged, this);
        }
        
        var activeView = options.activeView || '';
        
        for (var i = 0; i < options.views.length; i++) {
            var view = options.views[i];
            view.id = view.id || "view" + i;
            view.visible = true;
            
            if (!activeView && i == 0) {
                activeView = view.id;
            }
            
            view.width = options.width == '?' ? '?' : '*';
            view.height = options.height == '?' ? '?' : '*';
        }
        
        Phoenix.UI.MultiView.callBase(this, "initFromOptions", [ options ]);
        
        this.set_activeView(activeView);
    },
    
    _showView: function(id) {
        var viewOptions = null;
        
        for (var i = 0; i < this.options.views.length; i++) {
            var view = this.options.views[i];
            
            if (view.id == id) {
                viewOptions = Object.copy(view, true);
            }
        }
        
        if (!view) {
            throw new Error('Cannot find the view with id = "' + id + '"');
        }
        
        var controlType = viewOptions.type;
        var control = ControlsFactory.create(controlType);

        control.initFromOptions(viewOptions);
        
        this.controls.add(control);
    },
    
    _hideView: function(id) {
        var view = this[id];
        view.free();
        
        this.controls.remove(view);
    },
    
    set_activeView: function(value) {
        if (this._activeView === value) {
            return;
        }
        
        var oldView = this._activeView;
        this._activeView = value;
        
        if (oldView && oldView != value) {
            this._hideView(oldView);
        }
        
        this._showView(value);
        
        this.raise_onActiveViewChanged({ oldValue: oldView, newValue: value, view: this[value] });
    }
};

Auto.Properties(Phoenix.UI.MultiView.prototype, [
    { name: 'activeView' }
]);

Auto.Events(Phoenix.UI.MultiView.prototype, [
    'onActiveViewChanged'
]);

Phoenix.UI.MultiView.createClass('Phoenix.UI.MultiView', Phoenix.UI.Panel);
ControlsFactory.registerControl('multiView', Phoenix.UI.MultiView);
