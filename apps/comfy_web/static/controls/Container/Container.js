Type.createNamespace('Phoenix.UI');

Phoenix.UI.Container = function() {
    Phoenix.UI.Container.constructBase(this);
};

Phoenix.UI.Container.prototype = {
    _childContainer: null,
    
    initFromOptions: function(options) {        
        this.set_tag(options.tag);
    
        Phoenix.UI.Container.callBase(this, "initFromOptions", [ options ]);
    },
    
    get_childsContainer: function() {
        return this._childContainer;
    },
    
	instantiateInDom: function(domElement) {
	    if(this._tag) {
	        this.domElement = DOM.create(this._tag, domElement);
	    }	        
	    
	    this._childContainer = this.domElement || domElement;

        Phoenix.UI.Container.callBase(this, "instantiateInDom", [ domElement ]);
	}
};

Auto.Properties(Phoenix.UI.Container.prototype, [
    'tag'
]);

Phoenix.UI.Container.createClass('Phoenix.UI.Container', Control);
ControlsFactory.registerControl('container', Phoenix.UI.Container);
