Type.createNamespace('Phoenix.UI');

Phoenix.UI.Frame = function() {
    Phoenix.UI.Frame.constructBase(this);
};

Phoenix.UI.Frame.prototype = {
    defaultOptions: {
        bindings: {
            '*': '*'
        }
    },

    _uri: null,
    _page: null,

    _createPage: function() {
        this._disposePage();
            
        if (this._uri) {
            var navObj = Application.getPageNavigationObject(this._uri);
            var page = new Page(navObj.params);
		    this._page = page;
            
            page.add_initComplete(function() {
                this.controls.add(page);
            }, this);
		    
		    page.initFromUri(navObj.uri);
        }
    },

    _disposePage: function() {
        if (this._page) {
            this.controls.remove(this._page);
            this._page.free();
        }
    },

    initFromOptions: function (options) {
        this.addCssClass('frame_control');
        this._uri = options.uri;

        Phoenix.UI.Frame.callBase(this, "initFromOptions", [options]);
    },

    set_uri: function (value) {
        if (this._uri === value) {
            return;
        }

        this._uri = value;
        this._disposePage();

        if (value) {
            this._createPage();
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div", domElement);
        this._createPage();
        Phoenix.UI.Frame.callBase(this, "instantiateInDom", [domElement]);
    },

    free: function() {
        _page = null;
        Phoenix.UI.Frame.callBase(this, "free");
    }
};

Phoenix.UI.Frame.createClass('Phoenix.UI.Frame', Control);
ControlsFactory.registerControl('frame', Phoenix.UI.Frame);

Auto.Properties(Phoenix.UI.Frame.prototype, [
    { name: 'uri' }
]);