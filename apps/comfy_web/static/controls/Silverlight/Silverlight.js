Type.createNamespace('Phoenix.UI');

Phoenix.UI.Silverlight = function() {
    Phoenix.UI.Silverlight.constructBase(this);
};

Phoenix.UI.Silverlight.prototype = {
    initFromOptions: function(options) {
        if (isFunction(options.onSlLoad)) {
            this.add_onSlLoad(options.onSlLoad, this);
        }
    
        if (isFunction(options.onSlPreLoad)) {
            this.add_onSlPreLoad(options.onSlPreLoad, this);
        }
        
        if (isFunction(options.onSlError)) {
            this.add_onSlError(options.onSlError, this);
        }
        
        this._initialParams = options.params || "";
        
        this._source = Application.resolveUrl(options.url || '');
        Phoenix.UI.Silverlight.callBase(this, "initFromOptions", [ options ]);
    },
    
    _onSlLoad: function() {
        if (!this.silverlightObject || isNullOrUndefined(this.silverlightObject.Content)) {
            setTimeout(this._onSlLoad.bind(this), 1000);
            return;
        }

        this.loaded = true;
        
        this.raise_onSlLoad();
    },
    
    get_content: function() {
        return this.loaded ? this.silverlightObject.Content : null;
    },

    reload: function () {            
        if (this._instantiatedInDom && this.domElement) {
            var sibling = this.domElement.nextSibling;
            var parent = this.domElement.parentNode;
            DOM.remove(this.domElement);
            this.instantiateInDom(parent);
            parent.insertBefore(this.domElement, sibling);
            this.updateDom();
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div", domElement);
        
        var initParams = { params: this._initialParams };
        this.raise_onSlPreLoad(initParams);
        this._initialParams = initParams.params;
        
        var windowlessValue = this.options.isWindowless ? this.options.isWindowless : 'true';
        //var windowlessValue = 'true';

        Silverlight.createObject(this._source, this.domElement, null, {
                version: "4.0.50826.0",
                isWindowless: windowlessValue,
                minRuntimeVersion: '4.0.50826.0',
                autoUpgrade: 'true'
            },
            {
                onError: this.raise_onSlError,
                onLoad: this._onSlLoad.bind(this)
            }, this._initialParams
        );
        
        this.silverlightObject = this.domElement.firstChild;
        this.silverlightObject.style.width = '100%';
        this.silverlightObject.style.height = '100%';
        Phoenix.UI.Silverlight.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Events(Phoenix.UI.Silverlight.prototype, [
    'onSlLoad',
    'onSlError',
    'onSlPreLoad'
]);


Phoenix.UI.Silverlight.createClass('Phoenix.UI.Silverlight', Control);
ControlsFactory.registerControl('silverlight', Phoenix.UI.Silverlight);
