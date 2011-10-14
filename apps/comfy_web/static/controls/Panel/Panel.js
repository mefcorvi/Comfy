Type.createNamespace('Phoenix.UI');

Phoenix.UI.Panel = function() {
    Phoenix.UI.Panel.constructBase(this);
};

Phoenix.UI.Panel.__layouts = {};

Phoenix.UI.Panel.GetLayoutEngine = function(layout) {
    var layoutEngine = Phoenix.UI.Panel.__layouts[layout];
    
    if (!layoutEngine) {
        throw new Error('[Panel]: Layout engine "' + layout + '" have not been found');
    }
    
    return new layoutEngine();
}

Phoenix.UI.Panel.RegisterLayoutEngine = function(layout, layoutEngine) {
    if (Phoenix.UI.Panel.__layouts[layout]) {
        throw new Error('[Panel]: Layout engine "' + layout + '" already registered');        
    }
    
    Phoenix.UI.Panel.__layouts[layout] = layoutEngine;
}

Phoenix.UI.Panel.prototype = {
    defaultOptions: {
        width: '*',
        height: '*'
    },
    
    /**
     * Instantiate the control into some domElement
     * @param {DOMELement} domElement
     */
    instantiateInDom: function(domElement) {
        var element = DOM.create("div", domElement);
        
        if (this._zIndex) {
            element.style.zIndex = this._zIndex;
        }

        this.domElement = element;
        
        /*if (this._layoutEngine && this._layoutEngine.initInDom) {
            this._layoutEngine.initInDom();
        }*/

        Phoenix.UI.Panel.callBase(this, "instantiateInDom", [ domElement ]);
    },
    
    instantiateChildInDom: function(control, type, refControl) {
        /*if (!this._layoutEngine || !control.isInDocumentFlow()) {*/
            Phoenix.UI.Panel.callBase(this, "instantiateChildInDom", [ control, type, refControl ]);            
        /*} else {
            this._layoutEngine.instantiateChildInDom(control, type, refControl);
        }*/
    },
      
    /**
    * Initializes size and position of blocks in DOM
    */
    _updateChildsDom: function() {
        this._clientWidth = null;
        this._clientHeight = null;
        
        if (this._layoutEngine) {
            var controlsLength = this.controls.length;
            
            for (var i = 0; i < controlsLength; i++) {
                var control = this.controls[i];
                
                if (control.isInDocumentFlow()) {
                    this._layoutEngine.updateChildDom(control);
                }
            }
        }
    },

    removeChildFromDom: function(control) {
        Phoenix.UI.Panel.callBase(this, "removeChildFromDom", [ control ]);

        /*if (this._layoutEngine) {
            this._layoutEngine.removeChildFromDom(control);
        }*/
    },
    
    free: function() {
        Phoenix.UI.Panel.callBase(this, "free");
    }
};

Phoenix.UI.Panel.createClass('Phoenix.UI.Panel', Control);
ControlsFactory.registerControl('panel', Phoenix.UI.Panel);