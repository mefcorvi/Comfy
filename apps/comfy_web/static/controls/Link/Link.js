Type.createNamespace('Phoenix.UI');

Phoenix.UI.Link = function() {
    Phoenix.UI.Link.constructBase(this);
};

Phoenix.UI.Link.prototype = {
    _url: null,
    _text: null,
    
    defaultOptions: {
        disabledCssClass: 'link_disabled',
        url: 'javascript:void(0);',
        text: '',
        width: '?',
        height: '?',  
        bindings: {
            '*' : 'text'
        }
    },
    
    initFromOptions: function(options) {
        Phoenix.UI.Link.callBase(this, "initFromOptions", [ options ]);
        
        this._url = options.url || "";
        this._text = options.text || "";
        
        if (typeof(options.onClick) == 'function') {
            this.add_onClick(options.onClick, this);
        }
        
        if (typeof(options.onDblClick) == 'function') {
            this.add_onDblClick(options.onDblClick, this);
        }
    },
    
    set_text: function(value) {
        if (this.domElement) {
            $(this.domElement).text(value);
            this.update();
        }
        
        this._text = value;
    },
    
    set_url: function(value) {           
        if (this.domElement) {
            this.domElement.href = Application.resolveUrl(value);
        }
        
        this._url = value;
    },
    
    /**
     * Instantiate this control into DOM
     */
    instantiateInDom: function(domElement) {
        var element = DOM.create("a", domElement);
        var $element = $(element);
        
        this.domElement = element;
        
        $element
            .attr('href', Application.resolveUrl(this._url))
            .bind('click', function() {
                if (this._enabled) {
                    this.raise_onClick()
                }
            }.bind(this))
            .bind('dblclick', function() {
                if (this._enabled) {
                    this.raise_onDblClick()
                }
            }.bind(this));
            
        if (this.controls.getControlsInFlow().length == 0) {
            $element.text(this._text || '');
        }
                
        Phoenix.UI.Link.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Properties(Phoenix.UI.Link.prototype, [
    'text',
    'url'
]);

Auto.Events(Phoenix.UI.Link.prototype, [
    'onClick',
    'onDblClick'
]);

Phoenix.UI.Link.createClass('Phoenix.UI.Link', Control);
ControlsFactory.registerControl('link', Phoenix.UI.Link);
