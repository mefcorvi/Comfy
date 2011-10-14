Type.createNamespace('Phoenix.UI');

Phoenix.UI.Button = function() {
    Phoenix.UI.Button.constructBase(this);
};

Phoenix.UI.Button.prototype = {
    defaultOptions: {
        disabledCssClass: 'link_button_disabled',
        cssClass: 'link_button',
        padding: '20 0 0 13',
        margin: '1 0',
        width: '?',
        height: '33',
        maxWidth: 200,
        maxHeight: 33,
        text: 'Button',
        bindings: {
            '*': 'text'
        }
    },

    initFromOptions: function(options) {
        Phoenix.UI.Button.callBase(this, "initFromOptions", [ options ]);
        
        this._text = options.text;
        
        if (typeof(options.onClick) == 'function') {
            this.add_onClick(options.onClick, this);
        }
    },
    
    set_text: function(value) {
        if (this._text === value) {
            return;
        }
        
        this._text = value;
        
        if (this.domElement) {
            $(this.domElement.firstChild).text(this._text);
            this.update();
        }
    },

    update: function() {
        if (this.domElement) {
            DOM.setBoundingRect(this.domElement.firstChild, null, null);
        }

        Phoenix.UI.Button.callBase(this, "update");
    },

    updateDom: function() {
        Phoenix.UI.Button.callBase(this, "updateDom");
        
        if (this.domElement) {
            DOM.setBoundingRect(this.domElement.firstChild, Math.max(this.get_innerWidth() - this.get_padding().get_width(), 0) || null, this.get_innerHeight() || null);
        }
    },
      
    /**
     * Instantiate this control into DOM
     */
    instantiateInDom: function(domElement) {
        var element = DOM.create('a', domElement);
        element.href = "javascript:void(0);";
        element.innerHTML = ['<span>', this._text, '</span>'].join('');
        
        this.domElement = element;
        
        $(element).bind('click',
            function() {
                if (this._enabled) {
                    this.raise_onClick()
                }
            }.bind(this)
        );
        
        DOM.disableSelection(element);
        Phoenix.UI.Button.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Events(Phoenix.UI.Button.prototype, [
    'onClick'
]);

Auto.Properties(Phoenix.UI.Button.prototype, [
    { name: 'text' }
]);

Phoenix.UI.Button.createClass('Phoenix.UI.Button', Control);
ControlsFactory.registerControl('button', Phoenix.UI.Button);