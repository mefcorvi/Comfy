Type.createNamespace('Phoenix.UI');

Phoenix.UI.Literal = function() {
    Phoenix.UI.Literal.constructBase(this);
};

Phoenix.UI.Literal.prototype = {
    _type: null,
    _text: null,

    defaultOptions: {
        'width': '?',
        height: '?'
    },

    initFromOptions: function(options) {
        Phoenix.UI.Literal.callBase(this, "initFromOptions", [ options ]);
        this._text = options.text || "";
        this._type = isNullOrUndefined(options.dataType) ? 'text' : options.dataType;
    },
    
    set_text: function(value) {           
        if (this.domElement) {
            if (this._type == 'text') {
                this.domElement.data = value;
            } else {
                var domElement = this._getDomElementForHtmlValue(value);
                $(this.domElement).replaceWith(domElement);
                this.domElement = domElement;
            }
        }
        
        this._text = value;
    },
    
    _getDomElementForHtmlValue: function(value) {
        if (!value) {
            value = '<span style="display: none"></span>';
        }

        var $data = $(value);
        var domElement;
            
        if ($data.length > 1) {
            domElement = DOM.create("span");
            domElement.innerHTML = value;
        } else {
            domElement = $data.get(0);
        }

        return domElement;
    },

    instantiateInDom: function(domElement) {
        if (this._type == 'text')
        {
            var textNode = document.createTextNode(this._text || '');
            this.domElement = textNode;
    
            domElement.appendChild(textNode);
        } else if (this._text) {
            this.domElement = this._getDomElementForHtmlValue(this._text);
            domElement.appendChild(this.domElement);
        } else {
            this.domElement = DOM.create("span", domElement);
        }
        
        Phoenix.UI.Literal.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Properties(Phoenix.UI.Literal.prototype, [
    { name: 'text' }
]);

Phoenix.UI.Literal.createClass('Phoenix.UI.Literal', Control);
ControlsFactory.registerControl('literal', Phoenix.UI.Literal);
