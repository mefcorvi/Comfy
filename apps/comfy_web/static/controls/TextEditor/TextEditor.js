Type.createNamespace('Phoenix.UI');

Phoenix.UI.TextEditor = function() {
    Phoenix.UI.TextEditor.constructBase(this);
};

Object.extend(Phoenix.UI.TextEditor.prototype, {
    defaultOptions: {
        bindings: {
            '*' : 'text'
        }
    },

    initFromOptions: function(options) {
        Phoenix.UI.TextEditor.callBase(this, "initFromOptions", [ options ]);
        
        this._text = options.text || '';
    },

    _getUrl: function(resource) {
        return Application.resolveUrl('~/scripts/ckeditor/' + resource);
    },

    get_text: function() {
        if (this._ckeditor) {
            return this._ckeditor.getData();
        } else {
            return this._text;
        }
    },

    set_text: function(value) {
        if (this._ckeditor) {
            this._ckeditor.setData(value);
        } else {
            this._text = value;
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create('textarea', domElement);
        this.domElement.value = this._text;
        
        Phoenix.UI.TextEditor.callBase(this, "instantiateInDom", [ domElement ]);
        
        this._ckeditor = CKEDITOR.replace(this.domElement, { width: this.get_innerWidth(), height: this.get_innerHeight() - 130 });
    },
    
    updateDom: function() {
        Phoenix.UI.TextEditor.callBase(this, "updateDom");
        try {
            this._ckeditor.resize(this.get_innerWidth(), this.get_innerHeight());
        }
        catch(e) {
            setTimeout(this.updateDom.bind(this), 500);
        }
    },

    free: function() {
        CKEDITOR.remove(this._ckeditor);
        Phoenix.UI.TextEditor.callBase(this, "free");
    }
});

Auto.Properties(Phoenix.UI.TextEditor.prototype, [
    { name: 'text', autoEvent: true }
]);

//Auto.Events(Phoenix.UI.TextEditor.prototype, [ 'keyPressed', 'enterPressed' ]);

Phoenix.UI.TextEditor.createClass('Phoenix.UI.TextEditor', Control);
ControlsFactory.registerControl('textEditor', Phoenix.UI.TextEditor);