Type.createNamespace('Phoenix.UI');

Phoenix.UI.Label = function() {
    Phoenix.UI.Label.constructBase(this);
};

Phoenix.UI.Label.prototype = {
    defaultOptions: {
        width: '?',
        height: '?',
        bindings: {
            '*': 'text'
        },
        multiline: false
    },

    initFromOptions: function (options) {
        this._emptyText = options.emptyText || "";
        this._format = options.format;
        this._hideIfEmpty = !!options.hideIfEmpty;
        this._multiline = options.multiline;

        if (isDefined(options.text) && isUndefined(this._text)) {
            this.set_text(options.text);
        }

        Phoenix.UI.Label.callBase(this, "initFromOptions", [options]);
        this._checkIsEmpty();
    },

    _updateText: function() {
        this._checkIsEmpty();
        var text = this.get_text();

        if (this._format) {
            text = this._format.format(text);
        }

        if (this._multiline) {
            text = this._prepareMultiline(text);

            if (this.domElement) {
                this.domElement.innerHTML = text;
                this.update();
            }
        } else {
            if (this.domElement) {
                this.domElement.firstChild.data = text;
                this.update();
            }
        }
    },

    _checkIsEmpty: function() {
        if (this._hideIfEmpty) {
            if (!this._text) {
                this.hide();
            } else {
                this.show();
            }
        }
    },

    set_text: function (value) {
        this._text = value;
        this._updateText();
    },

    get_text: function () {
        return isNullOrUndefined(this._text) || this._text === '' ? this._emptyText : this._text;
    },

    _prepareMultiline: function (text) {
        text = text.replaceAll("\n", "<br />");
        text = text.replaceAll("\u000d\u000a", "<br />");

        return text;
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div");
        var text = this._format ? this._format.format(this.get_text()) : this.get_text();

        if (this._multiline) {
            text = this._prepareMultiline(text);
            this.domElement.innerHTML = text;
        } else {
            var textNode = document.createTextNode(text);
            this.domElement.appendChild(textNode);
        }

        domElement.appendChild(this.domElement);
        Phoenix.UI.Label.callBase(this, "instantiateInDom", [domElement]);
    },

    updateDom: function(sender) {
        if (this.options.width || this.options.height) {
            if (sender || !this.parent) {
                var element = this.domElement, width = this.get_clientWidth(), height = this.get_clientHeight();
                DOM.setBoundingRect(element, width, height);
            }
        }
            
        Phoenix.UI.Label.callBase(this, "updateDom", [ sender ]);
    }
};

Phoenix.UI.Label.createClass('Phoenix.UI.Label', Control);
ControlsFactory.registerControl('label', Phoenix.UI.Label);

Auto.Properties(Phoenix.UI.Label.prototype, [
    { name: 'text' },
    { name: 'format'}
]);