Type.createNamespace('Phoenix.UI');

Phoenix.UI.TextBox = function() {
    Phoenix.UI.TextBox.constructBase(this);
};

Object.extend(Phoenix.UI.TextBox.prototype, {
    _activeClassName: null,
    _watermarkText: null,
    _watermarkShowed: null,
    _mode: null,
    _format: null,

    defaultOptions: {
        bindings: {
            '*' : 'text'
        },
        border: '1',
        width: '*',
        height: '24px',
        padding: '2',
        disabledCssClass: 'textBox_disabled'
    },

    initFromOptions: function(options) {
        Phoenix.UI.TextBox.callBase(this, "initFromOptions", [ options ]);

        this._text = isDefined(this._text) ? this._text : (options.text || '');
        this._activeClassName = options.activeCssClass;
        this._watermarkText = options.watermark;
        this._mode = options.mode || 'single';
        this._format = options.format || 'text';

        if(options.validation)
            this.initValidation(options.validation);

        if (options.onChanged && isFunction(options.onChanged)) {
            this.add_textChanged(options.onChanged, this);
        }

        if (options.onKeyDown && isFunction(options.onKeyDown)) {
            this.add_keyDown(options.onKeyDown, this);
        }

        if (options.onKeyPress && isFunction(options.onKeyPress)) {
            this.add_keyPressed(options.onKeyPress, this);
        }

        if (options.onEnterPressed && isFunction(options.onEnterPressed)) {
            this.add_enterPressed(options.onEnterPressed, this);
        }

        if (this._format == 'double' || this._format == 'float') {
            this.initValidation({
                expression: /^[0-9]+?(\.[0-9]+?)?$/,
                errorText: 'Please enter a numeric value',
                notEmpty: true
            });
        }

        if (this._format == 'int') {
            this.initValidation({
                expression: /^[0-9]+?$/,
                errorText: 'Please enter an integer value',
                notEmpty: true
            });
        }
    },

    _onChange: function(args) {
        this.set_text(this.domElement.value, false);
    },

    get_dataSource: function() {
        return this.get_text();
    },

    get_text: function() {
        return this._watermarkShowed ? "" : this._text;
    },

    set_text: function(value, updateDom) {
        var updateDom = isNullOrUndefined(updateDom) ? true : updateDom;

        if (isNullOrUndefined(value))
            value = '';

        if (this.domElement) {
            if (this.domElement.value !== value && updateDom) {
                this.domElement.value = value;
                this._checkWatermark();
            }

            if (this.validate) {
                if (this.__validationTimeout) {
                    clearTimeout(this.__validationTimeout);
                }

                this.__validationTimeout = setTimeout(function() {
                    this.validate();
                }.bind(this), 300);
            }
        }

        if (this._text !== value) {
            var oldValue = this._text;
            this._text = value;

            this.raise_textChanged({oldValue: oldValue, newValue: value});
        }
    },

    set_enabled: function(value) {
        if (this._enabled === value)
            return false;

        this._enabled = value;

        if (this.domElement) {
            this.domElement.disabled = !value;
        };

        if (this._disabledCssClass) {
            if (!value) {
                this.addCssClass(this._disabledCssClass);
            } else {
                this.removeCssClass(this._disabledCssClass);
            }
        }
    },

    _onFocus: function() {
        var element = this.domElement;

        if (this._activeClassName) {
            $(element).addClass(this._activeClassName);
        }

        if (this._watermarkText && element.value == this._watermarkText && this._watermarkShowed) {
            element.value = "";
            this.removeCssClass('watermark');
            this._watermarkShowed = false;
        }
    },

    _onBlur: function() {
        var element = this.domElement;

        if(!element)
            return;

        if (this._activeClassName) {
            $(element).removeClass(this._activeClassName);
        }

        if (!element.value && this._watermarkText) {
            element.value = this._watermarkText;
            this.addCssClass('watermark');
            this._watermarkShowed = true;
        }
    },

    _checkWatermark: function() {
        if (this._watermarkText && this.domElement) {
            var element = this.domElement;

            if (!element.value) {
                element.value = this._watermarkText;
                this.addCssClass('watermark');
                this._watermarkShowed = true;
            } else if (this._watermarkShowed && element.value != this._watermarkText) {
                this.removeCssClass('watermark');
                this._watermarkShowed = false;
            }
        }
    },

    _onKeyDown: function(ev) {
        if (ev.keyCode == 13 || ev.keyCode == 10) {
            this.raise_enterPressed(ev);
        }

	this.raise_keyDown(ev);
    },

    getCaret: function() {
	var el = this.domElement;

	if (el.selectionStart) {
	    return el.selectionStart;
	} else if (document.selection) {
	    var r = document.selection.createRange();
	    if (r == null) {
		return 0;
	    }

	    var re = el.createTextRange(),
	    rc = re.duplicate();
	    re.moveToBookmark(r.getBookmark());
	    rc.setEndPoint('EndToStart', re);

	    return rc.text.length;
	};

	return 0;
    },

    _onKeyPressed: function(ev) {
        this.raise_keyPressed(ev);
    },

    selectAll: function() {
        if (this.domElement) {
            this.domElement.select();
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create(this._mode == 'multiline' ? 'textarea' : 'input', domElement);
        this.domElement.value = this._text;

        var $element = $(this.domElement);

        $element
            .bind('keyup', this._onChange.bind(this))
            .bind('mousedown', function() {
                this.__mouseDown = true;
            }.bind(this))
            .bind('mouseup', function() {
                this.__mouseDown = false;
            }.bind(this))
            .bind('mouseout', function(e) {
                if (this.__mouseDown) {
                    this.selectAll();
                    this.__mouseDown = false;
                    e.stopPropagation();
                    e.preventDefault();
                }
            }.bind(this))
            .bind('keydown', this._onKeyDown.bind(this))
            .bind('keyup', this._onKeyPressed.bind(this));

        if (this._activeClassName || this._watermarkText) {
            $element
                .bind('focus', this._onFocus.bind(this))
                .bind('blur', this._onBlur.bind(this));
        };

        if (this._watermarkText && !this._text) {
            this.domElement.value = this._watermarkText;
            this._watermarkShowed = true;
            this.addCssClass('watermark');
        }

        Phoenix.UI.TextBox.callBase(this, "instantiateInDom", [ domElement ]);
    }
});

Auto.Properties(Phoenix.UI.TextBox.prototype, [
    { name: 'text', autoEvent: true }
]);

Auto.Events(Phoenix.UI.TextBox.prototype, [ 'keyPressed', 'enterPressed', 'keyDown' ]);

Phoenix.UI.TextBox.createClass('Phoenix.UI.TextBox', Control);
ControlsFactory.registerControl('textBox', Phoenix.UI.TextBox);
ControlsFactory.registerControl('textbox', Phoenix.UI.TextBox);