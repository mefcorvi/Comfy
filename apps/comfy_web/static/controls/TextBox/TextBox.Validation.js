Phoenix.UI.TextBox.Validation = {
    validate: function() {
        this._isValid = true;        
        
        var options = this._validationOptions;
        
        if(!options)
            return;
        
        var text = this.get_text().toString();
        
        if(options.notEmpty && String.isNullOrEmpty(text))
            this._isValid = false;
        
        if (options.expression && text && !text.match(options.expression)) {
            this._isValid = false;
        }
        
        if (!this._isValid) {
            this._validationIsFailed();
            return false;
        }
        else {
            this._validationIsSuccess();
            return true;
        }
    },

    _validationIsSuccess: function() {
        this.removeCssClass('notValid');
        this._hideValidationTooltip();
    },

    _validationIsFailed: function() {
        this.addCssClass('notValid');
        this._showValidationTooltip();
    },

    _showValidationTooltip: function() {
        var domElement = this.domElement;

        if (this._validationTooltip) {
            return;
        }

        if (!domElement) {
            return;
        }

        var validationText = this.get_validationOptions().errorText || 'Wrong format';
        var offset = $(domElement).offset();

        var tooltip = DOM.create('div', document.body, { className: 'validation_tooltip' });
        this._validationTooltip = tooltip;
        $(tooltip).text(validationText);
        var tooltipStyle = tooltip.style;
        tooltipStyle.position = 'absolute';
        tooltipStyle.left = (offset.left) + 'px';
        tooltipStyle.top = (offset.top - 21) + 'px';
        tooltip.__height = $(tooltip).height();
        tooltip.__top = offset.top - 21;

        tooltipStyle.width = (this.get_clientWidth() - 6) + 'px';
        tooltipStyle.zIndex = DepthManager.getNewZIndex();
        
        $(tooltip)
            .animate({
                height: 0,
                top: offset.top,
                opacity: 0
            }, 0)
            .animate({
                height: tooltip.__height,
                top: tooltip.__top,
                opacity: 1
            }, 150);

        setTimeout(this._hideValidationTooltip.bind(this), 5000);
        this.add_onFree(this._hideValidationTooltip, this);
    },
    
    _hideValidationTooltip: function() {
        if (this._validationTooltip) {
            this.remove_onFree(this._hideValidationTooltip, this);
            var tooltip = this._validationTooltip;
            $(tooltip)
                .animate({
                    height: 0,
                    top: tooltip.__top + 27,
                    opacity: 0
                }, 150, null, function() {
                    DOM.remove(tooltip);
                });
            this._validationTooltip = null;
        }
    }
};

Auto.Properties(Phoenix.UI.TextBox.Validation, [
    'validationOptions',
    'isValid'
]);

Phoenix.UI.TextBox.prototype.initValidation = function(validationOptions) {
    if(!this.__validationInitialized) {
        Trait.Apply(this, Phoenix.UI.TextBox.Validation);
        this.__validationInitialized = true;
    }
    
    this._isValid = null;
    
    this.set_validationOptions(validationOptions);    
};