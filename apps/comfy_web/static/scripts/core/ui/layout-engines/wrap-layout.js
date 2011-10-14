Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.WrapLayout = function (control) {
    Nimble.Core.LayoutEngines.WrapLayout.constructBase(this, [ control ]);

    if (this.isDependsOnChildWidth() && this._orientation == 'horizontal') {
        throw new Error('Horizontal wrap layout should have determined width.');
    }
    
    if (this.isDependsOnChildHeight() && this._orientation == 'vertical') {
        throw new Error('Vertical wrap layout should have determined height.');
    }
};

Nimble.Core.LayoutEngines.WrapLayout.prototype = {
    _groupDimensions: null,
    _groupSumMeasure: null,

    _getChildsWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;

        if (controls.length == 0) {
            result = this._getDomWidth();
        } else {
            result = this._groupSumMeasure;
        }
        
        if (result !== null) {
            var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();
            result += offset;
        }

        return result;
    },

    _getChildsHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;
        
        if (controls.length == 0) {
            result = this._getDomHeight();
        } else {
            result = this._groupSumMeasure;
        }

        if (result !== null) {
            var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();
            result += offset;
        }

        return result;
    },

    _recalculateGroupIndex: function() {
        this._groupDimensions = [];
        this._groupSumMeasure = 0;

        var controls = this._control.controls.getControlsInFlow(),
            limiterMeasureFuncName = this._isHorizontal() ? 'get_innerWidth' : 'get_innerHeight',
            measureFuncName = this._isHorizontal() ? 'get_outerWidth' : 'get_outerHeight',
            invMeasureFuncName = !this._isHorizontal() ? 'get_outerWidth' : 'get_outerHeight';

        var groupLimiterMeasure = this._control[limiterMeasureFuncName](),
            currentIndex = 0,
            usedMeasure = 0;

        if (groupLimiterMeasure < 1) {
            for (var i = 0; i < controls.length; i++) {
                delete controls[i].$__wrapLayoutGroupIndex;
            }

            return;
        }

        for (var i = 0; i < controls.length; i++) {
            var controlMeasure = controls[i][measureFuncName](),
                invMeasure = controls[i][invMeasureFuncName](); // invMeasure used to determine width of column or height of row
            
            usedMeasure += controlMeasure;

            if (usedMeasure > groupLimiterMeasure) {
                currentIndex++;
                usedMeasure = controlMeasure;
            }

            if (invMeasure > this._groupDimensions[currentIndex] || !this._groupDimensions[currentIndex]) {
                this._groupDimensions[currentIndex] = invMeasure;
            }

            controls[i].$__wrapLayoutGroupIndex = currentIndex;
        }

        for (var i = 0; i < this._groupDimensions.length; i++) {
            this._groupSumMeasure += this._groupDimensions[i];
        }
    },

    get_childPosition: function(childControl) {
        var controls = this._control.controls.getControlsInFlow();
        var left = 0;
        var top = 0;

        for (var i = 0; i < controls.length; i++) {
            if (controls[i] === childControl) {
                break;
            }

            if (controls[i].$__wrapLayoutGroupIndex === childControl.$__wrapLayoutGroupIndex) {
                if (this._isHorizontal()) {
                    left += controls[i].get_outerWidth();
                } else {
                    top += controls[i].get_outerHeight();
                }
            }
        }

        for (var i = 0; i < childControl.$__wrapLayoutGroupIndex; i++) {
            if (!this._isHorizontal()) {
                left += this._groupDimensions[i];
            } else {
                top += this._groupDimensions[i];
            }
        }

        var valign = childControl.options.valign;
        var halign = childControl.options.halign;
        var offsetLeft = 0;
        var offsetTop = 0;

        if (!this._isHorizontal()) {
            if (halign && halign != 'left') {
                offsetLeft = (this._groupDimensions[childControl.$__wrapLayoutGroupIndex] - childControl.get_clientWidth()) * (halign == 'center' ? 0.5 : 1);
                left += offsetLeft;
            }
        } else {
            if (valign && valign != 'top') {
                offsetTop = (this._groupDimensions[childControl.$__wrapLayoutGroupIndex] - childControl.get_clientHeight()) * (valign == 'middle' ? 0.5 : 1);
                top += offsetTop;
            }
        }

        return { x: left, y: top, dx: offsetLeft, dy: offsetTop };
    },

    postUpdate: function() {
        this._recalculateGroupIndex();

        if (!this._isHorizontal()) {
            if (this._control.isDependsOnChildWidth()) {
                this._control._set_clientWidth(this._getChildsWidth());
            }
        } else {
            if (this._control.isDependsOnChildHeight()) {
                this._control._set_clientHeight(this._getChildsHeight());
            }
        }

        this._updateChildPositions();
    },

    _updateChildPositions: function() {
        var controls = this._control.controls.getControlsInFlow();
        
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];

            if (isNullOrUndefined(control.$__wrapLayoutGroupIndex)) {
                continue;
            }

            var position = this.get_childPosition(control);
            var domElement = controls[i].domElement;
            var style = domElement.style;

            if (this._isHorizontal()) {
                DOM.setFloat(style, 'left');
                style.marginTop = position.dy + 'px';
                style.marginBottom = (this._groupDimensions[control.$__wrapLayoutGroupIndex] - control.get_clientHeight() - position.dy) + 'px';
            }
            
            if (this._isVertical()) {
                DOM.setFloat(style, 'left');
                style.marginTop = position.y + 'px';
                style.marginLeft = (position.dx - (position.y > 0 ? this._groupDimensions[control.$__wrapLayoutGroupIndex] : 0)) + 'px';
                var rightOffset = (this._groupDimensions[control.$__wrapLayoutGroupIndex] - control.get_clientWidth() - position.dx) + 'px';
                style.marginRight = rightOffset;
            }
        }
    },
    
    updateChildDom: function(child) {
    }
};

Nimble.Core.LayoutEngines.WrapLayout.createClass('Nimble.Core.LayoutEngines.WrapLayout', Nimble.Core.LayoutEngines.StackLayout);
Nimble.Core.LayoutEngines.LayoutFactory.register('wrap', Nimble.Core.LayoutEngines.WrapLayout);