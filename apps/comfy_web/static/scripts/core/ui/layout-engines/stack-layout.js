Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.StackLayout = function (control) {
    Nimble.Core.LayoutEngines.StackLayout.constructBase(this, [control]);
    this._orientation = control.options.orientation || 'vertical';
};

Nimble.Core.LayoutEngines.StackLayout.prototype = {
    _orientation: null,

    _isHorizontal: function() {
        return this._orientation == 'horizontal';
    },

    _isVertical: function() {
        return !this._isHorizontal();
    },

    // #region Stretched

    _getStretchBaseWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var thisWidth = this._control.get_innerWidth();
        var resultWidth = thisWidth;
        var stretchControls = 0;

        for (var i = 0; i < controls.length; i++) {
            var width = controls[i].get_width();

            if (this._isHorizontal()) {
                if (!width.isStretched()) {
                    resultWidth -= controls[i].get_outerWidth();
                } else {
                    stretchControls += width.getStretchLevel();
                    resultWidth -= controls[i].get_margin().get_width();
                }
            } else {
                if (width.isStretched()) {
                    var level = width.getStretchLevel();
                
                    if (level > stretchControls)
                        stretchControls = level;
                }
            }
        }

        if (stretchControls == 0)
            return { base: 0, length: 0 };

        return { base: resultWidth / stretchControls, length: stretchControls };
    },

    _getStretchBaseHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var stretchControls = 0;
        var thisHeight = this._control.get_innerHeight();
        var resultHeight = thisHeight;

        for (var i = 0; i < controls.length; i++) {
            var height = controls[i].get_height();

            if (this._isHorizontal()) {
                if (height.isStretched()) {
                    var level = height.getStretchLevel();
                
                    if (level > stretchControls)
                        stretchControls = level;
                }
            } else {
                if (!height.isStretched()) {
                    resultHeight -= controls[i].get_outerHeight();
                } else {
                    stretchControls += height.getStretchLevel();
                    resultHeight -= controls[i].get_margin().get_height();
                }
            }
        }

        if (stretchControls == 0)
            return { base: 0, length: 0 };

        return { base: resultHeight / stretchControls, length : stretchControls };
    },

    _getStretchedWidthForLastChild: function(child) {
        var resultWidth = child.get_margin().get_width();

        if (this._isHorizontal()) {
            for (var i = 0, length = this._control.controls.length; i < length; i++) {
                var control = this._control.controls[i];

                if (control.isInDocumentFlow() && control != child) {
                    resultWidth += control.get_outerWidth();
                }
            }
        }

        return this._control.get_innerWidth() - resultWidth;
    },

    _getStretchedHeightForLastChild: function(child) {
        var resultHeight = child.get_margin().get_height();

        if (this._isVertical()) {
            for (var i = 0, length = this._control.controls.length; i < length; i++) {
                var control = this._control.controls[i];

                if (control.isInDocumentFlow() && control != child) {
                    resultHeight += control.get_outerHeight();
                }
            }
        }

        return this._control.get_innerHeight() - resultHeight;
    },

    _containsStretched: function(controls, measure) {
        var getMeasure = 'get_' + measure;

        for (var i = 0; i < controls.length; i++) {
            if (controls[i][getMeasure]().isStretched) {
                return true;
            }
        }

        return false;
    },

    // returns true if @child is last stretched child in list of controls
    _isLastStretched: function(child, measure) {
        var isLastStretched = true;
        var otherChilds = this._control.controls;

        for (var i = otherChilds.length - 1; i >= 0 && otherChilds[i] !== child; i--) {
            if (otherChilds[i]['get_' + measure]().isStretched()) {
                isLastStretched = false;
            }
        }

        return isLastStretched;
    },

    _getChildStretchedMeasure: function(child, measure) {
        var result = null,
            targetRect = this._getTargetRect(child),
            getters = this._getGetters(measure);

        var parentMeasure = targetRect[getters['getMeasure']]();

        if (!child.parent || parentMeasure.isAutoSize()) {
            console.warn("Warning: stretched child should not be contained in auto-sized parent", child.options);
            return 0;
        }

        if ((this._isHorizontal() ? measure == 'width' : measure == 'height') && this._isLastStretched(child, measure)) {
            result = this[getters['getStretchedMeasureForLastChild']](child);
        } else {
            var measureValue = child[getters['getMeasure']]();
            var stretchBase = this[getters['getStretchBaseMeasure']]();
            var stretchLevel = measureValue.getStretchLevel();
            result = Math.round(stretchBase.base * stretchLevel);

            if (this._isHorizontal() && measure == 'height') {
                result -= child.get_margin().get_height();
            }
                    
            if (!this._isHorizontal() && measure == 'width') {
                result -= child.get_margin().get_width();
            }
        }

        return result;
    },

    // #endregion

    _getChildsWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;

        if (controls.length == 0) {
            result = this._getDomWidth();
        } else {
            result = 0;

            if (!this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var width = controls[i].get_outerWidth();

                    if (width > result)
                        result = width;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_outerWidth();
                }
            }
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
            result = 0;

            if (this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var height = controls[i].get_outerHeight();

                    if (height > result)
                        result = height;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_outerHeight();
                }
            }
        }

        if (result !== null) {
            var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();
            result += offset;
        }

        return result;
    },

    get_domWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = 0;

        if (controls.length == 0) {
            if (this._control.get_width().pixels > 0) {
                result = this._control.get_width().pixels;
            } else {
                result = this._control.get_width().isAutoSize() ? this._getDomWidth() : 0;
            }
        } else {
            result = 0;

            if (!this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var width = controls[i].get_domWidth();

                    if (width > result)
                        result = width;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_domWidth();
                }
            }
            
            var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();
            result += offset;
        }

        if (this._control.options.minWidth > result) {
            result = this._control.options.minWidth;
        }

        return result;
    },
    
    get_domHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = 0;
        
        if (controls.length == 0) {
            if (this._control.get_height().pixels > 0) {
                result = this._control.get_height().pixels - this._control.get_border().get_height();
            } else {
                result = this._control.get_height().isAutoSize() ? this._getDomHeight() : 0;
            }
        } else {
            result = 0;

            if (this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var height = controls[i].get_domHeight();

                    if (height > result)
                        result = height;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_domHeight();
                }
            }
            
            var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();
            result += offset;
        }

        if (this._control.options.minHeight > result) {
            result = this._control.options.minHeight;
        }

        return result;
    },

    get_childPosition: function(childControl) {
        var controls = this._control.controls.getControlsInFlow();
        var left = 0;
        var top = 0;
        var thisIsHorizontal = this._isHorizontal();
        var options = childControl.options;
        var margin = childControl.get_margin();

        for (var i = 0; i < controls.length; i++) {
            if (controls[i] == childControl) {
                break;
            }

            if (thisIsHorizontal) {
                left += controls[i].get_clientWidth();
            } else {
                top += controls[i].get_clientHeight();
            }
        }

        if (thisIsHorizontal) {
            if (options.valign == 'middle') {
                top = Math.floor((this._control.get_innerHeight() - childControl.get_clientHeight()) / 2);
            } else if (options.valign == 'bottom') {
                top = Math.floor(this._control.get_innerHeight() - childControl.get_clientHeight());
            }
        } else {
            if (options.halign == 'center') {
                left = Math.floor((this._control.get_innerWidth() - childControl.get_clientWidth()) / 2);
            } if (options.halign == 'right') {
                left = Math.floor(this._control.get_innerWidth() - childControl.get_clientWidth());
            }
        }

        return { x: left + margin.left, y: top + margin.top };
    },

    _updateChildDom: function(child) {
        var position = child.get_layoutEngine().get_position();

        if (this._isHorizontal()) {
            DOM.setFloat(child.domElement.style, 'left');
            child.domElement.style.marginTop = position.y + 'px';
        } else {
            child.domElement.style.marginLeft = position.x + 'px';
        }
    },

    postUpdate: function() {
        var controls = this._control.controls.getControlsInFlow();

        for (var i = 0; i < controls.length; i++) {
            this._updateChildDom(controls[i]);
        }
    }
};

Nimble.Core.LayoutEngines.StackLayout.createClass('Nimble.Core.LayoutEngines.StackLayout', Nimble.Core.LayoutEngines.BaseLayout);
Nimble.Core.LayoutEngines.LayoutFactory.register('stack', Nimble.Core.LayoutEngines.StackLayout);