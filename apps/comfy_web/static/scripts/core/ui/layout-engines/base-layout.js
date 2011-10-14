var logBuffer = {};
var layoutTime = 0;
var layoutCount = 0;

Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.BaseLayout = function (control) {
    Nimble.Core.LayoutEngines.BaseLayout.constructBase(this);
    this._control = control;
};

var ___baseLayout = Nimble.Core.LayoutEngines.BaseLayout;

Nimble.Core.LayoutEngines.BaseLayout.__getDomTester = function(width, height) {
    var domTester = Nimble.Core.LayoutEngines.BaseLayout.__domTester;
    
    if (!domTester) {
        domTester = Nimble.Core.LayoutEngines.BaseLayout.__domTester = DOM.create('div', document.body);
    } else {
        domTester.innerHTML = '';
    }
    
    domTester.style.cssText = 'position: absolute; left: 0; top: 0; visibility: hidden; width: ' + (width ? (width + 'px') : '""') + '; height: ' + (height ? (height + 'px') : '""') + ';';

    return domTester;
};

Nimble.Core.LayoutEngines.BaseLayout.prototype = {
    _control: null,

    // #region Abstract methods
    
    _getChildsWidth: function() {
        throw new Error('Abstract method');
    },

    _getChildsHeight: function() {
        throw new Error('Abstract method');
    },

    _getChildStretchedMeasure: function(child, measure) {
        throw new Error('Abstract method');
    },

    get_childPosition: function(childControl) {
        throw new Error('Abstract method');
    },

    get_domWidth: function() {
        throw new Error('Abstract method');
    },

    get_domHeight: function() {
        throw new Error('Abstract method');
    },

    // #endregion

    // #region Autosize, DOM

    dropFontPropertiesCache: function() {
        var control = this._control,
            domElement = this._control.domElement;

        domElement.__fontFamily = null;
        domElement.__fontSize = null;
        domElement.__lineHeight = null;
    },

    _getDomWidth: function() {
        var parent = this._control.parent,
            control = this._control,
            result = null;
        
        if (parent.isDependsOnChildWidth() || this._containsStretched(parent.controls, 'width')) {
            var domElement = this._control.domElement;
            
            var fontFamily = domElement.__fontFamily;
            var fontSize = domElement.__fontSize;
            
            if (!fontFamily) {
                fontFamily = DOM.getCssProperty(domElement, "font-family", "fontFamily");
                domElement.__fontFamily = fontFamily;
            }

            if (!fontSize) {
                fontSize = DOM.getCssProperty(domElement, "font-size", "fontSize");
                domElement.__fontSize = fontSize;
            }

            var text = domElement.innerText || domElement.textContent || '';

            var isTextNode = DOM.isTextNode(domElement);

            result = isTextNode && Application.getTextWidth ? Application.getTextWidth(text, fontFamily, fontSize.replace('px', '')*1) : null;

            if (result == null) {
                var limitHeight = control.get_height().isAutoSize() ? parent.get_innerHeight() - control.get_padding().get_height() - control.get_border().get_height() : control.get_innerHeight();
                var _domTester = Nimble.Core.LayoutEngines.BaseLayout.__getDomTester('', limitHeight);
                    
                var item = domElement.cloneNode(true);
                item.style.cssText = 'position: static; margin: 0; padding: 0; border: 0; float: none; display: inline; width: ""; height: ""; font-family: ' + fontFamily + '; font-size: ' + fontSize + ';';

                _domTester.appendChild(item);
                result = _domTester.offsetWidth;
            }
        }

        return result;
    },

    _getDomHeight: function() {
        var result = null,
            control = this._control,
            parent = this._control.parent;
        
        if (parent.isDependsOnChildHeight() || this._containsStretched(parent.controls, 'height')) {
            var domElement = this._control.domElement;

            var fontFamily = domElement.__fontFamily;
            var fontSize = domElement.__fontSize;
            var lineHeight = domElement.__lineHeight;
            
            if (!fontFamily) {
                fontFamily = DOM.getCssProperty(domElement, "font-family", "fontFamily");
                domElement.__fontFamily = fontFamily;
            }

            if (!fontSize) {
                fontSize = DOM.getCssProperty(domElement, "font-size", "fontSize");
                domElement.__fontSize = fontSize;
            }

            if (!lineHeight) {
                lineHeight = DOM.getCssProperty(domElement, "line-height", "lineHeight");
                domElement.__lineHeight = lineHeight;
            }

            var limitWidth = control.get_width().isAutoSize() ? parent.get_innerWidth() - control.get_padding().get_width() - control.get_border().get_width() : control.get_innerWidth();

            if (limitWidth < 0) {
                limitWidth = 0;
            }

            var isTextNode = DOM.isTextNode(domElement);

            if (isTextNode && Application.getTextHeight) {
                var lineHeightUnit = 0;

                var fontSize = fontSize.replace('px', '')*1;
            
                if (lineHeight == 'normal') {
                    lineHeightUnit = fontSize * 1.4;
                }

                if (lineHeight.endsWith('pt')) {
                    lineHeightUnit = Math.round(lineHeight.replace('pt','') * 4 / 3);
                }

                if (lineHeight.endsWith('%') || lineHeight.endsWith('px')) {
                    var unit = new DimensionUnit(lineHeight);
                    var lineHeightUnit = unit.percent * fontSize + unit.pixels;
                }

                var text = domElement.innerText || domElement.textContent || '';

                result = Application.getTextHeight(text, fontFamily, fontSize, lineHeightUnit, limitWidth*1);
            }

            if (result == null) {
                var _domTester = Nimble.Core.LayoutEngines.BaseLayout.__getDomTester(limitWidth, '');

                var item = domElement.cloneNode(true);
                item.style.cssText = 'position: static; margin: 0; padding: 0; border: 0; float: none; display: inline; line-height: ' + lineHeight + '; width: ""; height: ""; font-family: ' + fontFamily + '; font-size: ' + fontSize + ';';

                _domTester.appendChild(item);
                result = _domTester.offsetHeight;
            }
        }

        return result;
    },

    // #endregion

    isDependsOnChildSize: function() {
        return this.isDependsOnChildWidth() || this.isDependsOnChildHeight();
    },

    isDependsOnChildWidth: function() {
        return this._control.get_width().isAutoSize();
    },

    isDependsOnChildHeight: function() {
        return this._control.get_height().isAutoSize();
    },

    _getGetters: function(measure) {
        // some magic to increase performance
        var isWidthMeasure = measure == 'width';
        var cacheKey = isWidthMeasure ? '_getChildClientMeasure$getters$width' : '_getChildClientMeasure$getters$height';
        
        if (!___baseLayout[cacheKey]) {
            var pascalCaseMeasure = measure == 'width' ? 'Width' : 'Height';
            
            Nimble.Core.LayoutEngines.BaseLayout[cacheKey] = {
                getMeasure: 'get_' + measure,
                getStretchBaseMeasure: '_getStretchBase' + pascalCaseMeasure,
                getStretchedMeasureForLastChild: '_getStretched' + pascalCaseMeasure + 'ForLastChild',
                getChildsDomMeasure: '_getChilds' + pascalCaseMeasure,
                getInnerMeasure: 'get_inner' + pascalCaseMeasure
            };
        }

        return ___baseLayout[cacheKey];
    },

    _getTargetRect: function(child) {
        return this._control;
    },

    _getChildClientMeasure: function(child, measure) {
        if (!child._instantiatedInDom) {
            return null;
        }

        var targetRect = this._getTargetRect(child),
            getters = this._getGetters(measure),
            result = 0;

        var measureValue = child[getters['getMeasure']]();
        var parentMeasure = targetRect[getters['getMeasure']]();

        // measure: *
        if (measureValue.isStretched()) {
            result = this._getChildStretchedMeasure(child, measure);
        // measure: ?
        } else if (measureValue.isAutoSize()) {
            result = child.get_layoutEngine()[getters['getChildsDomMeasure']]();
        // measure: px, or %
        } else {
            result = measureValue.pixels;

            if (measureValue.percent > 0) {
                if (child.parent && !parentMeasure.isAutoSize()) {
                    var parentMeasure = targetRect[getters['getInnerMeasure']]();
                    result += measureValue.percent * parentMeasure;
                } else {
                    console.warn("Warning: relatively sized child should not be contained in auto-sized parent", child.options);
                }
            }
        }

        if (result && result < 0) {
            result = 0;
        }

        return result;
    },

    _recalculateNotFlowWidth: function() {
        var width = this._control.get_width();
        var clientWidth = null;

        if (width.isAutoSize()) {
            clientWidth = this._getChildsWidth();
        } else if (width.isStretched()) {
            clientWidth = Application.get_clientWidth();
        } else {
            clientWidth = Application.get_clientWidth() * width.percent + width.pixels
        }

        this._control._set_clientWidth(clientWidth);
    },

    recalculateClientWidth: function() {
        if (!this._control._visible) {
            this._control._set_clientWidth(0);
        } else {
            var width = this._control.get_width();

            if (width.isFixed()) {
                this._control._set_clientWidth(width.pixels);
            } else {
                if (!this._control.parent || !this._control._inDocumentFlow) {
                    this._recalculateNotFlowWidth();
                } else {
                    this._control._set_clientWidth(this._control.parent.get_layoutEngine().get_childClientWidth(this._control));
                }
            }
        }
    },

    _recalculateNotFlowHeight: function() {
        var height = this._control.get_height();
        var clientHeight = null;

        if (height.isAutoSize()) {
            clientHeight = this._getChildsHeight();
        } else if (height.isStretched()) {
            clientHeight = Application.get_clientHeight();
        } else {
            clientHeight = Application.get_clientHeight() * height.percent + height.pixels
        }

        this._control._set_clientHeight(clientHeight);
    },

    recalculateClientHeight: function() {
        if (!this._control._visible) {
            this._control._set_clientHeight(0);
        } else {
            var height = this._control.get_height();

            if (height.isFixed()) {
                this._control._set_clientHeight(height.pixels);
            } else {
                if (!this._control._inDocumentFlow || !this._control.parent) {
                    this._recalculateNotFlowHeight();
                } else {
                    this._control._set_clientHeight(this._control.parent.get_layoutEngine().get_childClientHeight(this._control));
                }
            }
        }
    },

    get_position: function() {
        if (this._control.parent) {
            return this._control.parent.get_layoutEngine().get_childPosition(this._control);
        } else {
            return { x: 0, y: 0 };
        }
    },

    get_childClientWidth: function(childControl) {
        return this._getChildClientMeasure(childControl, 'width');
    },

    get_childClientHeight: function(childControl) {
        return this._getChildClientMeasure(childControl, 'height');
    },

    updateDom: function() {
        var control = this._control;
        var clientWidth = control.get_clientWidth();
        var clientHeight = control.get_clientHeight();
        var padding = control.get_padding();
        var border = control.get_border();
        var margin = control.get_margin();
        var width = clientWidth !== null ? (Math.max(clientWidth - padding.get_width() - border.get_width(), 0)) : null;
        var height = clientHeight !== null ? (Math.max(clientHeight - padding.get_height() - border.get_height(), 0)) : null;

        DOM.setBoundingRect(this._control.domElement, width, height);
    },

    updateChildDom: function(child) {
    },

    __getChildsForUpdate: function(control) {
        if (!control.get_visible()) {
            return [];
        }

        // copy childs array and sort it
        // non-strethed controls should be at first place
        // width-strethed controls should be at second place
        // height-stretched controls should be at third place
        var childs = control.controls.slice(0);
        var orientation = control.options.orientation || 'vertical';
        var isHorizontal = orientation == 'horizontal';

        if (childs.length > 1) {
            childs.sort(function $sort (a, b) {
                if (isHorizontal) {
                    var aWidth = a.get_width().isStretched(), bWidth = b.get_width().isStretched();
                    if (aWidth == bWidth) { return a.__cid < b.__cid ? 1 : -1; }
                    if (aWidth) { return -1; }
                    if (bWidth) { return 1; }
                } else {
                    var aHeight = a.get_height().isStretched(), bHeight = b.get_height().isStretched();
                    if (aHeight == bHeight) { return a.__cid < b.__cid ? 1 : -1; }
                    if (aHeight) { return -1; }
                    if (bHeight) { return 1; }
                }
            });
        }
                
        return childs;
    },

    _updateItem: function(control, updating, visited) {
        var containsInVisited = !!visited[control.__cid];
        var dependsOnChildSize = control.isDependsOnChildSize();

        if (!containsInVisited) {
            var childs = this.__getChildsForUpdate(control);

            if (dependsOnChildSize && childs.length == 0) {
                visited[control.__cid] = true;
                containsInVisited = true; // if there are no childs in control then we can think that all childs are already updated
            } else {
                updating.add(control); // add control to queue. We must invoke postUpdate after all its controls would be updated
                updating.add(childs);
            }
        }

        // if we could calculate at least one of control's measure then we should do that
        if (dependsOnChildSize && !containsInVisited) {
            if (!control.isDependsOnChildWidth()) {
                control.get_layoutEngine().recalculateClientWidth();
            }

            if (!control.isDependsOnChildHeight()) {
                control.get_layoutEngine().recalculateClientHeight();
            }
        }

        // autosized control updates only in the second pass, other controls updates normally
        if ((!containsInVisited && !dependsOnChildSize) || (containsInVisited && dependsOnChildSize)) {
            control.get_layoutEngine().recalculateClientWidth();
            control.get_layoutEngine().recalculateClientHeight();
        }

        if (containsInVisited) {
            control.postUpdate();
        } else {
            visited[control.__cid] = true;
        }
    },

    update: function() {
        var time = new Date();
        var visited = {};
        var updating = [this._control];

        if (this._control.parent && this._control.isInDocumentFlow()) {
            var targetParent = this._control.parent;

            // find closest not auto-sized parent
            while (targetParent.parent && targetParent.isDependsOnChildSize()) {
                targetParent = targetParent.parent;
            }
            
            updating.add(targetParent);
        }

        while (updating.length > 0) {
            var control = updating.pop();
            this._updateItem(control, updating, visited);
        }

        for (var i in visited) {
            var control = Controls[i];
            
            if (control.isAttachedToDom()) {
                control.updateDom();
            }
        }

        layoutTime += new Date() - time;
        layoutCount++;

        return visited;
    },

    postUpdate: function() {
    }
};

Nimble.Core.LayoutEngines.BaseLayout.createClass('Nimble.Core.LayoutEngines.BaseLayout');