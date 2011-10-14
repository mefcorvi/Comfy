$get = function(id) {
    return document.getElementById(id);
};

var DOM = {
    _getComputedStyle: (document.defaultView && document.defaultView.getComputedStyle),
    _rdashAlpha: /-([a-z])/ig,
    _fcamelCase: function(all, letter) {
        return letter.toUpperCase();
    },
    
    jqGC: null,

    discardElement: function(element) {
        var jqGC = DOM.jqGC;

        if (!DOM.jqGC) {
            jqGC = document.createElement('div');
            jqGC.style.display = 'none';
            document.body.appendChild(jqGC);
            DOM.jqGC = jqGC;
        }
        
        var tagName = element.tagName;

        if (element.nodeType == 1 && tagName != 'TR' && tagName != 'TD') {            

            if (element.parentNode) {
                element.parentNode.removeChild(element);
            }
            
            element.innerHTML = '';
        } else {
            jqGC.appendChild(element);
            jqGC.innerHTML = '';
        }
    },

    remove: function(element) {
        if (!element) {
            return;
        }

        // Prevent memory leaks
        jQuery("*", element).each(function() {
            jQuery.event.remove(this);
            jQuery.removeData(this);
        });

        jQuery.event.remove(element);
        jQuery.removeData(element);
            
        // deleting of text nodes which been detached from a parent in IE may cause an error
        try {
            if (element.nodeType != 1 || element.parentNode)
                DOM.discardElement(element);
        }
        catch (e) { }
    },

    _nodeCaches: {},

    /**
    * Creates a new DOM node by tag. If parent node parameter is passed then created node will be appended to parent node.
    */
    create: function(tag, parentNode, params) {
        var time = new Date();
        var node;
        
        if (!params) {
            if (!DOM._nodeCaches[tag]) {
                DOM._nodeCaches[tag] = document.createElement(tag);
            }
            
            node = DOM._nodeCaches[tag].cloneNode(false);
        } else {
            node = document.createElement(tag);
        }
        
        if (parentNode) {
            parentNode.appendChild(node);
        }

        if (params) {
            for (var prop in params) {
                if (params.hasOwnProperty(prop)) {
                    node[prop] = params[prop];
                }
            }
        }

        return node;
    },
    
    appendChild: function(node, parent) {
        parent.appendChild(node);
    },

    /**
    * Removes a node from its parent
    */
    detachNode: function(node) {
        // if node is not detached yet
        if (node.parentNode) {
            node.parentNode.removeChild(node);
        }
    },

    /**
    * Creates a new text node in DOM and append it to parent node if it is passed.
    */
    createTextNode: function(value, parentNode) {
        var node = document.createTextNode(value || "");
        
        if (parentNode) {
            parentNode.appendChild(node);
        }
        
        return node;
    },

    disableSelection: function(target) {
        if (typeof (target.onselectstart) != "undefined") { //IE
            target.onselectstart = function() { return false; };
        }
        else if (typeof (target.style.MozUserSelect) != "undefined") { //Firefox route
            target.style.MozUserSelect = "none";
        }
        else { //All other route (ie: Opera)
            $("*", target).each(function() {
                this.unselectable = "on";
            });
        }
    },
    
    // a lite, a bit faster and a more buggy version of jquery curCss
    getCssProperty: function(elem, prop, camelCase) {
        var currentStyle = elem.currentStyle;
        
        if (currentStyle) {
            var camelCase = camelCase || prop.replace(DOM._rdashAlpha, DOM._fcamelCase);
            return currentStyle[camelCase] || currentStyle[prop];
        } else if (DOM._getComputedStyle) {
            var computedStyle = elem.ownerDocument.defaultView.getComputedStyle(elem, null);

            if (computedStyle) {
                return computedStyle.getPropertyValue(prop);
            }
        }
    },
    
    getPaddingHeight: function(elem) {
        var paddingTop = DOM.getCssProperty(elem, "padding-top", "paddingTop");
        var paddingBottom = DOM.getCssProperty(elem, "padding-bottom", "paddingBottom");

        return paddingTop.substr(0, paddingTop.length - 2) * 1 + paddingBottom.substr(0, paddingBottom.length - 2) * 1;
    },

    getPaddingWidth: function(elem) {
        var paddingLeft = DOM.getCssProperty(elem, "padding-left", "paddingLeft");
        var paddingRight = DOM.getCssProperty(elem, "padding-right", "paddingRight");

        return paddingLeft.substr(0, paddingLeft.length - 2) * 1 + paddingRight.substr(0, paddingRight.length - 2) * 1;
    },

    /**
    * Returns height of the margin. If margin height is negative then return zero.
    */
    getMarginHeight: function(elem) {
        var marginTop = DOM.getCssProperty(elem, "margin-top", "marginTop");
        var marginBottom = DOM.getCssProperty(elem, "margin-bottom", "marginBottom");

        return Math.max(marginTop.substr(0, marginTop.length - 2) * 1 + marginBottom.substr(0, marginBottom.length - 2) * 1, 0);
    },

    /**
    * Returns width of the margin. If margin width is negative then return zero.
    */
    getMarginWidth: function(elem) {
        var marginLeft = DOM.getCssProperty(elem, "margin-left", "marginLeft");
        var marginRight = DOM.getCssProperty(elem, "margin-right", "marginRight");

        return Math.max(marginLeft.substr(0, marginLeft.length - 2) * 1 + marginRight.substr(0, marginRight.length - 2) * 1, 0);
    },

    getBorderHeight: function(elem) {
        var borderTop = DOM.getCssProperty(elem, "border-top-width", "borderTopWidth");
        var borderBottom = DOM.getCssProperty(elem, "border-bottom-width", "borderBottomWidth");

        return borderTop.substr(0, borderTop.length - 2) * 1 + borderBottom.substr(0, borderBottom.length - 2) * 1;
    },

    getBorderWidth: function(elem) {
        var borderLeft = DOM.getCssProperty(elem, "border-left-width", "borderLeftWidth");
        var borderRight = DOM.getCssProperty(elem, "border-right-width", "borderRightWidth");

        return borderLeft.substr(0, borderLeft.length - 2) * 1 + borderRight.substr(0, borderRight.length - 2) * 1;
    },
    
    setPosition: function(node, pos) {
        var elStyle = node.style;

        elStyle.left = isNullOrUndefined(pos.x) ? "" : (pos.x + "px");
        elStyle.top = isNullOrUndefined(pos.y) ? "" : (pos.y + "px");
    },

    get_scrollHeight: function(item) {
        if ($.browser.opera) {
            return item.scrollHeight + DOM.getPaddingHeight(item);
        }

        return item.scrollHeight;
    },

    get_scrollWidth: function(item) {
        if ($.browser.opera) {
            return item.scrollWidth + DOM.getPaddingWidth(item);
        }

        return item.scrollWidth;
    },

    get_childsWidth: function(elem) {
        var childs = elem.childNodes;
        var result = 0;

        for (var i = 0; i < childs.length; i++) {
            result += childs[i].offsetWidth || 0;
        }

        return result;
    },

    get_childsHeight: function(elem) {
        var childs = elem.childNodes;
        var result = 0;

        for (var i = 0; i < childs.length; i++) {
            result += childs[i].offsetHeight || 0;
        }

        return result;
    },

    /**
    * Sets the bounding rectangle for some DOM node.
    * @param HTMLElement node
    * @param int width
    * @param int height
    */
    setBoundingRect: function(node, width, height) {
        var elStyle = node.style;

        elStyle.width = (isNullOrUndefined(width) || width < 0) ? "" : (width + "px");
        elStyle.height = (isNullOrUndefined(height) || height < 0) ? "" : (height + "px");
    },

    support: {
        cssFloat: jQuery.support.cssFloat
    },
    
    setFloat: function(style, floatValue) {
        var cssFloat = DOM.support.cssFloat;

        if (cssFloat) {
            style.cssFloat = floatValue;
        } else {
            style.styleFloat = floatValue;
        }
    },

    /**
    * Inserts the new node after the reference node
    * @param HTMLElement referenceNode
    * @param HTMLElement newNode
    */
    insertAfter: function(referenceNode, newNode) {
        referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
    },
    
    /**
    * Inserts the new node before the reference node
    * @param HTMLElement referenceNode
    * @param HTMLElement newNode
    */
    insertBefore: function(referenceNode, newNode) {
        referenceNode.parentNode.insertBefore(newNode, referenceNode);
    },
    
    /**
    * Inserts the new node after or before (determined by type) the reference node
    * @param String type
    * @param HTMLElement referenceNode
    * @param HTMLElement newNode
    */
    insert: function(type, referenceNode, newNode) {
        if (type == 'after') {
            DOM.insertAfter(referenceNode, newNode);
        } else if (type == 'before') {
            DOM.insertBefore(referenceNode, newNode);
        } else {
            throw new Error('Unknown type of insertion');
        }
    },

    isFocusable: function(node) {
        return isDefined(node.tabIndex) && node.tabIndex >= 0;
    },

    isTextNode: function(node) {
        return node.nodeType == 3 || (node.childNodes.length == 1 && node.childNodes[0].nodeType == 3);
    }
};