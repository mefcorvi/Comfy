/**
* TODO:
* 1) Avoid of code duplicating
*/

Type.createNamespace('Phoenix.UI');

Phoenix.UI.ScrollablePanel = function() {
    Phoenix.UI.ScrollablePanel.constructBase(this);
};

Phoenix.UI.ScrollablePanel.prototype = {
    _innerContainer: null,
    _vscrollNode: null,
    _maxScrollY: null,
    _horizontal: null,
    _vertical: null,
    _measures: {
        width: 14,
        height: 14,
        vSliderWidth: 12,
        vSliderHeight: 36,
        hSliderWidth: 36,
        hSliderHeight: 12,
        vArrowHeight: 17,
        hArrowWidth: 17
    },

    defaultOptions: {
        width: '*',
        height: '*'
    },

    initFromOptions: function(options) {
        options.cssClass = (options.cssClass || '') + ' scrollable_panel';
        
        if (isFunction(options.onScrollChanged)) {
            this.add_scrollYChanged(options.onScrollChanged, this);
        }

        this._vertical = !isNullOrUndefined(options.vertical) ? options.vertical : true;
        this._horizontal = !!options.horizontal;

        this._vscrollVisible = this._vertical;
        this._hscrollVisible = this._horizontal;
        

        if (this._horizontal) {
            options.cssClass += " hscrollable_panel";
        }

        if (this._vertical) {
            options.cssClass += " vscrollable_panel";
        }

        Phoenix.UI.ScrollablePanel.callBase(this, "initFromOptions", [ options ]);
    },
    
    get_childsContainer: function() {
        return this._innerContainer;
    },
    
    focus: function() {
        if (this._focusDetector) {
            this._focusDetector.focus();
        }
    },

    blur: function() {
        if (this._focusDetector) {
            this._focusDetector.blur();
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create("div", domElement);
        this._innerContainer = DOM.create("div", this.domElement);
        this._innerContainer.className = "scroll_panel_container";
        
        var className = "scroll_panel_container";
        
        Phoenix.UI.ScrollablePanel.callBase(this, "instantiateInDom", [ domElement ]);

        this._focusDetector = DOM.create("a", this.domElement, { className: 'focus_detector', href: 'javascript:void(0)' });
        
        $(this.domElement).mousedown(function(ev) {
            setTimeout(function() {
                if (!DOM.isFocusable(ev.target))
                    this._focusDetector.focus();
            }.bind(this), 0);
        }.bind(this));

        $(this.domElement).keydown(this._onKeyDownHandler.bind(this));

        if (this._vertical) {
            this._initVScrollInDom();
        }
        
        if (this._horizontal) {
            this._initHScrollInDom();
        }
    },

    _onKeyDownHandler: function(ev) {
        var scrollSpeed = 10;

        if (ev.keyCode == Keys.Down) {
            this.scrollBy(0, scrollSpeed);
        }
        if (ev.keyCode == Keys.Up) {
            this.scrollBy(0, -scrollSpeed);
        }
        if (ev.keyCode == Keys.Left) {
            this.scrollBy(-scrollSpeed, 0);
        }
        if (ev.keyCode == Keys.Right) {
            this.scrollBy(scrollSpeed, 0);
        }
        if (ev.keyCode == Keys.PgDown) {
            this.scrollByPage(0, 1.9);
        }
        if (ev.keyCode == Keys.PgUp) {
            this.scrollByPage(0, -1.9);
        }
        if (ev.keyCode == Keys.Home) {
            this.scrollToTop();
        }
        if (ev.keyCode == Keys.End) {
            this.scrollToBottom();
        }
    },
    
    _initVScrollInDom: function() {
        this._vscrollNode = DOM.create("div", this.domElement);
        this._vscrollNode.className = "vscroll_container";

        $(this.domElement).bind("mousewheel", function(ev, delta) {
            if (this._vscrollVisible) {
                this.scrollBy(0, -delta*25);
                ev.stopPropagation();
            }
        }.bind(this));
        
        DOM.disableSelection(this._vscrollNode);
        
        this._topArrow = DOM.create("a", this._vscrollNode, { className: "top_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_topArrow');
        this._vslider = DOM.create("a", this._vscrollNode, { className: "slider_item", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_sliderItem');
        this._bottomArrow = DOM.create("a", this._vscrollNode, { className: "bottom_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_bottomArrow');
        
        $(this._vslider).bind("mousedown", function(ev) {
            this._startVSliderDrag(ev.clientY);
            ev.preventDefault();
            ev.stopPropagation();
        }.bind(this));
        
        $(this._vscrollNode).bind("mousedown", function(ev) {
            var sliderPos = this._convertVScrollToSliderPos(this._innerContainer.scrollTop);
            
            var y = ev.layerY || ev.offsetY;
            
            this.scrollByPage(0, y > sliderPos ? 1 : -1);
        }.bind(this));
        
        $(this._topArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(0, -1);
            ev.stopPropagation();
        }.bind(this));
        
        $(this._bottomArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(0, 1);
            ev.stopPropagation();
        }.bind(this));
    },

     _initHScrollInDom: function() {
        this._hscrollNode = DOM.create("div", this.domElement);
        this._hscrollNode.className = "hscroll_container";

        // if vertical scroll is not available then attach to domElement
        var wheelNode = this._vertical ? this._hscrollNode : this.domElement;
        $(wheelNode).bind("mousewheel", function(ev, delta) {
            if (this._hscrollVisible) {
                this.scrollBy(-delta*25, 0);
                ev.preventDefault();
                ev.stopPropagation();
            }
        }.bind(this));

        DOM.disableSelection(this._hscrollNode);
        
        this._leftArrow = DOM.create("a", this._hscrollNode, { className: "left_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_leftArrow');
        this._hslider = DOM.create("a", this._hscrollNode, { className: "slider_item", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_sliderItem');
        this._rightArrow = DOM.create("a", this._hscrollNode, { className: "right_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_rightArrow');
        
        $(this._hslider).bind("mousedown", function(ev) {
            this._startHSliderDrag(ev.clientX);
            ev.preventDefault();
            ev.stopPropagation();
        }.bind(this));
        
        $(this._hscrollNode).bind("mousedown", function(ev) {
            var sliderPos = this._convertHScrollToSliderPos(this._innerContainer.scrollLeft);
            var x = ev.layerX || ev.offsetX;
            
            this.scrollByPage(x > sliderPos ? 1 : -1, 0);
        }.bind(this));
        
        $(this._leftArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(-1, 0);
            ev.stopPropagation();
        }.bind(this));
        
        $(this._rightArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(1, 0);
            ev.stopPropagation();
        }.bind(this));
    },
    
    _startScrollByArrows: function(dirX, dirY) {
        this._scrollInterval = window.setInterval(function() {
            this.scrollBy(dirX * 7, dirY * 7);
        }.bind(this), 20);
        
        var mouseUp = function(ev) {
            $(document.body).unbind("mouseup", mouseUp);
            window.clearInterval(this._scrollInterval);
            ev.stopPropagation();
        }.bind(this);
    
        $(document.body).bind("mouseup", mouseUp);       
    },
    
    _startVSliderDrag: function(startY) {
        var startScroll = this._innerContainer.scrollTop;
    
        var mouseMove = function(ev) {
            var delta = ev.clientY - startY;
            this.set_scrollY(startScroll + this._convertVSliderPosToScroll(delta + this._measures.vSliderHeight));
            ev.stopPropagation();
        }.bind(this);
    
        var mouseUp = function(ev) {
            $(document.body).unbind("mouseup", mouseUp);
            $(document.body).unbind("mousemove", mouseMove);
            ev.stopPropagation();
        }.bind(this);
    
        $(document.body).bind("mouseup", mouseUp);
        $(document.body).bind("mousemove", mouseMove);
    },
    
    _startHSliderDrag: function(startX) {
        var startScroll = this._innerContainer.scrollLeft;
    
        var mouseMove = function(ev) {
            var delta = ev.clientX - startX;
            this.set_scrollX(startScroll + this._convertHSliderPosToScroll(delta + this._measures.hSliderWidth));
            ev.stopPropagation();
        }.bind(this);
    
        var mouseUp = function(ev) {
            $(document.body).unbind("mouseup", mouseUp);
            $(document.body).unbind("mousemove", mouseMove);
            ev.stopPropagation();
        }.bind(this);
    
        $(document.body).bind("mouseup", mouseUp);
        $(document.body).bind("mousemove", mouseMove);
    },

    scrollBy: function(scrollX, scrollY) {
        if (scrollY != 0) {
            this.set_scrollY(this.get_scrollY() + scrollY);
        }
        if (scrollX != 0) {
            this.set_scrollX(this.get_scrollX() + scrollX);
        }
    },
    
    scrollByPage: function(pageX, pageY) {
        var xscroll = 0;
        var yscroll = 0;

        if (pageY != 0) {
            var innerHeight = this.get_innerHeight();
        
            if (innerHeight <= 0) {
                return;
            }
        
            yscroll = Math.round(innerHeight / 2 * pageY);
        }

        if (pageX != 0) {
            var innerWidth = this.get_innerWidth();
        
            if (innerWidth <= 0) {
                return;
            }
        
            xscroll = Math.round(innerWidth / 2 * pageX);
        }
        
        this.scrollBy(xscroll, yscroll);
    },
    
    get_scrollY: function() {
        return this.__cachedScrollY || 0;
    },
    
    set_scrollY: function(value) {
        var element = this._innerContainer;
    
        if (!element) {
            return;
        }
        
        if (this._maxScrollY > 0) {
            var delta = Math.abs(this.__cachedScrollY - value);
        
            if (value > this._maxScrollY - delta) {
                value = this._maxScrollY;
            } else if (value < delta) {
                value = 0;            
            }
        
            element.scrollTop = value;
            var old = this.__cachedScrollY;
            this.__cachedScrollY = value;
            
            this.raise_scrollYChanged({ newValue: value, oldValue: old, maxValue: this._maxScrollY });
            
            this.__setVSliderPosition();
        }
    },
    
    get_scrollX: function() {
        return this.__cachedScrollX || 0;
    },
    
    set_scrollX: function(value) {
        var element = this._innerContainer;
    
        if (!element) {
            return;
        }
        
        if (this._maxScrollX > 0) {
            var delta = Math.abs(this.__cachedScrollX - value);
        
            if (value > this._maxScrollX - delta) {
                value = this._maxScrollX;
            } else if (value < delta) {
                value = 0;            
            }
        
            element.scrollLeft = value;
            var old = this.__cachedScrollX;
            this.__cachedScrollX = value;
            
            this.raise_scrollXChanged({ newValue: value, oldValue: old, maxValue: this._maxScrollX });
            
            this.__setHSliderPosition();
        }
    },

    __setVSliderPosition: function() {
        if (!this._vslider || isNullOrUndefined(this.__cachedScrollY)) {
            return;
        }
        
        if (this.__cachedScrollY > this._maxScrollY) {
            this.__cachedScrollY = this._maxScrollY;
            this._innerContainer.scrollTop = this.__cachedScrollY;
        }
            
        this._vslider.style.top = this._convertVScrollToSliderPos(this.__cachedScrollY) + "px";
    },
    
    __setHSliderPosition: function() {
        if (!this._hslider || isNullOrUndefined(this.__cachedScrollX)) {
            return;
        }
        
        if (this.__cachedScrollX > this._maxScrollX) {
            this.__cachedScrollX = this._maxScrollX;
            this._innerContainer.scrollLeft = this.__cachedScrollX;
        }
            
        this._hslider.style.left = this._convertHScrollToSliderPos(this.__cachedScrollX) + "px";
    },

    scrollToTop: function() {
        this.set_scrollY(0);
    },
    
    scrollToBottom: function() {
        this.set_scrollY(this._maxScrollY);
    },

    scrollToLeft: function() {
        this.set_scrollX(0);
    },

    _convertVScrollToSliderPos: function(scrollY) {
        return Math.round(scrollY / this._maxScrollY * (this.get_innerHeight() - 2 * this._measures.vArrowHeight - this._measures.vSliderHeight) + this._measures.vArrowHeight);
    },
    
    _convertVSliderPosToScroll: function(sliderPos) {
        return Math.round(this._maxScrollY * (sliderPos - this._measures.vArrowHeight) / (this.get_innerHeight() - 2 * this._measures.vArrowHeight - this._measures.vSliderHeight));
    },
    
    _convertHScrollToSliderPos: function(scrollX) {
        return Math.round(scrollX / this._maxScrollX * (this.get_innerWidth() - 2 * this._measures.hArrowWidth - this._measures.hSliderWidth) + this._measures.hArrowWidth);
    },
    
    _convertHSliderPosToScroll: function(sliderPos) {
        return Math.round(this._maxScrollX * (sliderPos - this._measures.hArrowWidth) / (this.get_innerWidth() - 2 * this._measures.hArrowWidth - this._measures.hSliderWidth));
    },

    get_innerWidth: function(noCache) {
        var innerWidth = Phoenix.UI.ScrollablePanel.callBase(this, "get_innerWidth", [ noCache ]);
        
        return innerWidth - (this._vertical && this._vscrollVisible ? this._measures.width : 0);
    },
    
    get_innerHeight: function(noCache) {
        var innerHeight = Phoenix.UI.ScrollablePanel.callBase(this, "get_innerHeight", [ noCache ]);
        
        return innerHeight - (this._horizontal && this._hscrollVisible ? this._measures.height : 0);
    },
    
    isDependsOnChildWidth: function() {
        return this._horizontal || this.get_width().isAutoSize();
    },

    isDependsOnChildHeight: function() {
        return this._vertical || this.get_height().isAutoSize();
    },

    updateDom: function() {
        DOM.setBoundingRect(this._innerContainer, this.get_innerWidth(), this.get_innerHeight());
        Phoenix.UI.ScrollablePanel.callBase(this, "updateDom");
    },

    postUpdate: function() {
        Phoenix.UI.ScrollablePanel.callBase(this, "postUpdate");
        
        if (this._vertical) {
            this.updateVerticalScroll();
        }
            
        if (this._horizontal) {
            this.updateHorizontalScroll();
        }
    },

    updateVerticalScroll: function() {
        if (!this._vertical) {
            throw new Error('Vertical scroll is not available');
        }

        var scrollHeight = this.get_layoutEngine()._getChildsHeight();
        var offsetHeight = this.get_innerHeight();

        if (offsetHeight && (scrollHeight > offsetHeight)) {
            this._maxScrollY = scrollHeight - offsetHeight;
            this.__setVSliderPosition();
                
            if (offsetHeight <= this._measures.vSliderHeight + this._measures.height + this._measures.vArrowHeight * 2) {
                this._vslider.style.display = 'none';
            } else {
                this._vslider.style.display = '';
            }

            if (!this._vscrollVisible) {
                this._vscrollVisible = true;
                this._vscrollNode.style.display = "";
                this.update();
            }
        } else {
            if (this.get_scrollY() > 0) {
                this.scrollToTop();
            }
                
            if (this._vscrollVisible) {
                this._vscrollVisible = false;
                this._vscrollNode.style.display = "none";
                this.update();
            }
        }
    },

    updateHorizontalScroll: function() {
        if (!this._horizontal) {
            throw new Error('Horizontal scroll is not available');
        }

        var scrollWidth = this.get_layoutEngine()._getChildsWidth();
        var offsetWidth = this.get_innerWidth();
            
        if (offsetWidth && (scrollWidth > offsetWidth)) {
            this._maxScrollX = scrollWidth - offsetWidth;
            this.__setHSliderPosition();
                
            if (offsetWidth <= this._measures.hSliderWidth + this._measures.width + this._measures.hArrowWidth * 2) {
                this._hslider.style.display = 'none';
            } else {
                this._hslider.style.display = '';
            }

            if (!this._hscrollVisible) {
                this._hscrollVisible = true;
                this._hscrollNode.style.display = "";
                this.update();
            }
        } else {
            if (this.get_scrollX() > 0) {
                this.scrollToLeft();
            }
                
            if (this._hscrollVisible) {
                this._hscrollVisible = false;
                this._hscrollNode.style.display = "none";
                this.update();
            }
        }
    },

    removeVerticalScrollFromDom: function() {
        DOM.remove(this._vslider);
        DOM.remove(this._topArrow);
        DOM.remove(this._bottomArrow);
        DOM.remove(this._vscrollNode);
    },

    removeHorizontalScrollFromDom: function() {
        DOM.remove(this._hslider);
        DOM.remove(this._leftArrow);
        DOM.remove(this._rightArrow);
        DOM.remove(this._hscrollNode);
    },
    
    free: function() {
        if (this._vertical) {
            this.removeVerticalScrollFromDom()
        };

        if (this._horizontal) {
            this.removeHorizontalScrollFromDom()
        };

        DOM.remove(this._focusDetector);
        DOM.remove(this._innerContainer);
        Phoenix.UI.ScrollablePanel.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.ScrollablePanel.prototype, [
    { name: 'scrollY', autoEvent: true },
    { name: 'scrollX', autoEvent: true }
]);

Phoenix.UI.ScrollablePanel.createClass('Phoenix.UI.ScrollablePanel', Control);
ControlsFactory.registerControl('scrollablePanel', Phoenix.UI.ScrollablePanel);
