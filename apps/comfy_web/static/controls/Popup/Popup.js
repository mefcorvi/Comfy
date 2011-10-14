Type.createNamespace('Phoenix.UI');

Phoenix.UI.Popup = function() {
    Phoenix.UI.Popup.constructBase(this);
    this._fadeSpeed = $.browser.msie ? 0 : 'fast';
};

Phoenix.UI.Popup.prototype = {
    _uri: null,
    _page: null,
    _buttons: null,
    _scrolling: null,
    _fadeSpeed: null,
    container: null,

    defaultOptions: {
        'visible': false
    },
    
    staticVars: {
        level: 0
    },

    focus: function() {
        var firstControl = this.findControl(function(control) {
            return control.isFocusable();
        });
        
        if (firstControl) {
            firstControl.focus();
        }
    },

    initFromOptions: function(options) {
        var thisObj = this;
        this._title = options.title || '';
        this._inDocumentFlow = false;
        var isAutoSizeHeight = options.height == '?';
        var isAutoSizeWidth = options.width == '?';
        this._scrolling = options.scrolling && (!isAutoSizeHeight || options.maxHeight);

        this.attachEventsFromOptions(options, ["onCommand", "onOpen", "onClose", "onUriLoaded"]);

        var oldControls = options.controls || [];

        options.cssClass = options.cssClass || 'popup';
        options.controls = [
            {
                id: 'innerBox', 
                type: 'box',
                width: isAutoSizeWidth ? '?' : '*',
                height: isAutoSizeHeight ? '?' : '*',
                onLoad: function() {
                    this._innerWidthDelta = 17;
                },
                controls: [
                    {
                        type: 'panel',
                        height: isAutoSizeHeight ? '?' : '*',
                        width: isAutoSizeWidth ? '?' : '*',
                        orientation: 'vertical',
                        id: 'innerPanel',
                        controls: [
                            {
                                type: 'container',
                                tag: 'h1',
                                height: '26px',
                                width: '100%',
                                padding: '4px',
                                margin: '0 0 0 1',
                                cssClass: 'popup_header',
                                orientation: 'horizontal',
                                onLoad: function() {
                                    DOM.disableSelection(this.domElement);
                                    thisObj._attachDragHandlers(this.domElement);
                                },
                                onFree: function() {
                                    thisObj._detachDragHandlers(this.domElement);
                                },
                                controls: [
                                    {
                                        type: 'label',
                                        width: '*',
                                        height: '*',
                                        text: this._title,
                                        onLoad: function() {
                                            thisObj.add_titleChanged(function(sender, args) {
                                                this.set_text(args.newValue);
                                            }, this)
                                        }
                                    },
                                    {
                                        type: 'link',
                                        width: '15',
                                        height: '15',
                                        focusable: false,
                                        text: 'Close popup',
                                        onClick: function() {
                                            this._sendCloseCommand();
                                            this.close();
                                        }.bind(this)
                                    }
                                ]
                            },
                            {
                                type: this._scrolling ? 'scrollablePanel' : 'panel',
                                id: 'popupContainer',
                                cssClass: 'popupContainer',
                                width: isAutoSizeWidth ? '?' : '*',
                                height: isAutoSizeHeight ? '?' : '*',
                                maxHeight: options.maxHeight ? options.maxHeight : null,
                                controls: oldControls
                            }
                        ]
                    }
                ]
            }
        ];
        
        if (isAutoSizeHeight) {
            delete options.maxHeight;
        }

        Phoenix.UI.Popup.callBase(this, "initFromOptions", [ options ]);
        this._opened = false;
        this._uri = options.uri;
        this._buttons = options.buttons || [];
        this.container = this['innerBox']['innerPanel']['popupContainer'];

        this._initButtons();
        
        for (var i = 0, len = this.container.controls.length; i < len; i++) {
            this.container.controls[i].popup = this;
        }
    },

    // #region Draggable

    _attachDragHandlers: function(domElement) {
        var thisObj = this;

        thisObj.__startDrag = function(e) {
            thisObj.__dragged = true;
            thisObj.__dragStartX = e.clientX;
            thisObj.__dragStartY = e.clientY;
            thisObj.__originalPos = thisObj.get_position();
            $(document.body).mousemove(thisObj.__processDrag);
            $(document.body).mouseup(thisObj.__stopDrag);
            e.preventDefault();
            e.stopPropagation();
        };

        thisObj.__stopDrag = function(e) {
            if (!thisObj.__dragged) {
                return;
            }

            delete thisObj.__dragStartX;
            delete thisObj.__dragStartY;
            delete thisObj.__dragged;
            delete thisObj.__originalPos;

            $(document.body).unbind('mousemove', thisObj.__processDrag);
            $(document.body).unbind('mouseup', thisObj.__stopDrag);
        }

        thisObj.__processDrag = function(e) {
            var pos = thisObj.__originalPos;
            thisObj.set_position(
                pos.x + e.clientX - thisObj.__dragStartX,
                pos.y + e.clientY - thisObj.__dragStartY
            );
            e.preventDefault();
            e.stopPropagation();
        };

        $(domElement).mousedown(thisObj.__startDrag);
    },

    _detachDragHandlers: function(domElement) {
        $(domElement)
            .unbind('mousedown', this.__startDrag)
            .unbind('mouseout', this.__stopDrag)
            .unbind('mouseup', this.__stopDrag);
        
        delete this.__processDrag;
        delete this.__stopDrag;
        delete this.__startDrag;
    },

    // #endregion
    
    set_buttons: function(value) {
        if (this._buttons === value) {
            return;
        }
        
        this._buttons = value;
        
        if (this.domElement) {
            var buttonsContainer = this['innerBox']['innerPanel']['buttonsContainer'];
            
            if (buttonsContainer) {
                buttonsContainer.free();
                this['innerBox']['innerPanel'].controls.remove(buttonsContainer);
            }
            
            this._initButtons();
        }
    },
    
    _initButtons: function() {
        if (this._buttons.length == 0) {
            return;
        }
        
        var options = {
            id: 'buttonsContainer',
            height: '40px',
            cssClass: 'popup_buttons',
            padding: '7 6 7 0',
            margin: '0 1 0 0',
            border: '0 1 0 0',
            controls: []
        };
        
        for (var i = this._buttons.length - 1; i >= 0; i--) {
            var popup = this;
            var button = this._buttons[i];
            
            var buttonControl = {
                type: 'button',
                onClick: function() {
                    popup.raise_onCommand({ button: this.get_text() }); // raise command with text from button
                }
            };
            
            if (button.text) {
                buttonControl.text = button.text;
            }
            
            if (button.cssClass) {
                buttonControl.cssClass = button.cssClass;
            }
            
            if (isString(button)) {
                buttonControl.text = button;
            }
            
            options.controls.add(buttonControl);
        }
        
        var buttonsContainer = new Phoenix.UI.Panel();
        buttonsContainer.initFromOptions(options);
        
        this['innerBox']['innerPanel'].controls.add(buttonsContainer);
    },
    
    instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div');
        this.domElement.style.cssText = "position: absolute; left: -3000px; top: -3000px; z-index: 10000; display: none";
        document.body.appendChild(this.domElement);
        
        Phoenix.UI.Popup.callBase(this, "instantiateInDom", [ domElement ]);
    },
    
    get_opened: function() {
        return this._opened;
    },
    
    get_parentClientWidth: function() {
        return Application.get_clientWidth();
    },
    
    get_parentClientHeight: function() {
        return Application.get_clientHeight();
    },

    get_parentInnerWidth: function() {
        return Application.get_clientWidth();
    },

    get_parentInnerHeight: function() {
        return Application.get_clientHeight();
    },

    $get_page: function() {
        return this.page;
    },
    
    open: function() {
        if (this._opened) {
            return;
        }
        
        this.staticVars.level++;
        this.__level = this.staticVars.level;
        
        if (this._scrolling) {
            this.container.scrollToTop();
        }
        
        Application.add_onKeyDown(this._appKeyDown, this);
            
        if (this._uri) {
            if (this.page) {
                this.container.controls.remove(this.page);
                this.page.free();
            }
            
            var page = new Page();
            this.page = page;
            page.popup = this;
            
            var onInitComplete = function() {
                this.container.controls.add(page);
                this.raise_onUriLoaded(page);
                page.remove_initComplete(onInitComplete, this);
            };
                        
            page.add_initComplete(onInitComplete, this);
            page.initFromUri(this._uri);
        }
                        
        this._opened = true;
        this.show();

        var lightBoxZIndex = DepthManager.getNewZIndex();

        $(this.domElement)
            .css('z-index', DepthManager.getNewZIndex())
            .stop(true)
            .fadeIn(this._fadeSpeed);

        this._lightBox = DOM.create("div");
        this._lightBox.className = "popup_lightBox";
        this._lightBox.style.zIndex = lightBoxZIndex;

        document.body.appendChild(this._lightBox);

        this.raise_onOpen();
        this.focus();
    },
    
    _appKeyDown: function(sender, args) {
        if (args.keyCode == Keys.Esc && this.__level === this.staticVars.level) {
            this._sendCloseCommand();
            this.close();
        }
    },
    
    _sendCloseCommand: function() {
        this.raise_onCommand({ button: 'Close' });
    },
    
    close: function() {
        if (!this._opened) {
            return;
        }

        DOM.remove(this._lightBox);
        this._lightBox = null;
        
        Application.remove_onKeyDown(this._appKeyDown, this);        
        this.raise_onClose();
        this._opened = false;
        this.__level = undefined;
        
        $(this.domElement).stop(true).fadeOut(this._fadeSpeed, function() {
            this.hide();
            
            if (this.page) {
                this.page.popup = null;
                this.container.controls.remove(this.page);
                this.page.free();
                this.page = null;
            }
        }.bind(this));
        
        this.staticVars.level--;
    },

    set_position: function(x, y) {
        var style = this.domElement.style;
        style.left = x + 'px';
        style.top = y + 'px';
        this._position = { x: x, y: y };
    },

    get_position: function() {
        return this._position;
    },
    
    updateDom: function() {
        if (!this._opened) {
            return;
        }
        
        var element = this.domElement;
        var elStyle = element.style;
            
        var clientWidth = this.get_clientWidth();
        var clientHeight = this.get_clientHeight();
        
        this.set_position(
            Math.round((Application.get_clientWidth() - clientWidth) / 2),
            Math.round((Application.get_clientHeight() - clientHeight) / 2)
        );
        elStyle.width = clientWidth ? clientWidth + 'px' : '';
        elStyle.height = clientHeight ? clientHeight + 'px' : '';
        
        Phoenix.UI.Popup.callBase(this, "updateDom");
    },

    free: function() {
        this.close();
        document.body.removeChild(this.domElement);
        Phoenix.UI.FancyBox.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.Popup.prototype, [
    { name: 'title', autoEvent: true },
    { name: 'uri' }
]);

Auto.Events(Phoenix.UI.Popup.prototype, [
    'onUriLoaded',
    'onCommand',
    'onOpen',
    'onClose'
]);

Phoenix.UI.Popup.createClass('Phoenix.UI.Popup', Control);
ControlsFactory.registerControl('popup', Phoenix.UI.Popup);
