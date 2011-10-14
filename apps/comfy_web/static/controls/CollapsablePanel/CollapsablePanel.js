Type.createNamespace('Phoenix.UI');

Phoenix.UI.CollapsablePanel = function() {
    Phoenix.UI.CollapsablePanel.constructBase(this);
};

Phoenix.UI.CollapsablePanel.prototype = {
    _title: null,
    _oldHeight: null,
    _isCollapsed: null,
    _showCollapseButton: null,
    _showStatusBar: null,
    _cookieEnabled: null,
    _cookieKey: null,
    _scrolling: null,
    
    set_title: function(value) {
        if (this._title === value) {
            return;
        }
    
        this.header.lblTitle.set_text(value);
        this._title = value;
    },
    
    set_statusText: function(value) {
        if (this._statusText === value) {
            return;
        }
        
        if (this.statusBar) {
            this.statusBar.lblStatus.set_dataSource(value);
        }

        this._statusText = value;
    },

    showStatusBar: function() {
        if (this.statusBar) {
            this.statusBar.show();
        }
    },
    
    hideStatusBar: function() {
        if (this.statusBar) {
            this.statusBar.hide();
        }
    },

    initFromOptions: function(options) {
        this._showCollapseButton = isNullOrUndefined(options.showCollapseButton) ? true : options.showCollapseButton;
        this._headerAsCollapseButton = isNullOrUndefined(options.headerAsCollapseButton) ? true : options.headerAsCollapseButton;
        
        if (isFunction(options.onCollapsed)) {
            this.add_onCollapsed(options.onCollapsed, this);
        }
        
        if (isFunction(options.onExpanded)) {
            this.add_onExpanded(options.onExpanded, this);
        }
        
        if (isFunction(options.onCommand)) {
            this.add_onCommand(options.onCommand, this);
        }
        
        options.minHeight = options.minHeight || 28;
        this._scrolling = options.scrolling;
        
        this.addCssClass('collapsable_panel');
        this._cookieKey = options.cookieKey;
        this._cookieEnabled = !!this._cookieKey;
        this._statusText = options.statusText;
        
        var panel = {};

        // TODO: !!! Bad smell code
        panel.type = this._scrolling ? 'scrollablePanel' : 'panel';
        panel.cssClass = "section " + (options.innerPanelCssClass || '');
        panel.id = 'innerPanel';
        panel.border = '1 0 1 1';
        panel.width = '100%';
        panel.height = options.height == '?' ? '?' : '*';
        panel.layout = options.layout;
        panel.orientation = options.orientation;
        panel.controls = options.controls;
        panel.valign = options.valign;
        panel.halign = options.halign;
        panel.padding = options.padding;
        panel.columns = options.columns;
        panel.rows = options.rows;
        panel.vertical = options.vertical;
        panel.horizontal = options.horizontal;
        panel.margin = '0 1 0 0';
        options.padding = undefined;
        
        if (options.bindings) {
            panel.bindings = { '*': '*' };
        }
        
        options.layout = 'stack';
        options.orientation = 'vertical';
        
        options.controls = [
            {
                type: 'panel',
                cssClass: this._showCollapseButton ? 'section_header_expandable section_header' : 'section_header',
                width: '100%',
                height: options.showHeader == false ? '0' : '28',
                padding: '5px 0',
                border: options.showHeader == false ? '1 0 1 1' : '1',
                id: 'header',
                orientation: 'horizontal',
                domHandlers: (this._showCollapseButton && this._headerAsCollapseButton) ? {
                    click: this._collapseClicked.bind(this)
                } : null,
                controls: [
                    {
                        id: 'lblTitle',
                        type: 'label',
                        width: '*',
                        height: '20px',
                        valign: 'middle',
                        text: options.title || ''
                    },
                    {
                        id: 'lnkCollapse',
                        type: 'link',
                        width: '18px',
                        height: '*',
                        focusable: false,
                        cssClass: 'collapse_arrow',
                        text: '',
                        domHandlers: (this._showCollapseButton && !this._headerAsCollapseButton) ? {
                            click: this._collapseClicked.bind(this)
                        } : null
                    }
                ]
            },
            panel
        ];
        
        if (options.showStatusBar) {
            var thisObj = this;

            var statusLabel = {
                type: 'label',
                hideIfEmpty: true,
                width: '?',
                padding: '5 0'
            };

            if (options.statusLabelTemplate) {
                statusLabel = options.statusLabelTemplate;
            }
            
            statusLabel.id = 'lblStatus';
            statusLabel.cssClass = 'status_label';

            var statusBar = {
                id: 'statusBar',
                type: 'panel',
                cssClass: 'section_status',
                height: '25',
                width: '100%',
                border: '1 0 1 1',
                orientation: 'horizontal',
                customFunctions: {
                    'setButtonStatus': function(buttonName, status) {
                        var button = this.buttons.findControl(function(c) { return c.get_text() == buttonName; });
                        if (!button) {
                            throw new Error('button ' + buttonName + ' not found!');
                        }
                        button.set_enabled(status);
                    }
                },
                controls: [
                    statusLabel,
                    {
                        id: 'buttons',
                        type: 'repeater',
                        cssClass: 'status_buttons',
                        width: '?',
                        height: '*',
                        orientation: 'horizontal',
                        template: {
                            type: 'button',
                            padding: '10 0',
                            border: '1',
                            margin: '1',
                            height: '*',
                            cssClass: 'status_button',
                            bindings: {
                                '*': 'text'
                            },
                            onClick: function() {
                                thisObj.raise_onCommand({ button: this.get_text() });
                            }
                        }
                    }
                ],
                onLoad: function() {
                    this.buttons.set_dataSource([].makeObservable());
                    if (this.lblStatus) {
                        this.lblStatus.set_dataSource(thisObj._statusText);
                    }
                    
                    if (options.buttons) {
                        thisObj.set_buttons(options.buttons);
                    }
                }
            };
            
            options.controls.add(statusBar);
        }
        
        Phoenix.UI.CollapsablePanel.callBase(this, "initFromOptions", [ options ]);
    },

    _childsTreeLoaded: function() {
        this.__registerInnerChilds();
        Phoenix.UI.CollapsablePanel.callBase(this, "_childsTreeLoaded");
    },

    __registerInnerChilds: function() {
        var controls = this.innerPanel.controls;

        for (var i = 0; i < controls.length; i++) {
            var child = controls[i];
            this.controls._registerControl(child);
        }
    },
    
    __unregisterInnerChilds: function() {
        var controls = this.innerPanel.controls;

        for (var i = 0; i < controls.length; i++) {
            var child = controls[i];
            this.controls._unregisterControl(child);
        }
    },

    get_buttons: function() {
        return this.statusBar.buttons.get_dataSource();
    },

    setButtonStatus: function(buttonName, status) {
        if (this.statusBar) {
            this.statusBar.setButtonStatus(buttonName, status);
        }
        else {
            throw new Error("Status bar is not enabled! Use 'showStatusBar' option");
        }
    },
    
    set_buttons: function(buttons) {
        var ds = this.statusBar.buttons.get_dataSource();
        ds.clear();
        
        //buttons.reverse();
        ds.add(buttons);
    },
    
	instantiateInDom: function(domElement) {
        Phoenix.UI.CollapsablePanel.callBase(this, "instantiateInDom", [ domElement ]);
	    
        if (this.get_isCollapsed()) {
            this._collapse();
        }
        
        this._initCollapsable();
	},

    free: function() {
        this.__unregisterInnerChilds();
        Phoenix.UI.CollapsablePanel.callBase(this, "free");
    },
	
	_setCollapsedHeight: function() {
        this._oldHeight = this.get_height();
        this._oldMinHeight = this.options.minHeight;
        this.options.minHeight = undefined;
        this.set_height(new DimensionUnit('?'));
	},
	
	_setOriginalHeight: function() {
        this.set_height(this._oldHeight);
        this.options.minHeight = this._oldMinHeight;
	},

    _collapse: function() {
        var collapseLink = this.header.lnkCollapse;
        var $element = $(this.innerPanel.domElement);

        collapseLink.addCssClass("collapse_arrow_toggled");
        collapseLink.removeCssClass("collapse_arrow");
            
        $element.fadeOut(350, function() {
            this._setCollapsedHeight();
            this.innerPanel.hide();
            this.raise_onCollapsed();
            this.update();
        }.bind(this));
    },

    _expand: function() {
        var collapseLink = this.header.lnkCollapse;
        var $element = $(this.innerPanel.domElement);

        collapseLink.removeCssClass("collapse_arrow_toggled");
        collapseLink.addCssClass("collapse_arrow");
            
        if (this._oldHeight) {
            this._setOriginalHeight();
            this.innerPanel.show();
            this.update();
        }
            
        $element.fadeIn(350);
        this.raise_onExpanded();
    },
	
	get_isCollapsed: function() {
	    if (this._isCollapsed === null) {
            this._isCollapsed = this._showCollapseButton && (this._cookieEnabled && $.cookie(this._getCookieKey()) == 1);	        
	    }
	    
	    return this._isCollapsed;
	},
	
	set_isCollapsed: function(value) {
	    if (value == this.get_isCollapsed()) {
	        return;
	    }
	
        if (this.get_isCollapsed()) {
            this._expand();
        }
        else {
            this._collapse();
        }

	    this._isCollapsed = value;
	    
	    if (this._cookieEnabled) {
            $.cookie(this._getCookieKey(), value ? 1 : 0, { expires: 7 });
        }
	},
	
	_getCookieKey: function() {
	    return encodeURIComponent(document.location.href + this._cookieKey);
	},
	
	_initCollapsable: function() {
        var element = this.innerPanel;
        var collapseLink = this.header.lnkCollapse;
        var $element = $(this.innerPanel.domElement);
        
        if (!this._showCollapseButton) {
            collapseLink.hide();
        }

        if (this.get_isCollapsed()) {
            collapseLink.addCssClass("collapse_arrow_toggled");
            collapseLink.removeCssClass("collapse_arrow");
            
            $element.fadeOut(0);
            
            this.raise_onCollapsed();
        }
        else {
            collapseLink.removeCssClass("collapse_arrow_toggled");
            collapseLink.addCssClass("collapse_arrow");
            
            this.raise_onExpanded();
        }
	},
	
	_collapseClicked: function(sender, args) {
        args.stopPropagation();
        args.preventDefault();
        this.set_isCollapsed(!this._isCollapsed);
	}
};

Auto.Events(Phoenix.UI.CollapsablePanel.prototype, [
    'onCollapsed',
    'onExpanded',
    'onCommand'
]);

Auto.Properties(Phoenix.UI.CollapsablePanel.prototype, [
    { name: 'title' },
    { name: 'statusText' }
]);

Phoenix.UI.CollapsablePanel.createClass('Phoenix.UI.CollapsablePanel', Phoenix.UI.Panel);
ControlsFactory.registerControl('collapsablePanel', Phoenix.UI.CollapsablePanel);
