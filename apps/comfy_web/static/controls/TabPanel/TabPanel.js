Type.createNamespace('Phoenix.UI');

Phoenix.UI.TabPanel = function() {
    Phoenix.UI.TabPanel.constructBase(this);
};

Phoenix.UI.TabPanel.prototype = {
    _activeTab: null,   
    _headerDomElement: null,

    defaultOptions: {
        'border': '0',
        'height': '*',
        'width': '*'
    },
    
    get_innerHeight: function(noCache) {
        return Math.max(Phoenix.UI.TabPanel.callBase(this, "get_innerHeight", [ noCache ]) - (this.__headerHeight || 26) || 0, 0);
    },

    get_activeTab: function() { return this._activeTab; },
    set_activeTab: function(value) {
        if (this._activeTab) {
            this._activeTab.hide();
        }
        
        this._activeTab = value;
        this._activeTab.show();
    },
    
    initFromOptions: function(options) {
        if (options.controls) {
            throw new Error('You can\'t add controls to a tab panel. Use the \'tabs\' property instead of.');
        }
        
        if (options.tabs) {
            options.controls = [];
            
            for (var i = 0; i < options.tabs.length; i++) {
                var tabOptions = options.tabs[i];
                tabOptions.type = '_tab';
                tabOptions.width = '100%';
                tabOptions.height = '100%';
                tabOptions.cssClass = 'tab_page';
                options.controls.add(tabOptions);
            }
        }
        
        this.addCssClass('tabs_panel');
    
        Phoenix.UI.TabPanel.callBase(this, "initFromOptions", [ options ]);
    },
    
	instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div', domElement);
        this.instantiateHeader(this.domElement);
        
        Phoenix.UI.TabPanel.callBase(this, "instantiateInDom", [ domElement ]);
        
        if (this.controls.length > 0) {
            for (var i = 0, len = this.controls.length; i < len; i++) {
                this.controls[i].hide();
            }
            this.set_activeTab(this.controls[0]);
        }
        
        this.__headerHeight = this._headerDomElement.offsetHeight;
	},
	
	instantiateHeader: function(domElement) {
	    var headerUl = DOM.create('ul');
	    this._headerDomElement = headerUl;
	    headerUl.className = 'tab_panel_header';
	    
	    for (var i = 0, len = this.controls.length; i < len; i++) {
	        var control = this.controls[i];
	        var tabLi = DOM.create('li');
	        
	        var tabLink = DOM.create('a');
	        var tabText = document.createTextNode(control.get_title());
	        
	        $(tabLi).hover(this.tabHeaderMouseOver.bind(this), this.tabHeaderMouseOut.bind(this))
	        
	        tabLink.href = 'javascript:void(0);';
	        tabLink.__tab = control;
	        control._headerDomItem = tabLi;
	        $(tabLink).click(this.tabHeaderItemClick.bind(this));

	        tabLink.appendChild(tabText);
	        tabLi.appendChild(tabLink);
	        headerUl.appendChild(tabLi);
	    }
	    
	    domElement.appendChild(headerUl);
	},
	
	updateDom: function() {
	    if (this.controls.length > 0) {
	        var headerUl = this._headerDomElement;
	        var innerWidth = this.get_innerWidth();
	            
	        var paddings = DOM.getPaddingWidth(headerUl.childNodes[0]);
        	    
    	    var charsCount = 0;
        	    
    	    for (var i = 0, len = this.controls.length; i < len; i++) {
	            charsCount += this.controls[i].get_title().length;
    	    };
    	        
	        for (var i = 0, len = this.controls.length; i < len; i++) {
	            var control = this.controls[i];
	            var li = headerUl.childNodes[i];
                var titleLength = control.get_title().length;
	            var width = Math.floor(titleLength / charsCount * innerWidth) - paddings;

                if (width > titleLength * 10) {
                    width = titleLength * 10;
                }
	                
	            li.style.width = width + 'px';
	        }
	    }
        
        Phoenix.UI.TabPanel.callBase(this, "updateDom");
	},
	
	tabHeaderMouseOver: function(ev) {
	    var $target = $(ev.currentTarget);
	    $target.addClass('hovered');
	},
	
	tabHeaderMouseOut: function(ev) {
	    var $target = $(ev.currentTarget);
	    $target.removeClass('hovered');
	},
	
	tabHeaderItemClick: function(ev) {
	    this.set_activeTab(ev.currentTarget.__tab);
	},
	
	free: function() {
	    DOM.discardElement(this._headerDomElement);
	    this._activeTab = null;
	    Phoenix.UI.TabPanel.callBase(this, "free");
	}
};

Phoenix.UI.Tab = function() {
    Phoenix.UI.Tab.constructBase(this);
};

Phoenix.UI.Tab.prototype = {
    tabPanel: null,
    _headerDomItem: null,

    defaultOptions: {
        'border': '1',
        'height': '100%',
        'width': '100%'
    },
    
    get_isActive: function() {
        return this.tabPanel._activeTab === this;
    },
    
    set_isActive: function() {
        this.tabPanel.setActiveTab(this);    
    },
    
    show: function() {
        this.domElement.style.display = '';
        
        if (this._headerDomItem) {
            $(this._headerDomItem).addClass('active');
        }
        
        this.update(this);
        this.raise_onActivate();
    },
    
    hide: function() {
        this.domElement.style.display = 'none';
        
        if (this._headerDomItem) {
            $(this._headerDomItem).removeClass('active');
        }
    },

    initFromOptions: function(options) {
        if (isFunction(options.onActivate)) {
            this.add_onActivate(options.onActivate, this);
        }

        Phoenix.UI.TabPanel.callBase(this, "initFromOptions", [ options ]);
        this._title = options.title;
    },
    
	instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div');
        this.className = 'tab';
        domElement.appendChild(this.domElement);
        
        Phoenix.UI.TabPanel.callBase(this, "instantiateInDom", [ domElement ]);
	}
};

Auto.Events(Phoenix.UI.Tab.prototype, [
    'onActivate'
]);

Auto.Properties(Phoenix.UI.Tab.prototype, [
    { name: 'title', autoEvent: true }
]);

Phoenix.UI.TabPanel.createClass('Phoenix.UI.TabPanel', Control);
Phoenix.UI.Tab.createClass('Phoenix.UI.Tab', Control);
ControlsFactory.registerControl('tabPanel', Phoenix.UI.TabPanel);
ControlsFactory.registerControl('_tab', Phoenix.UI.Tab);