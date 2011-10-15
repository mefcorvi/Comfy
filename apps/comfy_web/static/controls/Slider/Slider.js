Type.createNamespace('Phoenix.UI');

Phoenix.UI.Slider = function() {
    Phoenix.UI.Slider.constructBase(this);
    this._sliders = new Hashtable();
};

Phoenix.UI.Slider.prototype = {
    _domList: null, // ul
    _sliders: null,

    defaultOptions: {
        width: '*',
        height: '61',
        padding: '0 0 18 0'
    },

    initFromOptions: function(options) {
        this._selectedValue = options.selectedValue;
        this._textProperty = options.textProperty;
        this._textPropertyFormatter = options.textPropertyFormatter;
        this._title = options.title || "";
        
        if (options.onChanged && isFunction(options.onChanged)) {
            this.add_selectedValueChanged(function(sender, args) {
                options.onChanged.bind(this)(args);
            }, this)
        }
        
        options.cssClass = options.cssClass || 'slider';
        
        if (!this._textProperty) {
            throw new Error('[Slider]: attribute "textProperty" cannot be null.');
        }

        Phoenix.UI.Slider.callBase(this, "initFromOptions", [ options ]);
    },
    
    set_dataSource: function(value) {
        if (this._dataSource === value) {
            return;
        }

        if (this._dataSource) {
            this._detachHandlers();
            this._clearItems();
        }
        
        this._selectedValue = this.options.selectedValue;
        this._dataSource = value;
        this._attachHandlers();
        
        this.initDomFromDataSource();
    },
    
    _attachHandlers: function() {
        if (this._dataSource && this._dataSource.__observable) {
            //this._dataSource.add_added(this._dataSource_itemAdded, this);
            //this._dataSource.add_removed(this._dataSource_itemRemoved, this);
        }
    },

    _detachHandlers: function() {
        if (this._dataSource && this._dataSource.__observable) {
            //this._dataSource.remove_added(this._dataSource_itemAdded, this);
            //this._dataSource.remove_removed(this._dataSource_itemRemoved, this);
        }
    },
    
    initDomFromDataSource: function() {
        if (this._domList && this._dataSource) {
            this._domList.innerHTML = '';
        
            var textGetter = 'get_' + this._textProperty;
            
            for (var i = 0; i < this._dataSource.length; i++) {
                var dataItem = this._dataSource[i];
                var value, text;
            
                if (dataItem[textGetter]) {
                    text = dataItem[textGetter]();
                } else {
                    text = dataItem[this._textProperty];
                }
                
                value = dataItem;
                
                if (!this._selectedValue) {
                    this._selectedValue = value;
                }
                
                this.createItemInDom(value, text);
            }
        }
    },
    
	instantiateInDom: function(domElement) {
	    this.domElement = DOM.create('div', domElement);
	    
	    this.createHeaderInDom();
	    this._domList = DOM.create('ul', this.domElement);
	    
	    this.initDomFromDataSource();
	
        Phoenix.UI.Slider.callBase(this, "instantiateInDom", [ domElement ]);
	},
	
	createHeaderInDom: function() {
	    var domHeader = DOM.create('h3', this.domElement);
	    var domHeaderSpan = DOM.create('span', domHeader);
	    domHeaderSpan.appendChild(document.createTextNode(this._title));
	},
	
	createItemInDom: function(value, text) {
	    if (this._textPropertyFormatter) {
	        text = this._textPropertyFormatter(text);
	    }
	
	    var domItem = DOM.create('li');
	    domItem.appendChild(document.createTextNode(text));
	    domItem.__sliderValue = value;
	    this._sliders.put(value, domItem);
	    
	    if (this._selectedValue == value) {
	        domItem.className = 'selected';
	    }
	    
	    var $domItem = $(domItem);
	    
	    $domItem
	        .click(function() { this._itemClicked(domItem); }.bind(this))
	        .hover(
	            function() { $(this).addClass('hovered'); },
	            function() { $(this).removeClass('hovered'); }
    	    );
    	
	    this._domList.appendChild(domItem);
	    
	    var itemsCount = this._domList.childNodes.length;
	    
	    var firstWidth, itemWidth, lastWidth;
	    
	    /*if (itemsCount > 2) {
	        firstWidth = Math.floor(50 / itemsCount);
	        itemWidth = Math.floor((100 - 100 / itemsCount) / (itemsCount - 2));
	        lastWidth = 100 - (firstWidth + itemWidth * (itemsCount - 2))
	    } else {
	        firstWidth = itemsCount == 2 ? 50 : 100;
	        lastWidth = 100 - firstWidth;
	    }*/
	    
	    if (itemsCount > 2) {
	        firstWidth = Math.round(50 / (itemsCount - 1));
	        itemWidth = Math.round(100 / (itemsCount - 1));
	        lastWidth = ($.browser.msie ? 99 : 100) - firstWidth - itemWidth * (itemsCount - 2);
	    } else {
	        firstWidth = itemsCount == 2 ? 50 : 100;
	        lastWidth = ($.browser.msie ? 99 : 100) - firstWidth;
	    }
	    
    	// calculate widths
    	for (var i = 0; i < itemsCount; i++) {
    	    var node = this._domList.childNodes.item(i);
    	    
    	    if (i == 0) {
    	        $(node).addClass('first');
    	        node.style.width = firstWidth + '%';
    	    } else if (i == itemsCount - 1) {
    	        $(node).addClass('last');    	    
    	        node.style.width = lastWidth + '%';
    	    } else {
    	        $(node).removeClass('last');    	    
    	        node.style.width = itemWidth + '%';
    	    }
    	}
	},
	
	set_selectedValue: function(value) {
	    if (this._selectedValue === value) {
	        return;
	    }
	    
	    this.raise_selectedValueChanged({ oldValue: this._selectedValue, newValue: value });
	    
	    this._selectedValue = value;
	    
	    $('li', this._domList).removeClass('selected');
	    $(this._sliders.get(value)).addClass('selected');
	},
	
	_clearItems: function() {
	    for (var key in this._sliders.keys()) {
            DOM.remove(this._sliders.get(key));
	    }
	    
	    this._sliders.clear();
	},
	
	_itemClicked: function(sender) {
	    this.set_selectedValue(sender.__sliderValue);
	},
	
	free: function() {
	    this._clearItems();
	    Phoenix.UI.Slider.callBase(this, "free");
	}
};

Auto.Properties(Phoenix.UI.Slider.prototype, [
    { name: 'selectedValue', autoEvent: true },
    { name: 'textProperty' },
    { name: 'title' }
]);


Phoenix.UI.Slider.createClass('Phoenix.UI.Slider', Control);
ControlsFactory.registerControl('slider', Phoenix.UI.Slider);
