Type.createNamespace('Phoenix.UI');

Phoenix.UI.FancyBox = function() {
    Phoenix.UI.FancyBox.constructBase(this);
};

Phoenix.UI.FancyBox.prototype = {
    _contentRounded: null,
    _contentContainer: null,
    _innerWidthDelta: 21,
    _innerHeightDelta: 22,

    get_innerWidth: function() {
        return Math.max(Phoenix.UI.FancyBox.callBase(this, "get_innerWidth") - this._innerWidthDelta, 0);
    },

    get_innerHeight: function() {
        return Math.max(Phoenix.UI.FancyBox.callBase(this, "get_innerHeight") - this._innerHeightDelta, 0);
    },

    _set_clientHeight: function(value) {
        if (this.get_height().isAutoSize()) {
            value += this._innerHeightDelta;
        }
        
        Phoenix.UI.FancyBox.callBase(this, "_set_clientHeight", [value]);
    },
    
    _set_clientWidth: function(value) {
        if (this.get_width().isAutoSize()) {
            value += this._innerWidthDelta;
        }
        
        Phoenix.UI.FancyBox.callBase(this, "_set_clientWidth", [value]);
    },

    get_childsContainer: function() {
        return this._contentContainer;
    },

    /**
    * Instantiate this control in DOM
    */
    instantiateInDom: function(domElement) {
        var element = DOM.create('div', domElement);
        var $element = $(element);
        
        var layout = '<div class="top_rounded">' +
                        '<span class="ltc">&nbsp;</span><span class="rtc">&nbsp;</span><div>&nbsp;</div>' +
                     '</div>' +
                     '<div class="content_rounded">' +
                         '<span class="l">&nbsp;</span><span class="r">&nbsp;</span><div class="content">' +
                             '<div class="main_content">' +
                                 '<div class="content_container"></div>' +
                             '</div>' +
                         '</div>' +
                     '</div>' +
                     '<div class="bottom_rounded">' +
                         '<span class="l">&nbsp;</span><span class="r">&nbsp;</span><div>&nbsp;</div>' +
                     '</div>';

        $element.html(layout);

        this.domElement = element;
        this.addCssClass('rounded_box');
        
        this._topRounded = $('.top_rounded', $element).get(0);
        this._contentRounded = $('.content_rounded', $element).get(0);
        this._contentContainer = $('.content_container', $element).get(0);

        Phoenix.UI.FancyBox.callBase(this, "instantiateInDom", [ domElement ]);
    },
    
    updateDom: function(sender) {
        var element = this.domElement;
            
        var clientWidth = this.get_clientWidth();
        var clientHeight = this.get_clientHeight();
        var innerWidth = this.get_innerWidth();
        var innerHeight = this.get_innerHeight();
            
        var elStyle = element.style;
            
        elStyle.width = clientWidth ? clientWidth + 'px' : '';
        elStyle.height = clientHeight ? clientHeight + 'px' : '';
            
        this._contentRounded.style.height = innerHeight ? innerHeight + 'px' : '';
            
        var contentContainerStyle = this._contentContainer.style;
        contentContainerStyle.width = innerWidth ? innerWidth + 'px' : '';
        contentContainerStyle.height = innerHeight ? innerHeight + 'px' : '';
        
        Phoenix.UI.FancyBox.callBase(this, "updateDom", [sender]);
    }
};

Phoenix.UI.FancyBox.createClass('Phoenix.UI.FancyBox', Control);
ControlsFactory.registerControl('box', Phoenix.UI.FancyBox);