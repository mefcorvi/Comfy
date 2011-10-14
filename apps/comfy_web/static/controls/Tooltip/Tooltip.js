Type.createNamespace('Phoenix.UI');

Phoenix.UI.Tooltip = function() {
    Phoenix.UI.Tooltip.constructBase(this);
};

Phoenix.UI.Tooltip.prototype = {
    /**
     * Declaring inner variables
     */
    _position: {x: 0, y: 0},

    defaultOptions: {
        'visible': false,
        'width': '?',
        'height': '?',
        'layout': 'stack',
        'fadeSpeed': 0
    },

    /**
     * Declaring inner controls
     */

    initFromOptions: function(options) {
        options.cssClass = options.cssClass || 'tooltip';
        this._inDocumentFlow = false;
        var tooltipArea =   { type: 'panel', width: '?', height: '?' };
        Object.extend(tooltipArea, options.template);

        options.controls = [
            tooltipArea
        ];
        Phoenix.UI.Tooltip.callBase(this, "initFromOptions", [ options ]);
    },

    /**
     * Manual instantiation
     */
    instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div');
        this.domElement.style.cssText = "position: absolute; left: " + this._position.x + "px; top: " + this._position.y + "px; z-index: 10000; display: none";
        document.body.appendChild(this.domElement);
        Phoenix.UI.Tooltip.callBase(this, "instantiateInDom", [ domElement ]);
    },

    updateDom: function() {
        if (!this._opened) {
            return;
        }
        
        var element = this.domElement;
        var elStyle = element.style;
            
        var clientWidth = this.get_clientWidth();
        var clientHeight = this.get_clientHeight();

        elStyle.width = clientWidth ? clientWidth + 'px' : '';
        elStyle.height = clientHeight ? clientHeight + 'px' : '';
        
        Phoenix.UI.Popup.callBase(this, "updateDom");
    },

    set_position: function(position) {
        this._position = position;
    },

    get_position: function() {
        return this._position;
    },

    open: function() {
        if (this._opened) {
            return;
        }
        
        this._opened = true;
        this.show();
        $(this.domElement)
            .css('z-index', DepthManager.getNewZIndex())
            .stop(true)
            .fadeIn(this.options.fadeSpeed);
    }
};

Phoenix.UI.Tooltip.createClass('Phoenix.UI.Tooltip', Control);
ControlsFactory.registerControl('tooltip', Phoenix.UI.Tooltip);