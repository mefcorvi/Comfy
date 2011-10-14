Type.createNamespace('Phoenix.UI');

Phoenix.UI.Image = function() {
    Phoenix.UI.Image.constructBase(this);
};

Phoenix.UI.Image.prototype = {
	_src: null,

    defaultOptions: {
        bindings: {
            '*': 'src'
        }
    },
	
	get_src: function() { return this._src; },
	set_src: function(value)
	{
	    if (this._src === value) {
	        return;
	    }
	
	    this._src = Application.resolveUrl(value);
	    
	    if (this.domElement) {
	        this._loadImg(this._src);
	    }
	},
	
	refresh: function()
	{
	    var separator = this._src.indexOf('?') >= 0 ? '&' : '?';
	    
	    if (this.domElement) {
	        this._loadImg(this._src + separator + Math.random());
	    }
	},
	
    initFromOptions: function(options) {
        Phoenix.UI.Image.callBase(this, "initFromOptions", [ options ]);
        
        this._src = Application.resolveUrl(options.src);
    },
    
	instantiateInDom: function(domElement) {
        this.domElement = DOM.create("img", domElement);
        this._loadImg(this.get_src());
        
        Phoenix.UI.Image.callBase(this, "instantiateInDom", [ domElement ]);
	},
	
	_loadImg: function(src) {
	    if (!this.domElement) {
	        return;
	    }
	    
	    var element = this.domElement;
        var $element = $(element);
        $element.removeClass("loading_error");
		
		$element.css({
		    width: this.get_innerWidth(),
		    height: this.get_innerHeight()
		});

		var img = DOM.create('img');
		
		$(img).css({
		    position: "absolute",
		    left: -1000,
		    top: -1000,
		    width: 1,
		    height: 1
		})
		.bind("load", function()
		{
            $element.removeClass("loading");
            element.src = this.src;
	        $(this).unbind("load");
            
            DOM.remove(this);
		})
		.bind("error", function() { $element.addClass("loading_error"); DOM.remove(this); });
        
        element.src = Application.get_configuration().get_blankImageUrl();
        $element.addClass("loading");
        
        document.body.appendChild(img);
		img.src = src;	    
	}
};

Phoenix.UI.Image.createClass('Phoenix.UI.Image', Control);
ControlsFactory.registerControl('image', Phoenix.UI.Image);
