Type.createNamespace('Phoenix.UI');

Phoenix.UI.FileUpload = function() {
    Phoenix.UI.FileUpload.constructBase(this);
    this._silverlightId = "FileUpload_" + Phoenix.UI.FileUpload.prototype._silverlightNextId;
    Phoenix.UI.FileUpload.prototype._silverlightNextId++;
};

Phoenix.UI.FileUpload.prototype = {
    _imageType: null,
    _silverlightId: null,
    _silverlightNextId: 1,

    defaultOptions: {
        height: '33px'
    },

    initFromOptions: function (options) {
        if (isFunction(options.onProgress)) {
            this.add_onProgress(options.onProgress, this);
        }

        if (isFunction(options.onComplete)) {
            this.add_onComplete(options.onComplete, this);
        }

        _imageType = options.imageType ? ImageType[options.imageType] : ImageType.user;

        Phoenix.UI.FileUpload.callBase(this, "initFromOptions", [options]);
    },

    _onSlLoad: function (service) {
        var oldEnabled = true;
        delete Silverlight[this._silverlightId + "Load"];

        service.ImageType = _imageType._value;

        service.OnProgress = function (sender, result) {
            var progress = result.Progress;

            if (progress == 0) {
                oldEnabled = this.get_enabled();
                this.set_enabled(false);

                var style = this._slContainer.style;
                style.position = "absolute";
                style.left = "-1000px";
                style.top = "-1000px";
            }

            this.set_text('Uploading... ' + progress + '%');
            this.raise_onProgress({ progress: progress });
        } .bind(this);

        service.OnComplete = function (sender, result) {
            this.update();
            this.set_text(this.options.text || '');
            this.set_enabled(oldEnabled);

            this.raise_onComplete({ id: result.ImageId });
        } .bind(this);
    },

    instantiateInDom: function (domElement) {
        Phoenix.UI.FileUpload.callBase(this, "instantiateInDom", [domElement]);

        this._slContainer = DOM.create("div", domElement);
        var style = this._slContainer.style;
        style.position = "absolute";
        style.overflow = "auto";
        //style.left = "-1000px";
        //style.top = "-1000px";
        //style.width = 100;
        //style.height = 100;
        //$(this._slContainer).css("color", "#FF0000");
        //$(this._slContainer).html("!test!");

        Silverlight[this._silverlightId + "_Load"] = this._onSlLoad.bind(this);

        Silverlight.createObject(
        	Application.resolveUrl('silverlight:Phoenix.FileUpload.xap'),
        	this._slContainer,
        	null,
        	{
        	    version: "4.0.50401.0",
        	    isWindowless: 'true',
        	    background: '#00FFFFFF'
        	},
            {
                onError: function () { }
            },
            "id=" + this._silverlightId
        );

        this._silverlightUploader = this._slContainer.firstChild;
    },

    clearUpdateStyleTimer: function() {
        if (this._updateStyleTimer) {
            clearTimeout(this._updateStyleTimer);
            this._updateStyleTimer = null;
        }
    },

    createUpdateStyleTimer: function() {
        this.clearUpdateStyleTimer();
        this._updateStyleTimer = setTimeout(this._updateStyle.bind(this), 100);
    },

    updateDom: function (sender) {
        var element = this.domElement;
        var offset = $(element).position();
        var style = this._slContainer.style;
        style.zIndex = 900000;
        style.left = offset.left + "px";
        style.top = offset.top + "px";

        this.createUpdateStyleTimer();

        var uploaderStyle = this._silverlightUploader.style;
        uploaderStyle.width = (element.offsetWidth + 1) + "px";
        uploaderStyle.height = element.offsetHeight + "px";
        uploaderStyle.overflow = "auto";

        Phoenix.UI.FileUpload.callBase(this, "updateDom", [sender]);
    },

    _updateStyle: function () {
        var element = this.domElement;
        var offset = $(element).position();
        var style = this._slContainer.style;
        style.zIndex = 900000;
        style.left = offset.left + "px";
        style.top = offset.top + "px";
        
        var uploaderStyle = this._silverlightUploader.style;
        uploaderStyle.width = (element.offsetWidth + 1) + "px";
        uploaderStyle.height = element.offsetHeight + "px";
        uploaderStyle.overflow = "auto";
    },

    free: function () {
        DOM.remove(this._silverlightUploader);
        this._silverlightUploader = null;

        DOM.remove(this._slContainer);
        this._slContainer = null;
        this.clearUpdateStyleTimer();

        Phoenix.UI.FileUpload.callBase(this, "free");
    }
};

Auto.Events(Phoenix.UI.Button.prototype, [
    'onProgress',
    'onComplete'
]);

Phoenix.UI.FileUpload.createClass('Phoenix.UI.FileUpload', Phoenix.UI.Button);
ControlsFactory.registerControl('fileUpload', Phoenix.UI.FileUpload);
