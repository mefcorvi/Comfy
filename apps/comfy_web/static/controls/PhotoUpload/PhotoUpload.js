Type.createNamespace('Phoenix.UI');

Phoenix.UI.PhotoUpload = function() {
    Phoenix.UI.PhotoUpload.constructBase(this);
};

Phoenix.UI.PhotoUpload.prototype = {        
    initFromOptions: function(options) {

        var thisObj = this;

        if (isFunction(options.onProgress)) {
            this.add_onProgress(options.onProgress, this);
        }
    
        if (isFunction(options.onComplete)) {
            this.add_onComplete(options.onComplete, this);
        }

        if (isFunction(options.onShotClicked)) {
            this.add_onShotClicked(options.onShotClicked, this);
        }

        if (isFunction(options.onFatalError)) {
            this.add_onFatalError(options.onFatalError, this);
        }
        
        options.controls = [
            {
                type: 'panel',
                id: 'panel',
                width: '320',
                layout: 'stack',
                orientation: 'vertical',
                halign: 'center',
                controls: [
                    {
                        id: 'photoSilverlight',
                        type: 'silverlight',
                        url: 'silverlight:Phoenix.PhotoUpload.xap',
                        width: '320',
                        height: '240',
                        isWindowless: ($.browser.firefox ? 'false' : 'true'),
                        onSlLoad: function (sender, args) {
                            var content = this.get_content();

                            content.Service.OnComplete = function (sender, args) {
                                this.get_content().Content.DeInit();
                                this.parent.parent.raise_onComplete({ id : args.ImageId });
                            }.bind(this);

                            content.Service.OnProgress = function (sender, args) {                                
                                this.parent.parent.raise_onProgress({ progress: args.Progress });
                            }.bind(this);

                            content.Content.ReadyToShot = function () {
                                this.parent.shotButton.set_enabled(true);
                            }.bind(this);

                            content.Content.CameraNotFound = function () {                                
                                this.parent.parent.raise_onFatalError({ caption: 'Camera not found!', text: 'Please connect your camera' });
                            }.bind(this);
                                                                              
                            content.Content.Init();
                        }
                    },
                    {
                        type: 'button',
                        id: 'shotButton',
                        text: 'Shot',
                        margin: '10 5 0 0',
                        enabled: false,
                        onClick: function (sender, args) {      
                            this.parent.parent.set_shotWasClicked(true);                      
                            this.parent.parent.raise_onShotClicked();
                            this.parent.photoSilverlight.get_content().Content.Shot();                            
                        }
                    }
                ]
            }            
        ];
                
        Phoenix.UI.PhotoUpload.callBase(this, "initFromOptions", [ options ]);        
    },

    init: function() {
        this.set_shotWasClicked(false);
        this.panel.shotButton.set_enabled(false);
        
        if ($.browser.msie) {
            this.panel.photoSilverlight.reload();
        }
    },

    deInit: function() {        
        if (!this.get_shotWasClicked())
        {
            this.panel.photoSilverlight.get_content().Content.DeInit();
        }
    }
};

Auto.Properties(Phoenix.UI.PhotoUpload.prototype, [
    { name: 'shotWasClicked' }
]);

Auto.Events(Phoenix.UI.Panel.prototype, [
    'onProgress',
    'onComplete',
    'onShotClicked',
    'onFatalError'    
]);

Phoenix.UI.PhotoUpload.createClass('Phoenix.UI.PhotoUpload', Phoenix.UI.Panel);
ControlsFactory.registerControl('photoUpload', Phoenix.UI.PhotoUpload);
