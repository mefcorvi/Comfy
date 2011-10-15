TextMeasurer = {
    init: function(onInit) {
        var browser = $.browser;
        if (/*true || */!Silverlight.isInstalled() || browser.mozilla || (browser.msie && browser.versionNumber >= 9)) {
            onInit();
            return;
        }
        
        var source = Application.resolveUrl('silverlight:Phoenix.TextMeasureService.xap');
        TextMeasurer.__onInit = onInit;

        TextMeasurer.domElement = DOM.create("div", document.body);
        TextMeasurer.domElement.style.cssText = 'position: absolute; left: -1000px; top: -1000px; z-index: 5000; width: 1px; height: 1px;';

        Silverlight.createObject(
            source,
            TextMeasurer.domElement,
            null,
            { version: "4.0.50401.0",
              width: '100%',
              height: '100%',
              isWindowless: 'true',
              background: '#00FFFFFF'
            },
            {
                onError: TextMeasurer.__onError
            }, this._initialParams
        );

        TextMeasurer.__slObj = TextMeasurer.domElement.firstChild;
    },

    __onError: function() {
        throw new Error("TextMeasurer unhandled error");
    },

    __onLoad: function(service) {
        TextMeasurer.__service = service;

        Application.getTextWidth = function(text, font, size, lineHeight, widthLimit) {
            if (size == 0 || !font) {
                return 0;
            }

            var result = TextMeasurer.__service.GetTextWidth(text, font, size, lineHeight || 0, widthLimit || 0);
            return result;
        };
                    
        Application.getTextHeight = function(text, font, size, lineHeight, widthLimit) {
            if (size == 0 || !font) {
                return 0;
            }

            var result = TextMeasurer.__service.GetTextHeight(text, font, size, lineHeight || 0, widthLimit || 0);
            return result;
        };

        TextMeasurer.__onInit();
    }
};