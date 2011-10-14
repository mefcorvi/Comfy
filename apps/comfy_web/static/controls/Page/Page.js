/**
 * Page class
 
 * @inherits Control
 * @constructor
 */
Page = function() {
    Page.constructBase(this);
    this._params = {};
    this._paramsIdx = {}; // indexes of param. It's used for right ordering of parameters. Last changed params should be at the end of uri.
    this._lastParamIdx = 0;
};

Page.prototype = {
    _isLoaded: false,
    _loadingXhr: null,
    _isLoading: false,
    
    // ======== Properties ========
    get_title: function(){
        return this.options.title;
    },
    
    get_isLoaded: function() {
        return this._isLoaded;
    },

    isSameUri: function(uri) {
        if (isNullOrUndefined(uri)) {
            return false;
        }

        if (isNullOrUndefined(this._fullUri)) {
            return this._uri === uri;
        }

        var nav = PageUri.parseUri(this._fullUri);
        var nav2 = PageUri.parseUri(uri);

        return nav.uri == nav2.uri;
    },

    __update_fullUri: function() {
        var oldFullUri = this._fullUri;
        var uri = PageUri.buildUri(this.get_uri(), this._params, function(a, b) {
            return this._paramsIdx[a] - this._paramsIdx[b];
        }.bind(this));

        if (uri === oldFullUri) {
            return;
        }

        this._fullUri = uri;
        this.raise_fullUriChanged({ newValue: uri, oldValue: oldFullUri });
    },

    sendMessage: function(msg) {
        this.raise_onMessage({ message: msg });
    },

    update_fullUri: function() {
        if (this.__updateUriTimeOut) {
            clearTimeout(this.__updateUriTimeOut);
        }

        this.__updateUriTimeOut = setTimeout(function() {
            this.__update_fullUri();
            this.__updateUriTimeOut = null;
        }.bind(this), 500);
    },

    set_fullUri: function(fullUri) {
        var nav = PageUri.parseUri(fullUri);
        this._uri = nav.uri;
        var names = [];

        for (var paramName in nav.params) {
            if (nav.params.hasOwnProperty(paramName)) {
                names.add(paramName);
            }
        }

        names.sort(function(a, b) { return nav.paramsIdx[a] - nav.paramsIdx[b]; })

        for (var i = 0; i < names.length; i++) {
            var paramName = names[i];
            this.set_param(paramName, nav.params[paramName]);
        }

        this.update_fullUri();
    },

    get_fullUri: function() {
        return this._fullUri;
    },
    
    get_param: function(key) {
        return this._params[key];
    },
    
    set_param: function(key, value) {
        var oldValue = this._params[key];

        if (oldValue === value) {
            return;
        }

        this._lastParamIdx++;
        this._params[key] = value;
        this._paramsIdx[key] = this._lastParamIdx;
        this.update_fullUri();
        this.raise_onParamChanged({key: key, newValue: value, oldValue: oldValue});
        this._set_sessionParam(key, value);
    },

    _set_sessionParam: function(key, value) {
        if (window.sessionStorage) {
           sessionStorage.setItem(this.get_uri() + ':' + key, value + '');
        }
    },
    
    loadParamsFromSession: function() {
        if (window.sessionStorage) {
            var pageUri = this.get_uri();
            for (var i = 0; i < sessionStorage.length; i++) {
                var storageKey = sessionStorage.key(i);

                var separatorIndex = storageKey.indexOf(':');
                if (separatorIndex == -1)
                    continue;

                var storedPageUri = storageKey.substring(0, separatorIndex);
                if (storedPageUri != pageUri)
                    continue;

                var paramKey = storageKey.substring(separatorIndex + 1);
                this.set_param(paramKey, sessionStorage.getItem(storageKey));
            }
        }
    },

    defaultOptions: {
        'width': '100%',
        height: '100%'
    },

    get_resource: function(resourceName) {
        return this.options.resources ? this.options.resources[resourceName] : null;
    },

    // ======== Methods ========
    initFromOptions: function(pageOptions) {
        if (typeof(pageOptions.onParamChanged) == 'function') {
            this.add_onParamChanged(pageOptions.onParamChanged, this);
        }

        this._resources = pageOptions.resources || {};

        Page.callBase(this, "initFromOptions", [ pageOptions ]);
        this._isLoaded = true;

        for (var i = 0; i < this._params.length; i++) {
            var param = this._params[i];
            this.raise_onParamChanged({key: param.key, value: param.value});
        }
    },
    
    initFromUri: function(pageUri) {
        this.abortLoading();
        this.set_fullUri(pageUri);

        if (!Application.pageIsAvailable(this.get_uri())) {
            pageUri = Application.get_error403Uri();
            this.set_fullUri(pageUri);
        }       
        
        this._isLoading = true;
        this._loadingXhr = Services.PagesService.getPage(this.get_uri(),
            function(data) {
                this._isLoading = false;
                this.loadParamsFromSession();
                this.initFromOptions(data);
            }.bind(this),
            function(request, errorType, errorThrown) {
                this._isLoading = false;
                var error = errorThrown;
                
                if (!error) {
                    try {
                        error = eval(request.responseText).Message;
                    }
                    catch(e) {
                        error = errorType + "\r\n" + request.responseText;
                    }
                }
                
                Application.throwError(errorThrown || error);
            }.bind(this)
        );
    },
    
    abortLoading : function() {
        if (this._isLoading && this._loadingXhr) {
            this._loadingXhr.abort();
        }
        
        this._isLoading = false;
        this._uri = null;
    },
    
    attachDataHandlers: function() {
        if (!this.options)
            return;

        var window = this;
        var data = window ? window.get_data() : null;

        if(data)
            this._attachForwardHandlers(data, true);
    },

    detachDataHandlers: function() {
        var window = this;

        if (window) {
            var data = window.get_data();
            if(data)
                this._detachForwardHandlers(data, true);
        }
    },

    free: function() {
        this.abortLoading();
        Page.callBase(this, "free");
    }
};

Auto.Events(Page.prototype, [
    'onParamChanged',
    'onMessage'
]);

Auto.Properties(Page.prototype, [
    { name: 'fullUri', autoEvent: true },
    { name: 'uri', autoEvent: true },
    { name: 'params' }
]);

Page.createClass('Page', Phoenix.UI.Panel);