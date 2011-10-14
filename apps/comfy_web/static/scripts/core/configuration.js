Configuration = function() { };

Configuration.prototype = {
    _configuration: null,
    
    // ======== Properties ========
    get_pagesServiceUrl: function() {
        return (this._configuration['pagesServiceUrl'] || '');
    },
    
    set_pagesServiceUrl: function(value) {
        this._configuration['pagesServiceUrl'] = value;
        return value;
    },
    
    get_rootUrl: function() {
        return (this._configuration['rootUrl'] || '');
    },

    get_error403Uri: function() {
        return (this._configuration['error403Uri'] || 'error403');
    },
    
    get_error404Uri: function() {
        return (this._configuration['error404Uri'] || 'error404');
    },

    get_startPageUri: function() {
        return (this._configuration['startPageUri'] || '');
    },
    
    set_startPageUri: function(value) {
        this._configuration['startPageUri'] = value;
    },
    
    set_rootUrl: function(value) {
        this._configuration['rootUrl'] = value;
        return value;
    },
    
    get_masterPage: function() {
        return (this._configuration['masterPage'] || '');
    },
    
    get_isDebug: function() {
        return !!this._configuration['isDebug'];
    },

    get_globalErrorHandling: function() {
        return !!this._configuration['globalErrorHandling'];
    },

    set_masterPage: function(value) {
        this._configuration['masterPage'] = value;
    },
    
    get_blankImageUrl: function() {
        return Application.resolveUrl(this._configuration['blankImageUrl'] || '~/Images/0.gif');
    },
    
    get_domElement: function() { return this._configuration['domElement']; },
    
    // ======== Methods ========
    init: function(configuration) {
        this._configuration = configuration;
    }
};

Configuration.createClass('Configuration');