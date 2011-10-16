Type.createNamespace('Services');

// TODO: Make sure that cache will be cleared
Services.PagesService = {
    _cache: {},
    
    getPage: function(pageUri, callback, error) {
	var serviceFullUrl = Application.resolveUrl(Application.get_configuration().get_pagesServiceUrl());
	
        if (this._cache[pageUri]){
            callback(eval(this._cache[pageUri]));
            return null;
        }
        
        return jQuery.ajax({
            type: 'GET',
            url: serviceFullUrl,
            data: { pageUri: pageUri },
            success: function(data) {
                this._cache[pageUri] = '(' + data + ')';
                callback(eval(this._cache[pageUri]));
            }.bind(this),
            error: error,
            dataType: 'text',
            cache: true
        });
    },

    getPagesList: function(callback, error) {
	var serviceFullUrl = Application.resolveUrl(Application.get_configuration().get_pagesServiceUrl());
        
        return jQuery.ajax({
            type: 'GET',
            url: serviceFullUrl + "/getLists",
            success: function(data) {
                callback(eval(data));
            }.bind(this),
            error: error,
            dataType: 'text',
            cache: true
        });        
    }
};