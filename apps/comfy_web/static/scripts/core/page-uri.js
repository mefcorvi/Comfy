PageUri = {
    parseUri: function(pageUri) {
        var uriParts = pageUri.split('|');
        var uri = uriParts[0];

        var params = {};
        var paramsIdx = {};
        uriParts.removeAt(0);

        if (uriParts.length > 0) {
            paramsParts = uriParts;

            if (paramsParts) {
                for (var i = 0; i < paramsParts.length; i++) {
                    var paramPair = paramsParts[i].split('='),
                        key = PageUri.urlPartDecode(paramPair[0]),
                        value = isUndefined(paramPair[1]) ? null : PageUri.urlPartDecode(paramPair[1]);
                    
                    params[key] = value;
                    paramsIdx[key] = i;
                }
            }
        }

        return { uri: uri, params: params, paramsIdx: paramsIdx };
    },

    buildUri: function(uri, params, paramsOrderFunc) {
        var names = [];
        
        for (var paramName in params) {
            if (params.hasOwnProperty(paramName)) {
                names.add(paramName);
            }
        }

        if (paramsOrderFunc) {
            names.sort(paramsOrderFunc);
        }

        var newUri = uri;
        for (var i = 0; i < names.length; i++) {
            var paramName = names[i],
                paramValue;
            
            if (!isNullOrUndefined(params[paramName])) {
                paramValue = params[paramName].toString();
            } else {
                paramValue = '';
            }
                
            newUri += "|" + PageUri.urlPartEncode(paramName) + "=" + PageUri.urlPartEncode(paramValue);
        }

        return newUri;
    },

    urlPartEncode: function(value) {
        return value.replaceAll('&', '&amp;').replaceAll('=', '&eq;').replaceAll('|', '&sp;').replaceAll(' ', '&n;');
    },

    urlPartDecode: function(value) {
        return value.replaceAll('&n;', ' ').replaceAll('&sp;', '|').replaceAll('&eq;', '=').replaceAll('&amp;', '&');
    },

    getBasePart: function(pageUri) {
        return PageUri.parseUri(pageUri).uri;
    },

    getParamsPart: function(pageUri) {
        return PageUri.parseUri(pageUri).params;
    }
};