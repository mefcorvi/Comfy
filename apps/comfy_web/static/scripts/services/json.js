(function($) {
    $.toJSON = function(o, deepLevel) {
        var type = typeof (o);

        if (o === null)
            return "null";

        if (type == "undefined")
            return undefined;

        if (type == "number" || type == "boolean")
            return o + "";

        if (type == "string")
            return $.quoteString(o);

        if (type == 'object') {
            if (o.constructor === Date) {
                var month = o.getUTCMonth() + 1;
                if (month < 10) month = '0' + month;

                var day = o.getUTCDate();
                if (day < 10) day = '0' + day;

                var year = o.getUTCFullYear();

                var hours = o.getUTCHours();
                if (hours < 10) hours = '0' + hours;

                var minutes = o.getUTCMinutes();
                if (minutes < 10) minutes = '0' + minutes;

                var seconds = o.getUTCSeconds();
                if (seconds < 10) seconds = '0' + seconds;

                var milli = o.getUTCMilliseconds();
                if (milli < 100) milli = '0' + milli;
                if (milli < 10) milli = '0' + milli;

                return '"' + year + '-' + month + '-' + day + 'T' +
                             hours + ':' + minutes + ':' + seconds +
                             '.' + milli + 'Z"';
            }

            if (o.constructor === Array) {
                if (o.__tracking) {
                    if (isDefined(deepLevel) && deepLevel <= 0) {
                        return null;
                    }

                    return $.toJSON({
                        __type: o.__type,
                        added: o.added,
                        items: o.items,
                        removed: o.removed
                    }, isDefined(deepLevel) ? (deepLevel - 1) : undefined);
                }

                var ret = [];
                for (var i = 0; i < o.length; i++)
                    ret.push($.toJSON(o[i], deepLevel) || "null");

                return "[" + ret.join(",") + "]";
            }

            var pairs = [];
            var properties = [];

            for (propertyName in o) {
                if (propertyName != '__type') {
                    properties.add(propertyName);
                }
            }

            if (o['__type']) {
                properties.insert(0, '__type');
            }

            for (var i = 0; i < properties.length; i++) {
                var k = properties[i];
                if (k.startsWith('__') && k !== '__type')
                    continue;

                var name;
                var type = typeof k;

                if (type == "number")
                    name = '"' + k + '"';
                else if (type == "string")
                    name = $.quoteString(k);
                else
                    continue;  //skip non-string or number keys                

                if (typeof o[k] == "function")
                    continue;  //skip pairs where the value is a function.

                var val = $.toJSON(o[k], deepLevel);

                pairs.push(name + ":" + val);
            }

            return "{" + pairs.join(", ") + "}";
        }
    };

    $.evalJSON = function(src) {
        return eval("(" + src + ")");
    };

    $.quoteString = function(string) {
        if (string.startsWith("/Date")) {
            string = string.replace(new RegExp('/','g'), "\\/");
        }
        
        if (string.startsWith("\\/Date")) {
            return '"' + string + '"';
        }

        if (string.match(_escapeable)) {
            return '"' + string.replace(_escapeable, function(a) {
                var c = _meta[a];
                if (typeof c === 'string') return c;
                c = a.charCodeAt();
                return '\\u00' + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
            }) + '"';
        }
        return '"' + string + '"';
    };

    var _escapeable = /["\\\x00-\x1f\x7f-\x9f]/g;

    var _meta = {
        '\b': '\\b',
        '\t': '\\t',
        '\n': '\\n',
        '\f': '\\f',
        '\r': '\\r',
        '"': '\\"',
        '\\': '\\\\'
    };
})(jQuery);