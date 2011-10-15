
///////////////////////////////////////////////////////////////////////////////
// String Extensions

/*function isNullOrUndefined(o) {
    return (o === null) || (o === undefined);
}*/

function isString(o){
    return typeof(o) === 'string';
}

Number.prototype.pluralize = function Number$pluralize(s1, s2) {
    if (this == 1) {
        return this + ' ' + s1;
    }
    
    return this + ' ' + s2;
}

String.__typeName = 'String';
String.Empty = '';

String.compare = function String$compare(s1, s2, ignoreCase) {
    if (ignoreCase) {
        if (s1) {
            s1 = s1.toUpperCase();
        }
        if (s2) {
            s2 = s2.toUpperCase();
        }
    }
    s1 = s1 || '';
    s2 = s2 || '';

    if (s1 == s2) {
        return 0;
    }
    if (s1 < s2) {
        return -1;
    }
    return 1;
}

String.prototype.compareTo = function String$compareTo(s, ignoreCase) {
    return String.compare(this, s, ignoreCase);
}

String.prototype.endsWith = function String$endsWith(suffix) {
    if (!suffix.length) {
        return true;
    }
    if (suffix.length > this.length) {
        return false;
    }
    return (this.substr(this.length - suffix.length) == suffix);
}

String.equals = function String$equals1(s1, s2, ignoreCase) {
    return String.compare(s1, s2, ignoreCase) == 0;
}

String._format = function String$_format(format, values, useLocale) {
    if (!String._formatRE) {
        String._formatRE = /(\{[^\}^\{]+\})/g;
    }

    return format.replace(String._formatRE,
                          function(str, m) {
                              var index = parseInt(m.substr(1));
                              var value = values[index + 1];
                              if (isNullOrUndefined(value)) {
                                  return '';
                              }
                              if (value.format) {
                                  var formatSpec = null;
                                  var formatIndex = m.indexOf(':');
                                  if (formatIndex > 0) {
                                      formatSpec = m.substring(formatIndex + 1, m.length - 1);
                                  }
                                  return value.format.call(value, formatSpec, useLocale);
                              }
                              else {
                                  if (useLocale) {
                                      return value.toLocaleString();
                                  }
                                  return value.toString();
                              }
                          });
}

String.format = function String$format(format) {
    return String._format(format, arguments, /* useLocale */ false);
}

String.prototype.format = function() {
    var args = [ this ];
    
    for (var i = 0; i < arguments.length; i++) {
        args.add(arguments[i]);
    }
    
    return String._format(this, args, false);
};

String.fromChar = function String$fromChar(ch, count) {
    var s = ch;
    for (var i = 1; i < count; i++) {
        s += ch;
    }
    return s;
}

String.prototype.htmlDecode = function String$htmlDecode() {
    if (!String._htmlDecRE) {
        String._htmlDecMap = { '&amp;': '&', '&lt;': '<', '&gt;': '>', '&quot;': '"' };
        String._htmlDecRE = /(&amp;|&lt;|&gt;|&quot;)/gi;
    }

    var s = this;
    s = s.replace(String._htmlDecRE,
                  function String$htmlDecode$replace(str, m) {
                      return String._htmlDecMap[m];
                  });
    return s;
}

String.prototype.htmlEncode = function String$htmlEncode() {
    if (!String._htmlEncRE) {
        String._htmlEncMap = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' };
        String._htmlEncRE = /([&<>"])/g;
    }

    var s = this;
    if (String._htmlEncRE.test(s)) {
        s = s.replace(String._htmlEncRE,
                      function String$htmlEncode$replace(str, m) {
                          return String._htmlEncMap[m];
                      });
    }
    return s;
}

String.prototype.indexOfAny = function String$indexOfAny(chars, startIndex, count) {
    var length = this.length;
    if (!length) {
        return -1;
    }

    startIndex = startIndex || 0;
    count = count || length;

    var endIndex = startIndex + count - 1;
    if (endIndex >= length) {
        endIndex = length - 1;
    }

    for (var i = startIndex; i <= endIndex; i++) {
        if (chars.indexOf(this.charAt(i)) >= 0) {
            return i;
        }
    }
    return -1;
}

String.prototype.contains = function String$contains(string) {
    return this.indexOf(string) > -1;
};

String.prototype.insert = function String$insert(index, value) {
    if (!value) {
        return this;
    }
    if (!index) {
        return value + this;
    }
    var s1 = this.substr(0, index);
    var s2 = this.substr(index);
    return s1 + value + s2;
}

String.isNullOrEmpty = function String$isNullOrEmpty(s) {
    return !s || !s.length;
}

String.prototype.lastIndexOfAny = function String$lastIndexOfAny(chars, startIndex, count) {
    var length = this.length;
    if (!length) {
        return -1;
    }

    startIndex = startIndex || length - 1;
    count = count || length;

    var endIndex = startIndex - count + 1;
    if (endIndex < 0) {
        endIndex = 0;
    }

    for (var i = startIndex; i >= endIndex; i--) {
        if (chars.indexOf(this.charAt(i)) >= 0) {
            return i;
        }
    }
    return -1;
}

String.localeFormat = function String$localeFormat(format) {
    return String._format(format, arguments, /* useLocale */ true);
}

String.prototype.padLeft = function String$padLeft(totalWidth, ch) {
    if (this.length < totalWidth) {
        ch = ch || ' ';
        return String.fromChar(ch, totalWidth - this.length) + this;
    }
    return this;
}

String.prototype.padRight = function String$padRight(totalWidth, ch) {
    if (this.length < totalWidth) {
        ch = ch || ' ';
        return this + String.fromChar(ch, totalWidth - this.length);
    }
    return this;
}

String.prototype.quote = function String$quote() {
    if (!String._quoteMap) {
        String._quoteMap = { '\\' : '\\\\',
                             '\'' : '\\\'', '"' : '\\"',
                             '\r' : '\\r', '\n' : '\\n', '\t' : '\\t', '\f' : '\\f',
                             '\b' : '\\b' };
    }
    if (!String._quoteRE || !RegExp._cacheable) {
        String._quoteRE = new RegExp("([\'\"\\\\\x00-\x1F\x7F-\uFFFF])", "g");
    }

    var s = this;
    if (String._quoteSkipTest || String._quoteRE.test(s)) {
        s = this.replace(String._quoteRE,
                         function String$quote$replace(str, m) {
                             var c = String._quoteMap[m];
                             if (c) {
                                 return c;
                             }
                             c = m.charCodeAt(0);
                             return '\\u' + c.toString(16).toUpperCase().padLeft(4, '0');
                         });
    }
    return '"' + s + '"';
}

String.prototype.remove = function String$remove(index, count) {
    if (!count || ((index + count) > this.length)) {
        return this.substr(0, index);
    }
    return this.substr(0, index) + this.substr(index + count);
}

/*String.prototype._replace = String.prototype.replace;
String.prototype.replace = function String$replace(oldValue, newValue) {
    if (oldValue.constructor == String) {
        newValue = newValue || '';
        return this.split(oldValue).join(newValue);
    }
    return String.prototype._replace.call(this, oldValue, newValue);
}*/

String.prototype.replaceAll = function String$replaceAll(oldValue, newValue) {
    if (oldValue.constructor != String) {
        throw new Error('Argument exception');
    }
    
    return this.replace(new RegExp(RegExp.escape(oldValue), "g"), newValue);
}

String.prototype.startsWith = function String$startsWith(prefix) {
    if (!prefix.length) {
        return true;
    }
    if (prefix.length > this.length) {
        return false;
    }
    return (this.substr(0, prefix.length) == prefix);
}

String.prototype.trim = function String$trim() {
    return this.trimEnd().trimStart();
}

String.prototype.trimEnd = function String$trimEnd() {
    return this.replace(/\s*$/, '');
}

String.prototype.trimStart = function String$trimStart() {
    return this.replace(/^\s*/, '');
}

String.prototype.unquote = function String$unquote() {
    return eval('(' + this + ')');
}

String.prototype.toCamelCase = function String$toCamelCase() {
    return String.isNullOrEmpty(this) ?
           this :
           this.charAt(0).toLowerCase() + this.substring(1, this.length);
}

String.prototype.toPascalCase = function String$toPascalCase() {
    return String.isNullOrEmpty(this) ?
           this :
           this.charAt(0).toUpperCase() + this.substring(1, this.length);
};

String.prototype.toNumber = function() {
    return this * 1;
};

String.prototype.toBoolean = function() {
    if (this == 'true') 
        return true;
    if (this == 'false')
        return false;
    throw new Error('Cannot parse bool. Valid values are "true", "false"');
};

String.prototype.hashCode = function() {
    var hash = 0;

	if (this.length == 0) {
        return code;
    }

	for (i = 0; i < this.length; i++) {
		chr = this.charCodeAt(i);
		hash = 31*hash + chr;
		hash = hash & hash;
	}

	return hash;    
};

String.leftPad = function (val, size, ch) {
    var result = new String(val);
    
    if (ch === null || ch === undefined || ch === '') {
        ch = " ";
    }
    
    while (result.length < size) {
        result = ch + result;
    }
    
    return result;
};