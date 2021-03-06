
Date.__typeName = 'Date';

Date.get_now = function Date$get_now() {
    return new Date();
}

Date.get_today = function Date$get_today() {
    var d = new Date();
    return new Date(d.getFullYear(), d.getMonth(), d.getDate());
}

Date.prototype.format = function Date$format(format, useLocale) {
    if (isNullOrUndefined(format) ||
        (format.length == 0) || (format == 'i')) {
        if (useLocale) {
            return this.toLocaleString();
        }
        else {
            return this.toString();
        }
    }
    if (format == 'id') {
        if (useLocale) {
            return this.toLocaleDateString();
        }
        else {
            return this.toDateString();
        }
    }
    if (format == 'it') {
        if (useLocale) {
            return this.toLocaleTimeString();
        }
        else {
            return this.toTimeString();
        }
    }

    return this._netFormat(format, useLocale);
}

Date.prototype._netFormat = function Date$_netFormat(format, useLocale) {
    var dtf = useLocale ? CultureInfo.Current.dateFormat : CultureInfo.Neutral.dateFormat;
    var useUTC = false;

    if (format.length == 1) {
        switch (format) {
            case 'f': format = dtf.longDatePattern + ' ' + dtf.shortTimePattern;
            case 'F': format = dtf.dateTimePattern; break;

            case 'd': format = dtf.shortDatePattern; break;
            case 'D': format = dtf.longDatePattern; break;

            case 't': format = dtf.shortTimePattern; break;
            case 'T': format = dtf.longTimePattern; break;

            case 'g': format = dtf.shortDatePattern + ' ' + dtf.shortTimePattern; break;
            case 'G': format = dtf.shortDatePattern + ' ' + dtf.longTimePattern; break;

            case 'R': case 'r': format = dtf.gmtDateTimePattern; useUTC = true; break;
            case 'u': format = dtf.universalDateTimePattern; useUTC = true; break;
            case 'U': format = dtf.dateTimePattern; useUTC = true; break;

            case 's': format = dtf.sortableDateTimePattern; break;
        }
    }

    if (format.charAt(0) == '%') {
        format = format.substr(1);
    }

    if (!Date._formatRE) {
        Date._formatRE = /dddd|ddd|dd|d|MMMM|MMM|MM|M|yyyy|yy|y|hh|h|HH|H|mm|m|ss|s|tt|t|fff|ff|f|zzz|zz|z/g;
    }

    var re = Date._formatRE;    
    var sb = new StringBuilder();
    var dt = this;
    if (useUTC) {
        dt = new Date(Date.UTC(dt.getUTCFullYear(), dt.getUTCMonth(), dt.getUTCDate(),
                               dt.getUTCHours(), dt.getUTCMinutes(), dt.getUTCSeconds(), dt.getUTCMilliseconds()));
    }

    re.lastIndex = 0;
    while (true) {
        var index = re.lastIndex;
        var match = re.exec(format);

        sb.append(format.slice(index, match ? match.index : format.length));
        if (!match) {
            break;
        }

        var fs = match[0];
        var part = fs;
        switch (fs) {
            case 'dddd':
                part = dtf.dayNames[dt.getDay()];
                break;
            case 'ddd':
                part = dtf.shortDayNames[dt.getDay()];
                break;
            case 'dd':
                part = dt.getDate().toString().padLeft(2, '0');
                break;
            case 'd':
                part = dt.getDate();
                break;
            case 'MMMM':
                part = dtf.monthNames[dt.getMonth()];
                break;
            case 'MMM':
                part = dtf.shortMonthNames[dt.getMonth()];
                break;
            case 'MM':
                part = (dt.getMonth() + 1).toString().padLeft(2, '0');
                break;
            case 'M':
                part = (dt.getMonth() + 1);
                break;
            case 'yyyy':
                part = dt.getFullYear();
                break;
            case 'yy':
                part = (dt.getFullYear() % 100).toString().padLeft(2, '0');
                break;
            case 'y':
                part = (dt.getFullYear() % 100);
                break;
            case 'h': case 'hh':
                part = dt.getHours() % 12;
                if (!part) {
                    part = '12';
                }
                else if (fs == 'hh') {
                    part = part.toString().padLeft(2, '0');
                }
                break;
            case 'HH':
                part = dt.getHours().toString().padLeft(2, '0');
                break;
            case 'H':
                part = dt.getHours();
                break;
            case 'mm':
                part = dt.getMinutes().toString().padLeft(2, '0');
                break;
            case 'm':
                part = dt.getMinutes();
                break;
            case 'ss':
                part = dt.getSeconds().toString().padLeft(2, '0');
                break;
            case 's':
                part = dt.getSeconds();
                break;
            case 't': case 'tt':
                part = (dt.getHours() < 12) ? dtf.amDesignator : dtf.pmDesignator;
                if (fs == 't') {
                    part = part.charAt(0);
                }
                break;
            case 'fff':
                part = dt.getMilliseconds().toString().padLeft(3, '0');
                break;
            case 'ff':
                part = dt.getMilliseconds().toString().padLeft(3).substr(0, 2);
                break;
            case 'f':
                part = dt.getMilliseconds().toString().padLeft(3).charAt(0);
                break;
            case 'z':
                part = dt.getTimezoneOffset() / 60;
                part = ((part >= 0) ? '-' : '+') + Math.floor(Math.abs(part));
                break;
            case 'zz': case 'zzz':
                part = dt.getTimezoneOffset() / 60;
                part = ((part >= 0) ? '-' : '+') + Math.floor(Math.abs(part)).toString().padLeft(2, '0');
                if (fs == 'zzz') {
                    part += dtf.timeSeparator + Math.abs(dt.getTimezoneOffset() % 60).toString().padLeft(2, '0');
                }
                break;
        }
        sb.append(part);
    }

    return sb.toString();
}

Date.$__parse = Date.parse;
Date.parse = function Date$parse(s) {
    // Date.parse by default returns the number of milliseconds
    // and not an actual Date instance
    return new Date(Date.$__parse(s));
}

Date.parseWcf = function Date$parseWcf(s) {
    if (isNullOrUndefined(s))
        return s; 

    var format = /.*?Date\((-?[0-9]+?)(?:[\-\+])([0-9]{2})([0-9]{2})\).*?/;
    var parsed = format.exec(s);
    
    if (!parsed[1] || !parsed[2] || !parsed[3]) {
        throw new Error('[Date] Cannot create Date object from "' + s + '".');
    }
    
    var d = new Date();
    
    /*var newOffset = parsed[2].toNumber() * 60 + parsed[3].toNumber();
    var currentOffset = -d.getTimezoneOffset();*/
    
    d.setTime(parsed[1].toNumber()/* + (currentOffset - newOffset) * 60 * 1000*/);
    
    return d;
}

Date.prototype.toWcfString = function Date$toWcfString() {
    return '\\/Date(' + this.getTime() + this.getGMTOffset() + ')\\/';
};


Date.prototype.getGMTOffset = function() {
    var offset = this.getTimezoneOffset();
    return (offset > 0 ? "-" : "+")
        + String.leftPad(Math.abs(Math.floor(offset / 60)), 2, "0")
        + String.leftPad(offset % 60, 2, "0");
};

Date.prototype.isValid = function Date$isValid() {
    return !isNaN(this.getTime());
};