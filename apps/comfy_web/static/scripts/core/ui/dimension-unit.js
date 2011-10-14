var DimensionUnit = function(s, s2) {
    this._autoSize = false;
    this._stretchLevel = 0;
    this._isStretched = false;

    // percent, pixels
    if (!isNullOrUndefined(s) && !isNullOrUndefined(s2)) {
        this.percent = s * 1;
        this.pixels = s2 * 1;
    }
    else {
        // string
        if (isString(s)) {
            this.parse(s);
        }
        else if (!isNullOrUndefined(s)) {
            this.pixels = s * 1;
            this.percent = 0;
        }
    }
};

DimensionUnit.Zero = new DimensionUnit(0);
DimensionUnit.__parseRegex = /^([0-9]+?(?:\.[0-9]+?)?\%)?((?:\s?(?:\+|\-)\s?)?[0-9]+?(?:px)?)?$/;

Object.extend(DimensionUnit.prototype, {
    percent: null,
    pixels: null,
    _autoSize: null,
    _stretchLevel: null,
    _isStretched: null,
    
    add: function(du) {
        return new DimensionUnit(this.percent + (du.percent || 0), this.pixels + (du.pixels || 0));
    },
    
    subtract: function(du) {
        return new DimensionUnit(this.percent - (du.percent || 0), this.pixels - (du.pixels || 0));
    },
    
    divide: function(n) {
        return new DimensionUnit(this.percent / n, Math.round(this.pixels / n));
    },

    parse: function(s) {
        if (isObject(s) && (s instanceof DimensionUnit)) {
            return s;
        }

        this.percent = 0;
        this.pixels = 0;
            
        if (isString(s)) {
            if (s.match(/^([0-9]*?)\*$/)) { // stretch mode
                var args = /^([0-9]*?)\*$/.exec(s);
                this.percent = null;
                this.pixels = null;
                this._stretchLevel = args[1] * 1 || 1;
                this._isStretched = this._stretchLevel > 0;
                return;
            }
        
            if (s === '?') { // auto-size mode (depends on childs)
                this.percent = null;
                this.pixels = null;
                this._autoSize = true;
                return;
            }

            var args = DimensionUnit.__parseRegex.exec(s);
            
            if (isNullOrUndefined(args)) {
                throw new Error('Cannot convert \"' + s + '\" to DimensionUnit.');
            }
            
            for (var i = 1; i < args.length; i++) {
                if (isNullOrUndefined(args[i]))
                    continue;
            
                if (args[i].endsWith('%')) {
                    this.percent = args[i].substr(0, args[i].length - 1) / 100;
                }
                else {
                    var pixels = args[i].replace(/\s/g, '');
                    
                    if (pixels.endsWith('px')) {
                        pixels = pixels.substr(0, pixels.length - 2);
                    }
                        
                    this.pixels = pixels.toNumber();
                }
            }
            
            if (isNaN(this.pixels) || isNaN(this.percent)) {
                throw new Error('Cannot convert \"' + s + '\" to DimensionUnit.');
            }
        }
    },
    
    isNull: function() {
        return this.percent === null && this.pixels === null && !this.isStretched() && !this.isAutoSize();
    },

    getStretchLevel: function() {
        return this._stretchLevel;
    },
    
    isStretched: function() {
        return this._isStretched;
    },
    
    isAutoSize: function() {
        return this._autoSize;
    },

    isFixed: function() {
        return !this.percent && this.pixels > 0;
    },
    
    toString: function() {
        var result = '';
        
        if (this.percent) {
            result += Math.round(this.percent * 100) + '%';
        }
        
        if (this.pixels) {
            result += (this.pixels > 0 ? '+' : '') + this.pixels + 'px';
        }
           
        if (!result) {
            result = '0';
        }
            
        return result;
    }
});