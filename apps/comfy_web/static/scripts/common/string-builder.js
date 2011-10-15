var StringBuilder = function StringBuilder$(s) {

    if ((s !== undefined) && (s !== null)) {
        this._parts = [ s ];
    }
    else {
        this._parts = [];
    }
}

StringBuilder.prototype = {
    get_isEmpty: function StringBuilder$get_isEmpty() {
        return (this._parts.length == 0);
    },

    append: function StringBuilder$append(s) {
        if ((s !== undefined) && (s !== null)) {
            this._parts.add(s);
        }
    },

    appendLine: function StringBuilder$appendLine(s) {
        this.append(s);
        this.append('\r\n');
    },

    clear: function StringBuilder$clear() {
        this._parts.clear();
    },

    toString: function StringBuilder$toString() {
        return this._parts.join('');
    }
};

StringBuilder.createClass('StringBuilder');