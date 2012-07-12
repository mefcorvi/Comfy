var Pattern = function(pattern) {
    require([pattern, "String"]);
    this._pattern = ErlangParser.parse(pattern);
};

Pattern.prototype = {
    match: function(expression) {
	var parsed = ErlangParser.parse(expression);
	this._variables = {};
	return this._match(this._pattern, parsed);
    },

    get_variables: function() {
	return this._variables;
    },

    _match: function(s, t) {
	if (s === t) {
	    return true;
	}

	if (this._isTuple(s) && this._isTuple(t)) {
	    return this._match(s.tuple, t.tuple);
	}

	if (isArray(s) && isArray(t) && s.length == t.length) {
	    var result = true;
	    for (var i = 0; i < s.length; i++) {
		result &= this._match(s[i], t[i]);
	    }
	    return result;	    
	}

	if (this._isAtom(s) && this._isAtom(t)) {
	    return s.atom == t.atom;
	}

	if (this._isPid(s) && this._isPid(t)) {
	    return this._match(s.pid, t.pid);
	}

	if (this._isBinaryString(s) && this._isBinaryString(t)) {
	    return this._match(s.binaryString, t.binaryString);
	}

	if (this._isVariable(s) && this._isVariable(t)) {
	    return true;
	}

	if (this._isVariable(s)) {
	    if (this._variables.hasOwnProperty(s.variable)) {
		return this._match(this._variables[s.variable], t);
	    }

	    this._variables[s.variable] = t;
	    return true;
	}

	if (this._isVariable(t)) {
	    if (this._variables.hasOwnProperty(t.variable)) {
		return this._match(this._variables[t.variable], s);
	    }

	    this._variables[t.variable] = s;
	    return true;
	}

	return false;
    },

    _isTuple: function(v) {
	return isObject(v) && isArray(v.tuple);
    },

    _isAtom: function(a) {
	return isObject(a) && isString(a.atom);
    },

    _isVariable: function(v) {
	return isObject(v) && isString(v.variable);
    },

    _isPid: function(p) {
	return isObject(p) && (isString(p.pid) || this._isVariable(p.pid));
    },

    _isBinaryString: function(s) {
	return isObject(s) && (isString(s.binaryString) || this._isVariable(s.binaryString));
    }
};

Pattern.createClass("Pattern");