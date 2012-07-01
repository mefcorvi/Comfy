/// <reference path="Function.js" />

required = function(obj, params) {
  if (isNullOrUndefined(obj))
    throw new Error("Argument is null");

  if (params) {
    for (var i = 0; i < params.length; i++) {
        if (isNullOrUndefined(obj[params[i]]))
            throw new Error("Argument " + params[i] + " is null");
    }
  }
};

require = function(params) {
    if (!isArray(params)) {
	throw new Error("Params should be an array.");
    }

    for (var i = 0; i < params.length; i += 2) {
	var value = params[i];
	var type = params[i + 1];
	var guardFunction = require.typeGuards[type];

	if (!isFunction(guardFunction)) {
	    throw new Error("Guard function for \"" + type + "\" type have not been found.");
	}

	if (!guardFunction(value)) {
	    throw new Error('[' + type + ' guard]: ' + value + ' is wrong value.');
	}
    }
};

require.typeGuards = {
    "String": isString,
    "Function": isFunction
};

tryEval = function(text, defaulValue) {
    try {
        return eval(text);
    } catch(e) {
        console.warn("Cannot evaluate the expression: \"" + text + "\"", e);
        return isUndefined(defaulValue) ? text : defaulValue;
    }
};

isNumber = function(o) {
    return typeof(o) == 'number';
};

isBoolean = function(o) {
    return typeof(o) == 'boolean';
};

isArray = function(o) {
    return o instanceof Array;
};

isObject = function(o) {
    return typeof(o) == 'object';
};

isUndefined = function(o) {
    return typeof(o) == 'undefined';
};

isNullOrUndefined = function(o) {
    return o === null || isUndefined(o);
};

isDefined = function(o) {
    return !isNullOrUndefined(o);
};

Object.extend = function (target, src, deep) {
    if (!src)
        return target;

    for (var prop in src) {
        if (prop === 'constructor' || (typeof (target) == 'function' && Function.prototype[prop]))
            continue;

        if (!src.hasOwnProperty(prop))
            continue;

        if (typeof src[prop] === 'function') {
            target[prop] = src[prop];
        } else if (typeof (src[prop]) === 'object') {
            target[prop] = (deep && src[prop]) ? Object.copy(src[prop], true) : src[prop];
        } else {
            target[prop] = src[prop];
        }
    }

    //HACK: IE
    if (src.hasOwnProperty('toString')) {
        target.toString = src.toString;
    }

    return target;
};

Object.copy = function(src, deep) {
    if (Entity.isInstance(src)) {
        throw new Error('copying of entities is prohibited');
    }

    if (src.isEnum)  {
        return src;
    }

    var copy;
    
    if (src instanceof Array) {
        copy = [];
    } else {
        var clonedObject = function() {};
        clonedObject.prototype = src.constructor.prototype;
        copy = new clonedObject();
    }    
    
    Object.extend(copy, src, deep);
    
    if (typeof(src.constuctor) == 'function') {
        copy.constructor = src.constructor;
    }

    return copy;
};

Object.keysCount = function(obj) {
    var result = 0;

    for (var prop in obj) {
        if (obj.hasOwnProperty(prop)) {
            result++;
        }
    }

    return result;
};

Object.getPropertyValue = function(obj, propName) {
    if (obj[propName])
        return obj[propName];
    
    if (obj['get_' + propName])
        return obj['get_' + propName]();

    return false;
};

//Objec.prototype.hash = function() {
//    var hash = '{';

//    for (var prop in this) {
//        if (typeof this[prop] === 'function')
//            continue;

//        hash = hash.concat(
//            (typeof this[prop] === 'object') ?
//                this[prop].hash() :
//                this[prop].toString()
//        ).concat(',');
//    }

//    hash = hash.replace(/,$/, '');
//    return hash + '}';
//};

//Object.prototype.equals = function(right) {
//    if(typeof this === 'function' && typeof right === function)
//        return this.toString() === right.toString();
//        
//    return this.hash() === right.hash();
//};