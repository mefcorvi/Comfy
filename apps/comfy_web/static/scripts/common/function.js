var isFunction = function(o) {
    return typeof(o) === 'function';
};

Function.prototype.bind = function bind(obj) {
    var func = this;

    return function Function$bind() {
        var args = Array.prototype.slice.call(arguments || []);

        return func.apply(obj, args);
    };
};

Function.prototype.after = function(target) {
    var func = this;    
    
    return function() {
        var args = Array.prototype.slice.call(arguments || []);
    
        target.apply(arguments.caller, args);
        return func.apply(arguments.caller, args);
    };
};

Function.prototype.before = function(target) {
    var func = this;    
    
    return function() {
        var args = Array.prototype.slice.call(arguments || []);
    
        result = func.apply(arguments.caller, args);        
        target.apply(arguments.caller, args);
        
        return result;
    };
};

Function.prototype.delay = function(delay, thisObj) {
    delay = isUndefined(delay) ? 0 : delay;
    var func = this;

    window.setTimeout(function() {
        if (thisObj) {
            func.call(thisObj);
        } else {
            func();
        }
    }, delay);
};

Function.prototype.when = function(condition) {
    if(!condition || typeof(condition) !== 'function')
        throw new Error('wrong Function.when argument');

    var delay = 100;
    var func = this;

    var intervalId = window.setInterval(function() {
        if(!condition())
            return;
            
        window.clearInterval(intervalId);
        func();
    }, delay);
};