var Debug = {
    getCallStack: function() {
        var callstack = [];
        
        var isCallstackPopulated = false;
        
        try {
            i.dont.exist += 0; //doesn't exist- that's the point
        } catch (e) {
            if (e.stack) { //Firefox
                var lines = e.stack.split("\n");
                for (var i = 0, len = lines.length; i < len; i++) {
                    if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
                        callstack.push(lines[i]);
                    }
                }
                //Remove call to printStackTrace()
                callstack.shift();
                isCallstackPopulated = true;
            }
            else if (window.opera && e.message) { //Opera
                var lines = e.message.split("\n");
                for (var i = 0, len = lines.length; i < len; i++) {
                    if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
                        var entry = lines[i];
                        //Append next line also since it has the file info
                        if (lines[i + 1]) {
                            entry += " at " + lines[i + 1];
                            i++;
                        }
                        callstack.push(entry);
                    }
                }
                //Remove call to printStackTrace()
                callstack.shift();
                isCallstackPopulated = true;
            }
        }
        if (!isCallstackPopulated) { //IE and Safari
            var currentFunction = arguments.callee.caller;
            while (currentFunction) {
                var fn = currentFunction.toString();
                var fname = fn.substring(fn.indexOf("function") + 8, fn.indexOf("(")) || "anonymous";
                callstack.push(fname);
                currentFunction = currentFunction.caller;
            }
        }
        
        return callstack;
    },
    
    getCallStackRoot: function() {
        return this.getCallStack().pop();
    }
};

Debug.startTracking = function(obj, breakFunctions, showArgsForFunctions) {
    var closure = function(p, breakpoint, showArgs) {
        return function() {
            var args = Array.prototype.slice.call(arguments || []);
            
            if (breakpoint) {
                console.log('Breakpoint for the function "' + p + '"');
                debugger;
            }
            
            var result = arguments.callee._old.apply(this, args);

            if (showArgs) {
                if (args.length > 0) {
                    console.log('Function "' + p + '" called for ', obj, 'with args: ', args, '. Result: ', result);
                } else {
                    console.log('Function "' + p + '" called for ', obj,' Result: ', result);
                }
            } else {
                    console.log('Function "' + p + '" called');
            }

            return result;
        }
    };

    for (var prop in obj) {
        if (typeof(obj[prop]) == 'function' && prop != 'toString') {
            var oldFunc = obj[prop];
            
            obj[prop] = closure(prop, breakFunctions && breakFunctions.contains(prop), showArgsForFunctions && showArgsForFunctions.contains(prop));
            obj[prop]._old = oldFunc;

            for (var oldProp in oldFunc) {
                obj[prop][oldProp] = oldFunc[oldProp];
            }
        }
    }
};

Debug.stopTracking = function(obj) {
    for (var prop in obj) {
        if (typeof(obj[prop]) == 'function' && prop != 'toString') {
            var oldFunc = obj[prop]._old;
            obj[prop] = oldFunc;
        }
    }
};

if (!window.console) {
    window.console = {
        log: function() {},
        error: function() {},
        info: function() {},
        warn: function() {},
        group: function() {},
        groupEnd: function() {}
    };
}

if (!console.info) {
    console.info = function() {};
}

if (!console.warn) {
    console.warn = function() {};
}

if (!console.error) {
    console.error = function() {};
}

Error.prototype.toString = function() {
    return this.name + '\r\n' + this.message + '\r\n' + (this.description || '');
}