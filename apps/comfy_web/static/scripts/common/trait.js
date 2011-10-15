Trait = {
    Apply: function(target, source, options) {
        var cache = source.__$traitPropertiesCache;
        
        if (!cache) {
            cache = [];
            
            for (var prop in source) {
                cache.add(prop);
            }
            
            source.__$traitPropertiesCache = cache;
        }
        
        for (var i = 0; i < cache.length; i++) {
            Trait._applyForProperty(target, source, options, cache[i]);
        }
    },
    
    _applyForProperty: function(target, source, options, prop) {
        if (typeof(source[prop]) == 'function') {
            if(target[prop] && typeof(target[prop]) == 'function') {
                this._traitFunction(
                    target, prop, source[prop], 
                    options && options.asOriginal && options.asOriginal.contains(prop),
                    options && options.overrideAll && options.overrideAll.contains(prop));
            }
            else
                target[prop] = source[prop];
        }
        else {
            target[prop] = source[prop];
        }
    },
    
    _traitFunction: function(object, funcName, newFunc, asOriginal, overrideAll) {
        if(object[funcName].__overriden) {
            return;
        }
        
        if(overrideAll) {
            object[funcName] = newFunc;
            return;
        }
    
        if(!object[funcName].__traits) {
            var original = object[funcName];
            
            var traitedFunc = function() {
                var args = Array.prototype.slice.call(arguments || []);
                
                var originalResult = arguments.callee.__traits[0].apply(this, args);
                
                // There been issue when original function was called with lesser count of arguments
                // args[args.length] = originalResult;
                var originalResultArgsIndex = original.length;

                // If function was called with greater count of arguments we put original
                // result to the end of arguments list
                if (args.length >= originalResultArgsIndex) {
                    originalResultArgsIndex = args.length;
                }

                args[originalResultArgsIndex] = originalResult;
                
                for(var i = 1 ; i < arguments.callee.__traits.length; i++)
                    arguments.callee.__traits[i].apply(this, args);
                    
                return originalResult;
            };
            
            traitedFunc.__traits = [original];
            
            object[funcName] = traitedFunc;
        }
        
        if(asOriginal) {
            // TODO: Warning! The source function can already has a link to the original function at the next line!
            newFunc.original = object[funcName].__traits[0]; // TODO: Check behaviour of that code on a chain of traits
            object[funcName].__traits[0] = newFunc;
        }
        else        
            object[funcName].__traits.add(newFunc);
    }
};