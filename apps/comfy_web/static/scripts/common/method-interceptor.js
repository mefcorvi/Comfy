/*
* Usage:
*     var methodInterceptor = new MethodInterceptor(<object>, function(originalFunc, arguments) {
*        originalFunc();
*    });
*    ...
*    methodInterceptor.restore();
*/

MethodInterceptor = function(target, funcName, methodInterceptorFunc) {
    this._target = target;
    this._oldFunc = {};
    this._funcName = funcName;
    this._methodInterceptorFunc = methodInterceptorFunc.bind(target);

    if (!isFunction(methodInterceptorFunc)) {
        throw new Error('You have to define a methodInterceptor function');
    }

    this.init();
};

MethodInterceptor.prototype = {
    init: function() {
        var target = this._target;
        var func = this._funcName;

        if (isFunction(target[func])) {
            this._oldFunc[func] = target[func];
            target[func] = (function(oldFunc, methodInterceptorFunc) {
                return function() {
                    var args = Array.prototype.slice.call(arguments || []);
                    return methodInterceptorFunc.call(this, oldFunc, args);
                }
            })(this._oldFunc[func], this._methodInterceptorFunc);
        } else {
            throw new Error('Cannot intercept function "' + func + '". That function is not exists.');
        }
    },
    
    restore: function() {
        var func = this._funcName;

        if (this._oldFunc.hasOwnProperty(func) && isFunction(this._oldFunc[func])) {
            this._target[func] = this._oldFunc[func];
        }

        this._oldFunc = {};
    }
};