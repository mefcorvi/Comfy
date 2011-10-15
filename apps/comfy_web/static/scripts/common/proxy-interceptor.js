/*
* Usage:
*     var proxyInterceptor = new ProxyInterceptor(<object>, function(methodName, arguments, originalFunc) {
*        sqlconsole.log(methodName);
*    });
*    ...
*    proxyInterceptor.restore();
*/

ProxyInterceptor = function(target, proxyInterceptorFunc) {
    this._target = target;
    this._oldFunc = {};
    this._proxyInterceptorFunc = proxyInterceptorFunc;

    if (!isFunction(proxyInterceptorFunc)) {
        throw new Error('You have to define a proxyInterceptor function');
    }

    this.init();
};

ProxyInterceptor.prototype = {
    init: function() {
        var target = this._target;

        for (var func in target) {
            if (isFunction(target[func])) {
                this._oldFunc[func] = target[func];
                target[func] = (function(func, oldFunc, proxyInterceptorFunc) {
                    return function() {
                        var args = Array.prototype.slice.call(arguments || []);
                        proxyInterceptorFunc(func, args, oldFunc);
                    }
                })(func, this._oldFunc[func], this._proxyInterceptorFunc);
            }
        }
    },
    
    restore: function() {
        for (var func in this._oldFunc) {
            if (this._oldFunc.hasOwnProperty(func) && isFunction(this._oldFunc[func])) {
                this._target[func] = this._oldFunc[func];
            }
        }

        this._oldFunc = {};
    }
};