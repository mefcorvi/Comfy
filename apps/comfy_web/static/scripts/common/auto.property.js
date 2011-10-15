/// <reference path="Object.js" />
/// <reference path="Data/Array.js" />
/// <reference path="Data/Array.Linq.js" />
if (!window.Auto)
    Auto = {};

Object.extend(Auto, {
    Property: function(object, property) {
        // array used to avoid cicles when set_prop(abc) causes invoking set_prop(abc);

        if (property.autoEvent) {
            Auto.Event(object, property.name + 'Changed');
        }

        this._attachProperty(
            object,
            property);
    },

    Properties: function(object, properties) {
        properties.forEach(function(property) {
            this.Property(object, property);
        }, this);
    },

    _attachProperty: function(object, property) {
        var name = typeof(property) === 'string' ?
                   property : property.name;
                   
        if (!name) {
            throw new Error('[Auto.Property]: property name is undefined');
        }
                   
        var getterName = 'get_' + name;
        
        if(!object[getterName]) {
            var getter = !isUndefined(property.defaultValue) || isFunction(property.get_defaultValue) ?
                 function() {
                    if (!this.hasOwnProperty('_' + name)) {
                        this['_' + name] = arguments.callee.get_defaultValue();
                    }

                    return this['_' + name];
                 } :
                 function() {
                    return this['_' + name];
                 }
        
            object[getterName] = getter;
            object[getterName].autoGetter = true;

            if (!isUndefined(property.defaultValue)) {
                var defValue = property.defaultValue;

                if (defValue && isObject(defValue)) {
                    console.warn("[Auto.Property] Using of object as a default value of the property can cause unexpected behaviour. Property name: ", property.name, ". Definition: ", property);
                }

                object[getterName].get_defaultValue = function() {
                    if (defValue && isObject(defValue)) {
                        return Object.copy(defValue);
                    }

                    return defValue;
                };
            }

            if (isFunction(property.get_defaultValue)) {
                object[getterName].get_defaultValue = property.get_defaultValue;
            }
        }

        if(!object['set_' + name]) {        
            var setter = !property.autoEvent ?
                function Auto$Property$setter(value) {
                    if (this['_' + name] === value)
                        return;

                    this['_' + name] = value;
                } :
                function Auto$Property$setter$withEvent(value) {
                    var propertyName = '_' + name;
                    if (this[propertyName] === value)
                        return;
                        
                    var oldValue = this.hasOwnProperty(propertyName) ? this[propertyName] : this['get' + propertyName]();

                    this[propertyName] = value;

                    this['raise_' + name + 'Changed']({
                        newValue: value,
                        oldValue: oldValue
                    });
                };

            setter.autoSetter = true;
            object['set_' + name] = setter;
        }
        
        if (!object['set_' + name].override) {
            // override setter to avoid stack overflow
            // Warning: this code can cause to unexpected behaviour in some cases
            object['__$set_' + name] = object['set_' + name];
            object['set_' + name] = function Auto$Property$setter(value) {
                if (!this.__alreadyCalledWith) {
                    this.__alreadyCalledWith = {};
                }

                if (!this.__alreadyCalledWith[name]) {
                    this.__alreadyCalledWith[name] = [];
                }

                var alreadyCalledWith = this.__alreadyCalledWith[name];
                
                if (!alreadyCalledWith.contains(value)) {
                    alreadyCalledWith.add(value);
                    this['__$set_' + name].apply(this, arguments);
                    alreadyCalledWith.remove(value);
                }
            }
            object['set_' + name].override = true;
        }
    }
});

Auto.Property.CreateOneWayLink = function(obj1, obj2, property1, property2) {
    if (!property2) {
        property2 = property1;
    }

    var linkInfo = {
        _obj1: obj1,
        _obj2: obj2,
        _property1: property1,
        _property2: property2,
        _obj1Handler: function(sender, args) {
            this._obj2['set_' + this._property2](args.newValue);
        },
        dispose: function() {
            this._obj1['remove_' + this._property1 + 'Changed'](this._obj1Handler);
        }
    };

    var handler1 = obj1['add_' + property1 + 'Changed'];

    if (!isFunction(handler1)) {
        throw new Error("[Auto.Property.CreateLink] Cannot find the event handler for \"" + property1 + "\" property");
    }

    obj2['set_' + property2](obj1['get_' + property1]());
    obj1['add_' + property1 + 'Changed'](linkInfo._obj1Handler, linkInfo);

    return linkInfo;
}

Auto.Property.CreateLink = function(obj1, obj2, property1, property2) {
    if (!property2) {
        property2 = property1;
    }

    var handler1 = obj1['add_' + property1 + 'Changed'];
    var handler2 = obj2['add_' + property2 + 'Changed'];

    if (!isFunction(handler1)) {
        throw new Error("[Auto.Property.CreateLink] Cannot find the event handler for \"" + property1 + "\" property");
    }

    if (!isFunction(handler2)) {
        throw new Error("[Auto.Property.CreateLink] Cannot find the event handler for \"" + property2 + "\" property");
    }

    obj2['set_' + property2](obj1['get_' + property1]());

    var linkInfo = {
        _obj1: obj1,
        _obj2: obj2,
        _property1: property1,
        _property2: property2,
        _obj1Handler: function(sender, args) {
            this._obj2['set_' + this._property2](args.newValue);
        },
        _obj2Handler: function(sender, args) {
            this._obj1['set_' + this._property1](args.newValue);
        },
        dispose: function() {
            this._obj1['remove_' + this._property1 + 'Changed'](this._obj1Handler);
            this._obj2['remove_' + this._property2 + 'Changed'](this._obj2Handler);
        }
    };

    obj1['add_' + property1 + 'Changed'](linkInfo._obj1Handler, linkInfo);
    obj2['add_' + property2 + 'Changed'](linkInfo._obj2Handler, linkInfo);

    return linkInfo;
};