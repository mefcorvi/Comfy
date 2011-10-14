Array.prototype.forEach = function(action, thisObject, inverse) {
    var func = !!thisObject ? action.bind(thisObject) : action;
    if(!inverse)    
        for (var i = 0; i < this.length; i++) {
            func(this[i], i);        
        }
    else
        for (var i = this.length - 1; i >= 0 ; i--) {
            func(this[i], i);        
        }
};

Array.prototype.select = function(selectFunc, thisObject) {
    if(typeof selectFunc === 'string') {
        var prop = selectFunc;
        
        selectFunc = function(item) {
            return item['get_' + prop] ?
                   item['get_' + prop]() :
                   item[prop];
        };
    }

    var func = !!thisObject ? selectFunc.bind(thisObject) : selectFunc;
    
    var result = [];

    this.forEach(function(item) {
        result.push(func(item));
    })

    return result;
};

Array.prototype.selectMany = function(selectFunc, thisObject) {
    var func = !!thisObject ? selectFunc.bind(thisObject) : selectFunc;

    var results = [];

    this.forEach(function(singleItem) {
        var singleResults = func(singleItem);

        singleResults.forEach(function(item) {
            results.push(item);
        });
    });

    return results;
};

Array.prototype.toDictionary = function(keySelector, valueSelector) {
    var result = {};

    this.forEach(function(item) {
        result[keySelector(item)] = valueSelector ? valueSelector(item) : item;
    });

    return result;
};

Array.prototype.where = function(filter, thisObject) {
    var filter = !!thisObject ? filter.bind(thisObject) : filter;

    var result = [];

    this.forEach(function(item) {
        if (filter(item)) {
            result.push(item);
        }
    });

    return result;
};

Array.prototype.first = function(filter, thisObject) {
    if (!filter)
        filter = function() { return true; }

    var filter = !!thisObject ? filter.bind(thisObject) : filter;

    for (var i = 0; i < this.length; i++) {
        if (filter(this[i]))
            return this[i];
    }

    return null;
};

Array.prototype.findByProperty = function(propertyName, value) {
    for (var i = 0; i < this.length; i++) {
        var item = this[i];
        var itemValue = item['get_' + propertyName] ? item['get_' + propertyName]() : item[propertyName];

        if (itemValue == value) {
            return item;
        }
    }
};

Array.prototype.last = function Array$last(filter, thisObject) {
    if (!filter)
        filter = function() { return true; }

    var filter = !!thisObject ? filter.bind(thisObject) : filter;    
    
    for (var i = this.length - 1; i >= 0 ; i--) {
        if (filter(this[i]))
            return this[i];
    }

    return null;
};

Array.prototype.single = function Array$single(filter, thisObject) {
    if (!filter)
        filter = function() { return true; }

    var founded = this.where(filter, thisObject);

    if (founded.length != 1)
        throw Error("array single exception");

    return founded[0];
};

Array.prototype.singleOrDefault = function Array$singleOrDefault(filter, thisObject) {
    var founded = this.where(filter, thisObject);

    if (founded.length > 1)
        throw Error("array single exception");    

    return founded.length == 1 ? founded[0] : null;
};

Array.prototype.contains = function Array$contains(input, comparer) {        
    /*if(input instanceof Array)
        return this._containsMany(input, comparer);*/
        
    if (!comparer) {
        return this.indexOf(input) >= 0;
    }

    for (var i = 0; i < this.length; i++) {
        if (comparer(input, this[i])) {
            return true;
        }
    }
    
    return false;
};

Array.prototype.containsMany = function Array$containsMany(items, comparer) {    
    for (var i = 0; i < items.length; i++) {
        if (!this.contains(items[i]), comparer) {
            return false;
        }
    }
        
    return true;
},

Array.prototype.distinct = function Array$distinct(comparer) {
    if(!comparer) {
        comparer = function(a, b) { return a === b };
    }
    
    var temp = [];

    for (var i = 0; i < this.length; i++) {
        var item = this[i];
        if (!temp.contains(item, comparer)) {
            temp.add(item);
        }
    }
    
    return temp;
};

Array.prototype.sum = function Array$sum(summator, thisObject) {
    var summator = !!thisObject ? summator.bind(thisObject) : summator;

    var result = 0;

    this.forEach(function(item) {
        result += summator(item);
    });

    return result;
};