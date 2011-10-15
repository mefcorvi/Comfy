Array.__typeName='Array';

Array.prototype.add=function Array$add(input) {
    if(!input || !(input instanceof Array)) {
        this[this.length]=input;
        return;
    }
    
    var length = input.length;

    for(var i=0; i<length; i++)
        this[this.length]=input[i];
        
    return input;
}

Array.prototype.addItem=function Array$addItem(input) {
    this[this.length]=input;
}

Array.prototype.ofType=function Array$ofType(type) {
    var result = [];

    if (isString(type)) {
        type = window[type];
    }

    for (var i = 0; i < this.length; i++) {
        if (!isNullOrUndefined(this[i]) && this[i].constructor && this[i].constructor === type) {
            result.add(this[i]);
        }
    }

    return result;
}

Array.prototype.aggregate=function Array$aggregate(seed,callback) {
    var length=this.length;

    for(var index=0;index<length;index++) {
        seed=callback(seed,this[index],index,this);
    }

    return seed;
}

Array.prototype.clear=function Array$clear() {
    if(this.length>0) {
        this.splice(0,this.length);
    }
}

Array.prototype.clone=function Array$clone() {
    var length=this.length;
    var array=new Array(length);

    for(var index=0;index<length;index++) {
        array[index]=this[index];
    }

    return array;
}

Array.prototype.dequeue=function Array$dequeue() {

    return this.shift();

}

Array.prototype.enqueue=function Array$enqueue(item) {

    // We record that this array instance is a queue, so we

    // can implement the right behavior in the peek method.

    this._queue=true;

    this.push(item);

}

Array.prototype.peek=function Array$peek() {

    if(this.length) {

        var index=this._queue?0:this.length-1;

        return this[index];

    }

    return null;

}

if(!Array.prototype.every) {

    Array.prototype.every=function Array$every(callback) {

        for(var i=this.length-1;i>=0;i--) {

            if(!callback(this[i],i,this)) {

                return false;

            }

        }

        return true;

    }

}

Array.prototype.extract=function Array$extract(index,count) {

    if(!count) {

        return this.slice(index);

    }

    return this.slice(index,index+count);

}

if (!Array.prototype.filter) {
    Array.prototype.filter = function Array$filter(callback) {
        var filtered = [];

        for (var i = 0; i < this.length; i++) {
            if (callback(this[i], i, this)) {
                filtered.add(this[i]);
            }
        }

        return filtered;
    }
}


Array.prototype.index=function Array$index(callback) {

    var length=this.length;

    var items={};

    for(var index=0;index<length;index++) {

        var key=callback(this[index],index);

        if(String.isNullOrEmpty(key)) {

            continue;

        }

        items[key]=this[index];

    }

    return items;

}

if (!Array.indexOf) {
    Array.prototype.indexOf = function Array$indexOf(item) {
        var length = this.length;
        
        if (length) {
            for (var index = 0;index < length; index++) {
                if (this[index] === item) {
                    return index;
                }
            }
        }
        
        return -1;
    }
}

Array.prototype.insert=function Array$insert(index, input) {
    if(input instanceof Array) {        
        for(var i = input.length - 1 ; i >= 0; i--)
            this.splice(index, 0 ,input[i]);
            
        return;
    }
    
    this.splice(index, 0 ,input);
}

Array.prototype.insertRange=function Array$insertRange(index,items) {

    this.splice(index,0,items);

}

if(!Array.prototype.map) {

    Array.prototype.map=function Array$map(callback) {

        var mapped=new Array(this.length);

        for(var i=this.length-1;i>=0;i--) {

            mapped[i]=callback(this[i],i,this);

        }

        return mapped;

    }

}

Array.parse=function Array$parse(s) {

    return eval('('+s+')');

}

Array.prototype.remove=function Array$remove(input) {
    if(!(input instanceof Array)) {
        var index=this.indexOf(input);
        
        if(index < 0)
            throw new Error('Item not found in array on remove.');

        this.splice(index,1);
            
        return;
    }

    for(var i=0; i < input.length; i++) {
        var index = this.indexOf(input[i]);
        
        if(index < 0)
            throw new Error('Item not found in array on remove.');
            
        this.splice(index,1);
    }
}

Array.prototype.exclude = function Array$exclude(input) {
    if (!(input instanceof Array)) {
        var index = this.indexOf(input);

        if (index < 0) {
            return [];
        }

        this.splice(index, 1);

        return [input];
    }

    var excluded = [];

    for (var i = 0; i < input.length; i++) {
        var index = this.indexOf(input[i]);

        if (index < 0) {
            continue;
        }

        excluded.add(input[i]);
        this.splice(index, 1);
    }

    return excluded;
}

Array.prototype.removeAt=function Array$removeAt(index) {
    return this.splice(index,1)[0];
}

Array.prototype.removeRange=function Array$removeRange(index,count) {
    return this.splice(index,count);
}

if(!Array.prototype.some) {
    Array.prototype.some=function Array$some(callback) {
        for(var i=this.length-1;i>=0;i--) {
            if(callback(this[i],i,this)) {
                return true;
            }
        }
        return false;
    }
} 

Array.prototype.synchronize = function Array$synchronize(source, comparer, args) {    
    var toAdd = [];
    var toRemove = [];

    for(var i = this.length-1; i >= 0; i--) {
        if(!source.contains(this[i], comparer))
            toRemove.add(this[i]);
    }
    
    for(var i = 0; i < source.length; i++) {
        if(!this.contains(source[i], comparer))
            toAdd.add(source[i]);
    }
    
    this.remove(toRemove, args);
    this.add(toAdd, args);
    
    return toAdd.length + toRemove.length;
};

Array.prototype.merge = function Array$merge(source, comparer, args) {
    var toAdd = source.where(function(item) {
        return !this.contains(item, comparer);
    }, this);
        
    this.add(toAdd, args);
};