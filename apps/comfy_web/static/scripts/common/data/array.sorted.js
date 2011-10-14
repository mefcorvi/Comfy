SortedArray = {
    add: function (data, args) {        
        var items = data instanceof Array ? data : [data];

        for (var i = 0; i < items.length; i++)
            this.splice(this._findPosition(items[i]), 0, items[i]);
    },

    sort: function() {
        if (this.length <= 1)
            return;
    
        var initial = [];
        
        for (var i = 0; i< this.length; i ++)
            initial[i] = this[i];   

        var sorted = this.__mergeSort(this);
        var sortMap = this.__getSortMapSimple(sorted);

        for(var i = 0; i < sortMap.length; i++) {
            this.splice(sortMap[i].end, 1, sortMap[i].item);
        }

        var movedItems = sortMap.select(function(sm) {return sm.item});

        //TODO: get rid of removed event
        if (movedItems.length > 0) {
            this.raise_removed({items: movedItems});
            this.raise_added(this.__createAddedArguments(movedItems));
        }
    },

    __getSortMapSimple: function(sortResult) {
        var map = [];
        
        for(var i=0; i < sortResult.length; i++) {
            if(sortResult[i] === this[i])
                continue;
                
            map.add({
                item: sortResult[i],
                begin: this.indexOf(sortResult[i]),
                end: i
            });            
        }
        
        return map;
    },

    __mergeSort: function(arr) {
        if (arr.length == 1)
            return arr;
        
        var arrSorted = [];
        var middle = Math.floor(arr.length/2);
        
        var leftArray = arr.slice(0, middle);
        var rightArray = arr.slice(middle, arr.length);
        
        leftArray =  this.__mergeSort(leftArray);
        rightArray = this.__mergeSort(rightArray);            
        
        var left = 0;
        var right = 0;

        for (var i = 0; i < leftArray.length + rightArray.length; i++) {
            if (left == leftArray.length){
                arrSorted.add(rightArray[right]);
                right++;
            } else if (right == rightArray.length){
                arrSorted.add(leftArray[left]);
                left++;
            } else if(this.__compare(leftArray[left], rightArray[right]) <= 0) {
                arrSorted.add(leftArray[left]);
                left++;
            } else {
                arrSorted.add(rightArray[right]);
                right++;
            }
        }
        return arrSorted;
    },

    _findPosition: function(item) {   
        if (this.length == 0)
            return 0;
        
        var start = 0;
        var end = this.length - 1;
        
        while( start <= end) {        
            var mid = start + Math.floor((end - start) / 2);
            
            var previous = this[mid - 1];
            var current = this[mid];
            var next = this[mid + 1];
            
            var leftCompare = previous ? this.__compare(item, previous) : 1;
            var midCompare = this.__compare(item, current);
            var rightCompare = next ? this.__compare(item, next) : -1;

            if (isUndefined(leftCompare) || isUndefined(midCompare) || isUndefined(rightCompare)) {
                throw new Error('Comparer returns undefined');
            }
            
            if(midCompare > 0) {
                if(rightCompare > 0)
                    start = mid + 1;
                else
                    return mid + 1;
            }
            else if(midCompare < 0) {
                if(leftCompare < 0)
                    end = mid - 1;
                else
                    return mid;
            }
            else
                return mid + 1;
        }
    }
};

Array.prototype.makeSorted = function(comparer) {
    if (this.__sortedArray)
       return this;

    if (!this.__observable) {
        this.makeObservable();
    }

    if (!isFunction(comparer)) {
        if (isString(comparer)) {
            comparer = (function(prop) {
                return function(h1, h2) {
                    return h1['get_' + prop]() == h2['get_' + prop]() ? 0 : (h1['get_' + prop]() > h2['get_' + prop]() ? 1 : -1);
                }
            })(comparer);
        } else {
            comparer = function(a, b) {
                return a == b ? 0 : (a > b ? 1 : -1);
            }
        }
    }

    this.__compare = comparer;
    
    Trait.Apply(this, SortedArray, {
        asOriginal: ['add']
    });

    this.__sortedArray = true;
    this.sort();
    
    return this;
};