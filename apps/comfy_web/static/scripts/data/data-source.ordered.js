OrderedDataSource = {
    set_orderBy: function(value) {        
        if (!value || !(typeof(value) === 'string') || value === '') {
            throw new Error('incorrect order property name');
        }

        this._orderByAndSortDirection = new Array();
        var oldOrderBy = this._orderBy;

        if (!isArray(value)) {
            value = value.split(',');
            
            for (var i = 0; i < value.length; i++) {
                value[i] = value[i].trim();
                valueWithOrder = value[i].split(' ');
                this._orderByAndSortDirection[i] = {
                    value: '_' + valueWithOrder[0].toCamelCase(),
                    order: valueWithOrder[1] || 'asc'
                };
            }
        }

        if (oldOrderBy === value) {
            return;
        }

        this._orderBy = value;
        this.raise_orderByChanged({ newValue: value, oldValue: oldOrderBy });
    },

    ascending: function() {
        this.set_sortDirection('asc');
        return this;
    },
    
    descending: function() {
        this.set_sortDirection('desc');
        return this;
    },

    _onLoadSuccess: function (result, context, onSuccess) {        
        this.sort();
    },

    set_sortDirection: function(direction) {
    },

    _onEntitySave: function(sender, args) {
        var toProcess = args.entities.where(function(item) { return this.contains(item);}, this );
        
        if(toProcess.length > 0)
            this.sort();
    },
    
    _onEntityLoad: function(sender, args) {
        var toProcess = args.entities.where(function(item) { return this.contains(item);}, this );
        
        if (toProcess.length > 0)
            this.sort();
    },
    
    sort: function() {
        if(this.length <= 1)
            return;
    
        var initial = [];
        
        for (var i = 0; i< this.length; i ++) {
            initial[i] = this[i];
        }

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
    
    __getSortMap: function(result) {
        var map = [];
    
        var resultPtr = 0;
        var thisPtr = 0;
        
        var seen = [];
        
        while(resultPtr < result.length && thisPtr < this.length) {
            var resItem = result[resultPtr];
            var thisItem = this[thisPtr];
            
            if(resItem === thisItem) {
                resultPtr++;
                thisPtr++;
            }
            else if(seen.contains(thisItem)) {
                thisPtr++;
            }
            else {
                map.add({
                    item: resItem,
                    begin: this.indexOf(resItem),
                    end: resultPtr
                });
                
                seen.add(resItem);
                
                resultPtr++;
            }
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
    },
    
    __switch: function(left, right) {
        var lObj = this[left];
        var rObj = this[right];
                    
        this[left] = rObj;
        this[right] = lObj;
    },

    __compare: function(left, right) {
        var res = 0;
        
        for (var i = 0; i < this._orderByAndSortDirection.length; i++) { 
            var getter = "get" + this._orderByAndSortDirection[i].value;

            if (this._orderByAndSortDirection[i].order == "desc") {
                res = this.__descCompare(getter, left, right);
            } else {
                res = this.__ascCompare(getter, left, right);
            }
                
            if (res != 0) {
                return res;
            }
        }

        return res;
    },
        
    __ascCompare: function(getter, left, right) {
        var l = left[getter](),
            r = right[getter]();

        var isDate = (l instanceof Date) || (r instanceof Date);
        var isNumber = typeof(l) == "number" || typeof(r) == "number";
        
        if (l != null && r != null && (!isNumber && !isDate)) {
            l = l.toString().toUpperCase();
            r = r.toString().toUpperCase();
        }
        
        if (l > r)
            return 1;

        if (l < r)
            return -1;
        
        return 0;
    },

    __descCompare: function(getter, left, right) {
        return -1 * this.__ascCompare(getter, left, right);
    }
};

Auto.Properties(OrderedDataSource, [
    { name: 'orderBy', autoEvent: true },
    { name: 'sortDirection', autoEvent: true }
]);

DataSource.orderBy = function(orderBy) {
    if (this.__isOrdered)
        return;

    this.__isOrdered = true;

    Trait.Apply(this, OrderedDataSource, {
        asOriginal: ['sort', '_findPosition']
    });

    this.set_orderBy(orderBy);
    this.sort();
        
    return this;
};