var Rect = function(s) {
    this.left = 0;
    this.top = 0;
    this.right = 0;
    this.bottom = 0;

    if (isDefined(s) && isString(s)) {
        this.parse(s);
    }
};

Object.extend(Rect.prototype, {
    left: null,
    top: null,
    right: null,
    bottom: null,

    get_width: function() {
        return this.right + this.left;
    },
    
    get_height: function() {
        return this.top + this.bottom;
    },

    parse: function(value) {
        var reg = /(?:(\-?[0-9]{1,2})(?:px)?)(?:\s|$)/ig;

        var arr = [];

        while (args = reg.exec(value)) {
            arr.add(args[1]);
        }

        if (arr.length == 1) {
            arr[3] = arr[2] = arr[1] = arr[0];
        } else if (arr.length == 2) {
            arr[3] = arr[1];
            arr[2] = arr[0];
        } else if (arr.length == 3) {
            arr[3] = arr[1];
        }

        if (arr.length == 4) {
            this.left = arr[0]*1, this.top = arr[1]*1, this.right = arr[2]*1, this.bottom = arr[3]*1;
        }
    },
    
    toString: function() {
        return "{0}px {1}px {2}px {3}px".format(this.top, this.right, this.bottom, this.left);
    }
});