var QueueProcessor = function(callback) {
    if (callback) {
        this.add_processed(callback, this);
    }

    this._queueList = {};
    this._queueTimeouts = {};
};

QueueProcessor.prototype = {
    _queueList: null,
    _queueTimeouts: null,

    add: function(queueId, args) {
        if (isNullOrUndefined(args)) {
            throw new Error('Args is null or undefined');
        }

        if (!this._queueList[queueId]) {
            this._queueList[queueId] = [];
        }
        
        this._queueList[queueId].add(args);
        this._checkQueue(queueId);
    },

    clear: function() {
        for (var timeoutId in this._queueTimeouts) {
            if (this._queueTimeouts.hasOwnProperty(timeoutId)) {
                clearTimeout(this._queueTimeouts[timeoutId]);
            }
        }

        this._queueList = {};
        this._queueTimeouts = {};
    },

    force: function(queueId) {
        this._clearTimeout(queueId);
        var queue = this._queueList[queueId];
            
        if (!queue || queue.length === 0) {
            return;
        }
            
        this._queueList[queueId] = [];
            
        this.raise_processed({ queueId: queueId, data: queue });
    },

    _clearTimeout: function(queueId) {
        if (this._queueTimeouts[queueId]) {
            clearTimeout(this._queueTimeouts[queueId]);
            this._queueTimeouts[queueId] = null;
        }
    },

    __getTimeoutFunction: function(queueId) {
        return function() {
            this.force(queueId);
        }.bind(this);
    },
    
    _checkQueue: function(queueId) {
        this._clearTimeout(queueId);
        this._queueTimeouts[queueId] = setTimeout(this.__getTimeoutFunction(queueId), 0);
    }
};

Auto.Events(QueueProcessor.prototype, [
    'processed'
]);

QueueProcessor.createClass('QueueProcessor');