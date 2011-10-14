if (!window.Auto)
    Auto = {};

Object.extend(Auto, {
    Event: function(object, eventName) {
        object['raise_' + eventName] = function Auto$Event$raise_event(args, context) {
            if(!this.__events ||
               !this.__events[eventName] || 
                this.__events[eventName].__handlers.length == 0)
                return;
        
            var sender = this;
            var handlers = this.__events[eventName].__handlers;
            
            for (var i = 0; i < handlers.length; i+=2) {
                var handler = handlers[i];
                var thisObj = handlers[i+1];
                
                var func = thisObj ? handler.bind(thisObj) : handler;
                func(sender, args, context);
            }
        };
        
        object['add_' + eventName] = function Auto$Event$add_event(handler, thisObj) {
            if (!this.__events) {
                this.__events = {};
            }

            if(!this.__events[eventName]) {
                this.__events[eventName] = {
                    __handlers: []
                };
            }
            
            var handlers = this.__events[eventName].__handlers;
            handlers.addItem(handler);
            handlers.addItem(thisObj);
        };

        object['remove_' + eventName] = function Auto$Event$remove_event(handler, thisObj) {
            if (!this.__events || !this.__events[eventName]) {
                return;
            }
            
            var handlers = this.__events[eventName].__handlers;
            var newHandlers = [];
            
            for (var i = 0; i < handlers.length; i+=2) {
                if (handlers[i] !== handler || (thisObj && handlers[i+1] !== thisObj)) {
                    newHandlers.addItem(handlers[i]);
                    newHandlers.addItem(handlers[i+1]);
                }
            }
            
            this.__events[eventName].__handlers = newHandlers;
        };
    },
    
    Events: function(object, events) {
        for(var i = 0; i < events.length; i ++)
            this.Event(object, events[i]);
    },
    
    ApplyEventDisposing: function ApplyEventDisposing(target) {
        if(target.dispose && target.dispose.__eventDisposing)
            return;
        
        Trait.Apply(target, {
            dispose: function() {
                var events = this.__events;
                if (events) {
                    for (var prop in events) {
                        if (events.hasOwnProperty(prop)) {
                            if (events[prop] && isArray(events[prop].__handlers)) {
                                events[prop].__handlers.clear();
                            }
                            delete events[prop];
                        }
                    }
                    delete this.__events;
                }
            },
            clearEvents: function() {
                if(this.__events)
                    delete this.__events;
            }
        });
        
        target.dispose.__eventDisposing = true;
    }
});