if (typeof(Controls) == 'undefined') {
    Controls = [];
    Controls.count = 0;
}

/**
 * Control
 * 
 * @return {Control}
 * @constructor
 * @param {Object} options
 */
var Control = function() {
    this.createControlsCollection();
    
    this.__cid = Controls.length;
    Controls[this.__cid] = this;
    Controls.count++;
    this._height = new DimensionUnit();
    this._width = new DimensionUnit();
    this._left = new DimensionUnit();
    this._top = new DimensionUnit();
    this._cssClasses = [];
    this._enabled = true;
    this._inDocumentFlow = true;
    this._level = 0;
    this.options = {};
    this._innerDelta = { width: 0, height: 0 };
    this._padding = { width: 0, height: 0 };

    this._attachedBindings = {};

    if (!this.defaultOptions || !this.defaultOptions.__initialized) {
        this.defaultOptions = Control._initDefaultOptions(Type.getInstanceType(this));
    }
    
    Auto.ApplyEventDisposing(this);
};

Control._counter = 0;
Control._times = 0;

Control._initDefaultOptions = function(type) {
    var baseType = type.get_baseType();
    var options = type.prototype.defaultOptions || {};

    while (baseType) {
        var defOptions = baseType.prototype.defaultOptions;

        if (defOptions) {
            for (var name in defOptions) {
                if (options[name] !== undefined)
                    continue;

                options[name] = defOptions[name];
            }
        }

        if (defOptions && defOptions.__initialized) {
            break;
        }
        
        baseType = baseType.get_baseType();
    }

    options.__initialized = true;

    type.prototype.defaultOptions = options;
    return options;
};

Control.__updateSortFunc = function(a, b) {
    return a._level - b._level;
};

Control.__globalUpdateQueue = new QueueProcessor(function(sender, args) {
    var controls = args.data;
    controls.sort(Control.__updateSortFunc);

    while (controls.length > 0) {
        var control = controls[0];

        if (control.isAttachedToDom()) {
            var visited = control.updateImmediately();

            for (var i in visited) {
                controls.exclude(Controls[i]);
            }

            //controls.exclude();
        } else {
            controls.exclude(control);
        }
    }
});

Control.forceUpdates = function() {
    Control.__globalUpdateQueue.force('update');
};

Control.prototype = {
    _clientWidth: null,
    _clientHeight: null,
    _position: null,
    _templateItems: null,
    _data: null,
    _inDocumentFlow: null,
    _tooltip: null,
    _level: null, // level in controls' hierarchy
    _disabledCssClass: null,
    _layoutEngine: null,
    _padding: null,
    _margin: null,
    _border: null,
    _innerDelta: null,
    _tooltipTemplate: null,
    _tooltipDelayTime: null,
    _tooltipEnabled: true,
    __tooltipObj: null,
    __tooltipTimeoutId: null,
    __tooltipPosition: null,
    
    controls: null,
    parent: null,
    id: null,
    domElement: null,
    minWidth: null,
    minHeight: null,
    maxWidth: null,
    maxHeight: null,

    get_layoutEngine: function() {
        return this._layoutEngine;
    },

    hide: function () {
        if (!this.get_visible()) {
            return;
        }

        this.set_visible(false);
        this.update();
    },

    show: function () {
        if (this.get_visible()) {
            return;
        }

        this.set_visible(true);
        this.update();
    },

    focus: function() {
        if (this.controls.length > 0) {
            this.controls.first().focus();
        } else if (this.domElement) {
            setTimeout(function() {
                try {
                    this.domElement.focus();
                } catch(e) {
                };
            }.bind(this), 0);
        }
    },

    blur: function() {
        if (this.domElement) {
            this.domElement.blur();
        }
    },

    isFocusable: function() {
        return this._focusable && (this.domElement ? DOM.isFocusable(this.domElement) : false);
    },

    findControl: function(filter) {
        var source = this.controls.clone();

        if (!isFunction(filter)) {
            throw new Error('Argument exception: filter must be a function');
        }

        while (source.length > 0) {
            var control = source.dequeue();

            if (filter(control)) {
                return control;
            }

            source.insert(0, control.controls);
        }

        return null;
    },

    // #region CSS

    addCssClass: function (value) {
        if (!value) {
            return;
        }

        var classes = value.split(' ');
        var changed = false;

        for (var i = 0; i < classes.length; i++) {
            if (!this._cssClasses.contains(classes[i])) {
                this._cssClasses.add(classes[i]);
                changed = true;
            }
        }

        if (changed) {
            this._update_cssClasses();
        }
    },

    _update_cssClasses: function () {
        if (!this.domElement)
            return;

        this.domElement.className = this._cssClasses.length > 0 ? this._cssClasses.join(' ') : '';
    },

    removeCssClass: function (value) {
        if (!value) {
            return;
        }

        var classes = value.split(' ');
        var changed = false;

        for (var i = 0; i < classes.length; i++) {
            if (this._cssClasses.contains(classes[i])) {
                this._cssClasses.remove(classes[i]);
                changed = true;
            }
        }

        if (changed) {
            this._update_cssClasses();
        }
    },

    setCssClass: function (value) {
        this._cssClasses = value ? value.split(' ') : [];
        this._update_cssClasses();
    },

    set_cssClass: function (value) {
        this._cssClasses = value ? value.split(' ') : [];
        this._update_cssClasses();
    },

    set_disabledCssClass: function (value) {
        if (isNullOrUndefined(value)) {
            return;
        }

        if (this._disabledCssClass === value)
            return;

        if (this._disabledCssClass)
            this.removeCssClass(this._disabledCssClass);

        this._disabledCssClass = value;

        if (!this._enabled) {
            this.addCssClass(this._disabledCssClass);
        }
    },

    hasCssClass: function (value) {
        return this._cssClasses.contains(value);
    },

    // #endregion

    get_childsContainer: function () {
        return this.domElement;
    },

    get_data: function () {
        if (!this._dataSync) {
            if (this.parent && !this._data) {
                var parentData = this.parent.get_data();

                if (parentData) {
                    this._data = parentData;
                }
            }
        }

        if (!this._dataSync && this._data) {
            this._dataSync = true;
            this.raise_onDataInit();
        }

        return this._data;
    },

    set_tooltip: function (value) {
        if (this._tooltip === value) {
            return;
        }

        this._tooltip = value;

        if (this.domElement) {
            this.domElement.title = value;
        }
    },

    set_tooltipEnabled: function (value) {
        this._tooltipEnabled = value;
    },

    get_tooltipEnabled: function () {
        return this._tooltipEnabled;
    }, 

    // #region Layout
    // ============================================

    isInDocumentFlow: function() {
        return this._inDocumentFlow;
    },

    isDependsOnChildSize: function() {
        return this.get_layoutEngine().isDependsOnChildSize();
    },

    isDependsOnChildWidth: function() {
        return this.get_layoutEngine().isDependsOnChildWidth();
    },

    isDependsOnChildHeight: function() {
        return this.get_layoutEngine().isDependsOnChildHeight();
    },

    get_clientWidth: function() {
        return this._visible ? this._clientWidth : 0;
    },
    
    _set_clientWidth: function(value) {
        this._clientWidth = value !== null ? Math.round(value) : null;

        if (isDefined(this.options.minWidth) && this._clientWidth < this.options.minWidth*1) {
            this._clientWidth = this.options.minWidth*1;
        }
        
        if (isDefined(this.options.maxWidth) && this._clientWidth > this.options.maxWidth*1) {
            this._clientWidth = this.options.maxWidth*1;
        }
    },

    get_clientHeight: function() {
        return this._visible ? this._clientHeight : 0;
    },
    
    _set_clientHeight: function(value) {
        this._clientHeight = value !== null ? Math.round(value) : null;

        if (isDefined(this.options.minHeight) && this._clientHeight < this.options.minHeight) {
            this._clientHeight = this.options.minHeight;
        }
        
        if (isDefined(this.options.maxHeight) && this._clientHeight > this.options.maxHeight) {
            this._clientHeight = this.options.maxHeight;
        }
    },

    get_innerWidth: function() {
        return Math.max(this._clientWidth - this._innerDelta.width, 0);
    },

    get_innerHeight: function() {
        return Math.max(this._clientHeight - this._innerDelta.height, 0);
    },

    get_outerWidth: function() {
        return this._visible ? (this._clientWidth + this.get_margin().get_width()) : 0;
    },

    get_outerHeight: function() {
        return this._visible ? (this._clientHeight + this.get_margin().get_height()) : 0;
    },

    set_width: function(value) {
        this._width = value instanceof DimensionUnit ? value : new DimensionUnit(value);
    },
    get_width: function() {
        return this._visible ? this._width : DimensionUnit.Zero;
    },

    set_height: function(value) {
        this._height = value instanceof DimensionUnit ? value : new DimensionUnit(value);
    },
    get_height: function() {
        return this._visible ? this._height : DimensionUnit.Zero;
    },

    get_domWidth: function() {
        return this.get_layoutEngine().get_domWidth();
    },

    get_domHeight: function() {
        return this.get_layoutEngine().get_domHeight();
    },

    get_padding: function() {
        return this._padding;
    },

    get_margin: function() {
        return this._margin;
    },

    get_border: function() {
        return this._border;
    },

    set_visible: function(value) {
        var oldValue = this._oldValue;

        if (this._visible == value) {
            return;
        }

        this._visible = value;
        this._updateVisible();

        this.raise_visibleChanged({ newValue: value, oldValue: oldValue });
    },

    _updateVisible: function() {
        if (!this._instantiatedInDom || !this.domElement) {
            return;
        }

        if (this._visible) {
            this.domElement.style.display = '';
        } else {
            this.domElement.style.display = 'none';
        }
    },

    update: function() {
        Control.__globalUpdateQueue.add('update', this);
    },

    postUpdate: function() {
        this.get_layoutEngine().postUpdate();
    },

    updateImmediately: function() {
        if (!this.domElement) {
            return;
        }

        return this.get_layoutEngine().update();
    },

    updateDom: function() {
        this.get_layoutEngine().updateDom();
        
        if (this.parent) {
            this.parent.get_layoutEngine().updateChildDom(this);
        }

        this.domElement.style.visibility = '';
    },

    // ============================================
    // #endregion

    initFromOptions: function (options) {
        this.options = options;

        this._applyResourceOptions(this.options);
        this._applyDefaultOptions(this.options);

        if (isFunction(options.onLoad)) {
            this.add_onLoaded(options.onLoad, this);
        }

        if (isFunction(options.onInit)) {
            this.add_initComplete(options.onInit, this);
        }

        if (isFunction(options.onFree)) {
            this.add_onFree(options.onFree, this);
        }

        if (isFunction(options.onAttached)) {
            this.add_onAttached(options.onAttached, this);
        }

        if (isFunction(options.onDataInit)) {
            this.add_onDataInit(options.onDataInit, this);
        }

        if (!isNullOrUndefined(options.width)) {
            this.set_width(new DimensionUnit(options.width));
        }

        if (!isNullOrUndefined(options.height)) {
            this.set_height(new DimensionUnit(options.height));
        }

        if (this.get_width().isNull() || this.get_height().isNull()) {
            throw new Error('Every control must have width and height');
        }

        this.addCssClass(options.cssClass || null);
        this.id = options.id || null;
        this.set_zIndex(options.zIndex || null);
        this.minWidth = options.minWidth || 0;
        this.minHeight = options.minHeight || 0;
        this.maxWidth = options.maxWidth || Infinity;
        this.maxHeight = options.maxHeight || Infinity;
        this._visible = options.visible === false ? false : true;
        this.set_enabled(isNullOrUndefined(options.enabled) ? true : options.enabled);
        this.set_disabledCssClass(options.disabledCssClass);
        this._tooltip = options.tooltip;
        this._tooltipTemplate = options.tooltipTemplate;
        this._tooltipDelayTime = options.tooltipDelayTime || 500;
        this._focusable = isDefined(options.focusable) ? options.focusable : true;

        this._layoutEngine = Nimble.Core.LayoutEngines.LayoutFactory.create(options.layout || 'stack', this);

        if (options.customFunctions)
            this._attachCustomFunctions(options.customFunctions);

        if (options.data) {
            this._data = this.initControlData(options.data);
        }

        if (options.handlers)
            this.attachHandlers(options.handlers);

        if (options.domHandlers) {
            this.attachDomHandlers(options.domHandlers);
        }

        if(this._tooltipTemplate!=null) {
            this.attachEventHandlers();
        }
        
        var controls = options.controls;
        var collection = this.controls;

        collection.eventsEnabled = false;
        
        this._initBindings('backward');

        if (controls) {
            for (var i = 0, len = controls.length; i < len; i++) {
                var controlItem = controls[i];
                // I HATE IE: in IE [ {}, {}, ].length === 3, but the third element is undefined
                if (!controlItem) {
                    continue;
                }

                if (!controlItem.hasOwnProperty("type")) {
                    throw new Error('[Control]: Undefined type for a control');
                }

                var control = ControlsFactory.create(controlItem.type);
                control.id = controlItem.id;
                control.parent = this;
                control._level = this._level + 1;
                collection.add(control);

                control.initFromOptions(controlItem);
                
                this._attachBackwardHandlers(control);
            }
            this._childsTreeLoaded();
        }

        collection.eventsEnabled = true;

        //TODO: This method is for 'template' initializations - should be removed
        this._postInit();
    },

    __updateTooltipPosition: function(sender, args) {
        if(this.__tooltipPosition == null) {
            this.__tooltipPosition = {x: args.clientX, y: args.clientY};
        } else {
            this.__tooltipPosition.x = args.clientX;
            this.__tooltipPosition.y = args.clientY;
        }
    },

    __clearTooltipTimeoutId: function() {
        if(this.__tooltipTimeoutId != null) {
            clearTimeout(this.__tooltipTimeoutId);
            this.__tooltipTimeoutId = null;
        }
    },

    _createTooltipWithDelay: function(sender, args) {
        this.__clearTooltipTimeoutId();
        this.__tooltipPosition = {x: args.clientX, y: args.clientY};
        this.__tooltipTimeoutId = setTimeout(function() {
                this._createTooltip(sender, args);
            }.bind(this),this._tooltipDelayTime);
    },
    
    _createTooltip: function(sender, args) {
        if(this._tooltipTemplate != null && this.__tooltipObj == null && this._tooltipEnabled) {
            this.__tooltipObj = new Phoenix.UI.Tooltip();
            this.__tooltipObj.initFromOptions({ template: this._tooltipTemplate });
            if(this._tooltipTemplate != null) {
                this.__tooltipObj.controls[0].set_dataSource(this.get_dataSource());
            }
            this.__tooltipObj.set_position({x: this.__tooltipPosition.x + 16, y: this.__tooltipPosition.y + 16});
            this.controls.add(this.__tooltipObj);
            this.__tooltipObj.open();  
        }
    },

    _destroyTooltip: function() {
        this.__clearTooltipTimeoutId();
        this.__tooltipPosition = null;
        
        if (this.__tooltipObj != null) {
            this.controls.remove(this.__tooltipObj);
            this.__tooltipObj.free();
            this.__tooltipObj = undefined;
        }
    },
    
    _childsTreeLoaded: function() {
    },

    _postInit: function() {
        this._initDataSourceMethods();
        this._initBindings('forward');
        this.attachDataHandlers();
        this.attachHistoryHandlers();
        this.raise_initComplete();
    },

    _applyDefaultOptions: function (options) {
        var defaultOptions = this.defaultOptions;

        if (!defaultOptions) {
            return;
        }

        for (var name in defaultOptions) {
            if (options[name] !== undefined)
                continue;

            options[name] = defaultOptions[name];
        }
    },

    _applyResourceOptions: function(options) {
        if (!options.resource) {
            return;
        }

        var resource = Application.get_resource(options.resource);

        if (!resource) {
            console.log('Unknown resource: ' + options.resource);
            return;
        }

        for (var name in resource) {
            if (options[name] !== undefined)
                continue;

            options[name] = resource[name];
        }
    },

    _attachCustomFunctions: function (functionsObject) {
        for (var funcName in functionsObject) {
            if (!isFunction(functionsObject[funcName]))
                throw new Error('Member "' + funcName +'" expected to be a function');

            if (this[funcName])
                throw new Error('Function "' + funcName +'" already exists');

            this[funcName] = functionsObject[funcName].bind(this);
        }
    },

    initControlData: function (dataOptions) {
        if (!dataOptions) {
            return;
        }

        var data = {};

        for (var i = 0; i < dataOptions.length; i++) {
            var item = dataOptions[i];

            if (item.name) {
                Auto.Property(data, { name: item.name, autoEvent: true });
                data['set_' + item.name](item.value);

                if (item.saveHistory) {
                    History.Observer.init(item.name, item.type, data, this.$get_page());
                }                

            } else {
                Auto.Property(data, { name: item, autoEvent: true });
            }
        }

        this.add_onFree(function () {
            if (data && data.dispose) {
                data.dispose();
            }
        }, this);

        return data;
    },

    attachDataHandlers: function() {
        if (!this.options)
            return;

        var window = this.get_window();
        var data = window ? window.get_data() : null;

        if(data)
            this._attachForwardHandlers(data, true, false);
    },

    detachDataHandlers: function() {
        var window = this.get_window();

        if (window) {
            var data = window.get_data();
            if(data)
                this._detachForwardHandlers(data, true);
        }
    },

    attachHistoryHandlers: function() {
        if (!this.options)
            return;

        var window = (this instanceof Page) ? this : this.$get_page();

        if (window)
            this._attachForwardHandlers(window, false, true);
    },

    detachHistoryHandlers: function() {
        var window = this.get_window();

        if (window) {
            var data = window.get_data();
            if(data)
                this._detachForwardHandlers(data, true);
        }
    },

    set_dataSource: function (value) {
        if (value === this._dataSource)
            return;

        if (this._dataSource)
            this._detachForwardHandlers(this._dataSource);
        
        var oldValue = this._dataSource;
        this._dataSource = value;
        
        if (this._dataSource)
            this._attachForwardHandlers(value, false, false);

        this.raise_dataSourceChanged({newValue: value, oldValue: oldValue});
    },

    set_enabled: function (value) {
        var oldValue = this._enabled;

        if (this._enabled === value)
            return false;

        this._enabled = value;

        if (this._disabledCssClass) {
            if (!value) {
                this.addCssClass(this._disabledCssClass);
            } else {
                this.removeCssClass(this._disabledCssClass);
            }
        }

        this.raise_enabledChanged({ newValue: value, oldValue: oldValue });
    },

    attachHandlers: function (handlers) {
        for (var name in handlers) {
            if (handlers.hasOwnProperty(name)) {
                var attachMethod = this['add_' + name];

                if (!attachMethod || !isFunction(attachMethod))
                    throw new Error('"' + name + '" event not found in control');

                if (!isFunction(handlers[name])) {
                    throw new Error('"' + name + '" handler must be a function');
                }

                attachMethod.bind(this)(handlers[name], this);
            }
        }
    },

    // #region Handling events 
    attachEventHandlers: function() {
        this.attachDomHandler('mousemove', this.__updateTooltipPosition);
        this.attachDomHandler('mouseover', this._createTooltipWithDelay);
        this.attachDomHandler('mouseout', this._destroyTooltip);
    },

    detachEventHandlers: function() {
        this.detachDomHandler('mousemove', this.__updateTooltipPosition);
        this.detachDomHandler('mouseover', this._createTooltipWithDelay);
        this.detachDomHandler('mouseout', this._destroyTooltip);
    },
    // #endregion

    // #region DOM Handlers
    
    attachDomHandlers: function (eventHandlersCollection) {
        if (!eventHandlersCollection)
            return;

        for (var eventName in eventHandlersCollection) {
            this.attachDomHandler(eventName, eventHandlersCollection[eventName]);
        }
    },

    attachDomHandler: function (eventName, handler) {
        if (!handler || !isFunction(handler))
            throw new Error('Dom event handler must be specified.');

        if (!this['add_' + eventName]) {
            Auto.Event(this, eventName);

            if (!this.__domEvents)
                this.__domEvents = [];

            if (!this.__domEvents.contains(eventName)) {
                this.__domEvents.add(eventName);

                if (this.domElement)
                    jQuery(this.domElement).bind(eventName,
                        function (args) {
                            this['raise_' + eventName](args);
                        } .bind(this)
                    );
            }
        }

        this['add_' + eventName](handler, this);
    },

    detachDomHandler: function (eventName, handler) {
        this['remove_' + eventName](handler);
    },

    initializeDomEvents: function () {
        if (!this.__domEvents)
            return;

        var that = this;

        for (var i = 0; i < this.__domEvents.length; i++) {
            jQuery(this.domElement).bind(this.__domEvents[i],
                function (eName) {
                    return function (args) {
                        this['raise_' + eName](args);
                    } .bind(that);
                } (this.__domEvents[i])
            );
        }
    },

    // #endregion

    // #region Bindings

    __parseSourceNames: function(sourceName) {
        var names = sourceName.split(','),
            result = [];

        for (var i = 0; i < names.length; i++) {
            result.add(names[i].trim());
        }

        return result;
    },

    _initBindings: function(bindingType) {
        if(!this._bindingHandlers)
            this._bindingHandlers = {
                forward: [],
                backward: []
            };

        if (!this.options.bindings)
            return;

        var funcName;
        
        if(bindingType === 'forward')
            funcName = '_initForwardBindingHandlers';
        else if(bindingType === 'backward')
            funcName = '_initBackwardBindingHandlers';
        else
            throw new Error('unknown binding initialization type');
        
        for (var sourceName in this.options.bindings) {
            var names = this.__parseSourceNames(sourceName);
            
            for (var i = 0; i < names.length; i++) {
                this[funcName](names[i], this.options.bindings[sourceName]);
            }
        }
    },

    _initForwardBindingHandlers: function(bindingSource, binding) {
        var handlers = [],
            isGlobal = false,
            isHistory = false,
            source = bindingSource,
            globalParts = bindingSource.split(':');

        // check if current binding is global
        if (globalParts.length == 2 && globalParts[0] == 'data') {
            isGlobal = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('Global data binding on * is not allowed');
            }
        }

        // check if current binding is history
        if (globalParts.length == 2 && globalParts[0] == 'history') {
            isHistory = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('History data binding on * is not allowed');
            }
        }

        // if target binding is an array then attach binding for each items of it
        if (binding instanceof Array) {
            for (var i = 0; i < binding.length; i++) {
                if (isDefined(binding[i])) {
                    handlers.add(this._getForwardHandlerFor(binding[i]));
                }
            }
        } else if (isDefined(binding)) {
            handlers.add(this._getForwardHandlerFor(binding));
        }

        if (!isHistory && this._tryInitComplexBinding(bindingSource, handlers, isGlobal)) {
            return;
        }

        var length = this._bindingHandlers.forward.length;
        this._bindingHandlers.forward[length] = source;
        this._bindingHandlers.forward[length + 1] = handlers;

        if (bindingSource === '*') {
            for (var i = 0; i < handlers.length; i++) {
                this.add_dataSourceChanged(handlers[i], this);
            }
        }
    },

    _tryInitComplexBinding: function(binding, handlers, isGlobal) {
        var parts = binding.split('.');

        if (parts.length < 2) {
            return false;
        }

        var thisObj = this;

        var source = parts[0];
        var event = parts[1];

        if(isGlobal && source === '*')
            throw new Error('Global data binding on * is not allowed');

        var handler = function(sender, args) {
            if (args.oldValue) {
                for(var i = 0; i < handlers.length; i++)
                    args.oldValue['remove_' + event](handlers[i], thisObj);
            }            
            
            if (args.newValue) {
                for(var i = 0; i < handlers.length; i++) {
                    args.newValue['add_' + event](handlers[i], thisObj);
                }
            }
        };
        handler._isComplex = true;
        
        if(source === '*')
            this.add_dataSourceChanged(handler, this);
        else if(source !== '*') {
            var length = this._bindingHandlers.forward.length;
            this._bindingHandlers.forward[length] = !isGlobal ? source : 'data:' + source;
            this._bindingHandlers.forward[length + 1] = [handler];
        }
        
        return true;
    },

    _initBackwardBindingHandlers: function(bindingSource, binding) {
        if (bindingSource.split('.').length > 1) {
            return;
        }

        var isGlobal = false,
            isHistory = false;

        var globalParts = bindingSource.split(':');
        if (globalParts.length == 2 && globalParts[0] == 'data') {
            isGlobal = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('Global data binding on * is not allowed');
            }
        }

        if (globalParts.length == 2 && globalParts[0] == 'history') {
            isHistory = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('History data binding on * is not allowed');
            }
        }

        var trackingProps = [];
        var backwardHandler = this._getBackwardHandlerFor(bindingSource, isGlobal, isHistory);

        if (binding instanceof Array) {
            for (var i = 0; i < binding.length; i++) {
                if (typeof binding[i] === 'string') {
                    trackingProps.add(binding[i]);
                } 
            }        
        } else if (typeof binding === 'string') {
            trackingProps.add(binding);
        }

        for (var i = 0; i < trackingProps.length; i++) {
            if (!this._checkAndAttachHandlerToSelf(trackingProps[i], backwardHandler)) {
                var length = this._bindingHandlers.backward.length;
                
                this._bindingHandlers.backward[length] = trackingProps[i];
                this._bindingHandlers.backward[length + 1] = backwardHandler;
            }
        }
    },

    _checkAndAttachHandlerToSelf: function(prop, handler) {
        if(this['add_' + prop + 'Changed'] && isFunction(this['add_' + prop + 'Changed'])) {
            this['add_' + prop + 'Changed'](handler, this);
            return true;
        }

        if(this[prop] && isFunction(this[prop]))
            return true;

        return false;
    },

    _getForwardHandlerFor: function(bindingTarget) {
        var thisObj = this;

        if(isFunction(bindingTarget))
            return bindingTarget;
        
        if(typeof bindingTarget !== 'string')
            throw Error('Unknow binding type: ' + bindingTarget);

        if(bindingTarget === '*')
            return function Binding$forwardHandler$toAllChilds(sender, args) {
                var controls = thisObj.controls;

                for (var i = 0; i < controls.length; i++) {
                    var control = controls[i];
                    control.set_dataSource(args.newValue);
                }
            };

        if(this['set_' + bindingTarget] && isFunction(this['set_' + bindingTarget]))
            return function Binding$forwardHandler$property(sender, args) { thisObj['set_' + bindingTarget](args.newValue); };

        var target = this[bindingTarget];

        if(isFunction(target))
            return target;
        else if (Control.isInstance(target))
            return function Binding$forwardHandler$child$toDataSource (sender, args) { thisObj[bindingTarget].set_dataSource(args.newValue); };
        else if (target !== undefined)
            return function Binding$forwardHandler$child$toField (sender, args) { thisObj[bindingTarget] = args.newValue; };
        else
            return function Binding$forwardHandler$lazy$child$toDataSource (sender, args) {
                var target = thisObj[bindingTarget];

                if (Control.isInstance(target)) {
                    target.set_dataSource(args.newValue);
                } else if (isUndefined(target)) {
                    throw new Error('Undefined property ' + bindingTarget);
                }
            };
    },

    _getBackwardHandlerFor: function(prop, isGlobal, isHistory) {
        if(typeof prop !== 'string')
            throw Error('Unknown property binding type');
        
        if(prop === '*')
            return function Binding$backwardHandler$this$dataSource(sender, args) {
                this.set_dataSource(args.newValue);
            };

        if (isGlobal) {
            return function Binding$backwardHandler$data$property(sender, args) {
                var ds;

                if (this instanceof Page) {
                    ds = this.get_data();
                } else {
                    ds = this.get_window().get_data();
                }

                if(!ds)
                    return;

                ds['set_' + prop](args.newValue);
            };
        }
        
        if (isHistory) {
            return function Binding$backwardHandler$history$property(sender, args) {
                var page;
                
                if (this instanceof Page) {
                    page = this;
                } else {
                    page = this.$get_page();
                }

                page.set_param(prop, args.newValue);
            };
        }

        return function Binding$backwardHandler$dataSourcec$propertyOrField(sender, args) {
            var ds = this.get_dataSource();

            if(!ds)
                return;

            if(ds['set_' + prop] && isFunction(ds['set_' + prop]))
                ds['set_' + prop](args.newValue);
            else
                ds[prop] = args.newValue;
        };
    },

    _attachForwardHandlers: function(object, globalMode, historyMode) {
        for (var i = 0; i < this._bindingHandlers.forward.length; i+=2) {
            var bindingSource = this._bindingHandlers.forward[i];
            
            if (bindingSource === '*') {
                continue;
            }

            if (globalMode) {
                var splitted = bindingSource.split(':');

                if (splitted.length != 2 || splitted[0] != 'data')
                    continue;

                bindingSource = splitted[1];
            }

            if (historyMode) {
                var splitted = bindingSource.split(':');

                if (splitted.length != 2 || splitted[0] != 'history')
                    continue;

                bindingSource = splitted[1];
            }

            if (object) {
                if (historyMode) {
                    var $event = function (callback, thisObj) {
                        var onParamChanged = (function(bSource) {
                            return function(sender, args) {
                                if (args.key === bSource) {
                                    callback.bind(thisObj)(sender, args);
                                }
                            };
                        })(bindingSource);

                        object.add_onParamChanged(onParamChanged);
                    };
                    
                    value = object.get_param(bindingSource);
                } else {
                    var $event = object['add_' + bindingSource + 'Changed'],
                        getter = object['get_' + bindingSource],
                        value = getter ? getter.call(object) : object[bindingSource];
                }
                
                var defined = !isUndefined(value),
                    handlers =  this._bindingHandlers.forward[i+1];

                for (var k = 0; k < handlers.length; k++) {
                    if ($event) {
                        $event.call(object, handlers[k], this);
                    }

                    if (defined) {
                        handlers[k].bind(this)(object, { newValue: value, oldValue: undefined });
                    }
                }
            } else {
                var handlers =  this._bindingHandlers.forward[i+1];

                for(var k = 0; k < handlers.length; k++) {
                    handlers[k].bind(this)(null, {newValue: null, oldValue: undefined});
                }
            }
        }
    },

    _attachBackwardHandlers: function(control) {        
        if (!control.id)
            return;        

        for (var i = 0; i < this._bindingHandlers.backward.length; i+=2) {
            if(this._bindingHandlers.backward[i] !== control.id)
                continue;

            control.add_dataSourceChanged(this._bindingHandlers.backward[i+1], this);
        }
    },

    _detachForwardHandlers: function(object, globalMode) {
        for(var i = 0; i < this._bindingHandlers.forward.length; i+=2) {
            var bindingSource = this._bindingHandlers.forward[i];
            
            if(bindingSource === '*')
                continue;

            if (globalMode) {
                var splitted = bindingSource.split(':');
                if(splitted.length <= 1)
                    continue;

                bindingSource = splitted[1];
            }

            var $event = object['remove_' + bindingSource + 'Changed'];
            
            if(!$event || !isFunction($event))
                continue;
            
            var handlers =  this._bindingHandlers.forward[i+1];

            for(var k=0; k < handlers.length; k++) {
                if(handlers[k]._isComplex)
                    handlers[k](this, {newValue: undefined, oldValue: object['get_' + bindingSource]()});

                object['remove_' + bindingSource + 'Changed'](handlers[k], this);
            }
        }
    },

    _detachBackwardHandlers: function(control) {
        if(!control.id)
            return;        

        for(var i = 0; i < this._bindingHandlers.backward.length; i+=2) {
            if(this._bindingHandlers.backward[i] !== control.id)
                continue;

            control.remove_dataSourceChanged(this._bindingHandlers.backward[i+1], this);
        }
    },

    _initDataSourceMethods: function () {
        if (this.options.setter) {
            var setter = this.options.setter;

            this.set_dataSource = function (value) {
                if (this._dataSource === value)
                    return;

                var oldVal = this._dataSource;

                setter.bind(this)(value);

                if (oldVal === this._dataSource)
                    this._dataSource = value;
            };
        }

        if (this.options.getter)
            this.get_dataSource = this.options.getter;
    },

    // #endregion

    // #region DOM

    isAttachedToDom: function() {
        return this.domElement && this._instantiatedInDom && !this._detachedFromDom;
    },

    // attach control in DOM to @domElement
    attachToDom: function(domElement) {
        if (!this._instantiatedInDom) {
            this.instantiateInDom(domElement);
        } else if (this._detachedFromDom) {
            DOM.appendChild(this.domElement, domElement);
            this._detachedFromDom = false;
        }

        this.raise_onAttached();
    },

    _calculateInnerDelta: function() {
        this._innerDelta.width = this._padding.get_width() + this._border.get_width();
        this._innerDelta.height = this._padding.get_height() + this._border.get_height();
    },

    // instantiate control into @domElement
    instantiateInDom: function (domElement) {
        this.set_instantiatedInDom(true);
        var element = this.domElement;
        var style = element.style;
        style.visibility = 'hidden';

        if (element) {
            if (this._cssClasses.join('')) {
                this._update_cssClasses();
            }

            if (this._tooltip) {
                element.title = this._tooltip;
            }

            this._padding = new Rect(this.options.padding);
            this._margin = new Rect(this.options.margin);
            this._border = new Rect(this.options.border);
            
            //style.cssText += 'padding: ' + this._padding.toString() + ' !important; margin: ' + this._margin.toString() + ' !important; border-width: ' + this._border.toString() + ' !important;';
            style.padding = this._padding.toString();
            style.margin = this._margin.toString();
            style.borderWidth = this._border.toString();
            
            this._calculateInnerDelta();
        }

        if (this.controls.length > 0) {
            this.instantiateChildsInDom();
        }

        this.initializeDomEvents();

        if (!this._visible) {
            this._updateVisible();
        }

        this.onLoad();
    },

    onLoad: function() {
        this.raise_onLoaded();
    },

    /**
    * Instantiate child elements in some domElement
    * @param {DOMElement} domElement
    */
    instantiateChildsInDom: function () {
        var controls = this.controls;

        for (var i = 0, len = controls.length; i < len; i++) {
            this.instantiateChildInDom(controls[i]);
        }
    },

    /**
    * Instantiate a child element in DOM
    * @param {Control} control
    * @param {String} type of instantiating (child, after, before)
    * @param {Control} reference control (if type is after or before)
    * @param {DomElement} dom container for the child
    */
    instantiateChildInDom: function (control, type, refControl, container) {
        var domElement = container || this.get_childsContainer();

        control.attachToDom(domElement);

        if ((type == 'after' || type == 'before') && refControl) {
            var refDom = refControl.domElement;
            var cDom = control.domElement;

            if (refDom && cDom && ((type == 'after' && cDom.previousSibling !== refDom) || (type == 'before' && cDom.nextSibling !== refDom)) && refDom.parentNode === cDom.parentNode) {
                DOM.insert(type, refDom, cDom);
            }
        }
    },

    /**
    * Remove a child element from DOM
    */
    removeChildFromDom: function (control) {
        if (control._instantiatedInDom) {
            control.detachFromDom();
            //control.removeFromDom();
        }
    },

    removeFromDom: function () {
        // remove dom element
        var element = this.domElement;

        if (element) {
            if (!this.parent || !this.parent.__free) {
                DOM.remove(element);
            }

            this.domElement = null;
        }

        this.set_instantiatedInDom(false);
    },

    detachFromDom: function() {
        this._clientWidth = null;
        this._clientHeight = null;

        if (this._instantiatedInDom) {
            var element = this.domElement;
            DOM.detachNode(this.domElement);
            this._detachedFromDom = true;
        }

        //this.detachDataHandlers();
    },

    // #endregion


    // disposes the control
    free: function () {
        if (this.__free) {
            return;
        }

        // destroy tooltip
        this._destroyTooltip();

        if (this._dataSource)
            this._detachForwardHandlers(this._dataSource);
        
        this.__free = true;
        this.raise_onFree();

        if(this._tooltipTemplate != null) {
            this.detachEventHandlers();
        }

        this.detachDataHandlers();
        this.detachHistoryHandlers();

        // free childs
        var childs = this.controls;
        childs.eventsEnabled = false;

        if (childs) {
            for (var i = childs.length - 1; i >= 0; i--) {
                var control = childs[i];
                childs[i].free();
            }

            childs.clear();
        }

        this.removeFromDom();
        this._data = null;
        this._dataSource = null;
        this._templateItems = null;
        this.options = null;

        this.clearEvents();

        Controls.count--;
        delete Controls[this.__cid];
    },

    get_window: function () {
        var parent = this.parent || this;
        
        var parentType = Type.getInstanceType(parent);

        if (parentType === Type.getType('Page') || parentType === Type.getType('Phoenix.UI.Popup'))
            return parent;

        return parent === this ? null : parent.get_window();
    },

    $get_page: function () {
        var parent = this.parent || this;
        
        var parentType = Type.getInstanceType(parent);

        if (parentType === Type.getType('Page'))
            return parent;

        return parent === this ? null : parent.$get_page();
    },

    // #region Child collections
    // ==================================

    _childAdded: function (sender, args) {
        var control = args.control;

        if (control.parent && control.parent !== this) {
            control.parent.controls.remove(control);
        }

        control.parent = this;
        control._level = this._level + 1;

        if (this._instantiatedInDom) {
            var container = document.createDocumentFragment();

            if (!args.nextControl) {
                this.instantiateChildInDom(control, null, null, container);
            } else {
                if (args.prevControl) {
                    this.instantiateChildInDom(control, 'after', args.prevControl, container);
                } else {
                    this.instantiateChildInDom(control, 'before', args.nextControl, container);
                }
            }

            this.get_childsContainer().appendChild(container);
        }

        this._attachBackwardHandlers(control);
    },

    _childRemoved: function (sender, args) {
        var control = args.control;

        this.removeChildFromDom(control);
        control.parent = null;
        
        this._detachBackwardHandlers(control);
     },
     
    _childsCollectionModified: function () {
        this.update();
    },
    
    appendControl: function (control) {
        this.controls.add(control);
    },

    createControlsCollection: function () {
        this.controls = [].makeControlsCollection(this);
        this.controls.add_added(this._childAdded, this);
        this.controls.add_removed(this._childRemoved, this);
        this.controls.add_modified(this._childsCollectionModified, this);
    },

    // ==================================
    // #endregion

    toString: function () {
        return Type.getInstanceType(this).get_name() + " #" + this.__cid +
            (this.id ? ", Id: " + this.id : "") +
            (this.controls ? ", Childs count: " + this.controls.length : "");
    },

    attachEventsFromOptions: function(options, eventsList) {
        if (!isArray(eventsList)) {
            throw new Error('Array exprected');
        }

        for (var i = 0; i < eventsList.length; i++) {
            var eventName = eventsList[i];

            if (!this['add_' + eventName]) {
                throw new Error('Event with name "' + eventName + '" have not been found');
            }

            if (options[eventName] && isFunction(options[eventName])) {
                this['add_' + eventName](options[eventName], this);
            }
        }
    }
};

Auto.Events(Control.prototype, [
    'initComplete',
    'onLoaded',
    'onAttached',
    'onFree',
    'onDataInit'
]);

Auto.Properties(Control.prototype, [
    { name: 'instantiatedInDom', defaultValue: false },
    { name: 'left' }, // dimension unit
    { name: 'top' }, // dimension unit
    { name: 'cssClass' },
    { name: 'dataSource', autoEvent: true },
    { name: 'zIndex' },
    { name: 'enabled', autoEvent: true },
    { name: 'visible', autoEvent: true },
    { name: 'tooltip' },
    { name: 'tooltipTemplate' },
    { name: 'tooltipDelayTime'},
    { name: 'disabledCssClass' }
]);

Control.createClass('Control');