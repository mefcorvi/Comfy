Type.createNamespace('Phoenix.UI');

Phoenix.UI.Tree = function(options) {
    Phoenix.UI.Tree.constructBase(this);
    this._level = 0;
};

Phoenix.UI.Tree.prototype = {
    _level: null,

    defaultOptions: {
        'width': '*',
        'height': '?'
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('UL');
        domElement.appendChild(this.domElement);

        Phoenix.UI.Tree.callBase(this, "instantiateInDom", [domElement]);
    },

    initFromOptions: function (options) {
        Phoenix.UI.Tree.callBase(this, "initFromOptions", [options]);

        if (isFunction(options.onInnerTreeOptionsCreated)) {
            this.add_onInnerTreeOptionsCreated(options.onInnerTreeOptionsCreated, this);
        }

        if (!options.nodeTemplate) {
            options.nodeTemplate = {
                /*bindings: {
                },*/
                controls: [
                    {
                        id: 'nodeText',
                        width: '*',
                        type: 'label'
                    }
                ]
            };
        }

        this.addCssClass('treeview');

        if (this._level > 0) {
            this.addCssClass('treeview_' + this._level);
        }

        if (!(options.nodeTemplate instanceof Array))
            options.nodeTemplate = [options.nodeTemplate];

        if (options.nodeTemplate[0].type && options.nodeTemplate[0].type !== 'treeNode') {
            throw new Error('You cannot change type of a node template');
        }

        if (!options.nodeTemplate[0].bindings) {
            var bindingProp = options.textProperty || '*';
            options.nodeTemplate[0].bindings = {};
            options.nodeTemplate[0].bindings[bindingProp] = '*';
        }

        options.nodeTemplate[0].type = "treeNode";
        options.nodeTemplate[0].id = "node";

        if (options.emptyDataTemplate) {
            if (!(options.emptyDataTemplate instanceof Array)) {
                options.emptyDataTemplate = [options.emptyDataTemplate];
            }

            this._emptyDataTemplate = new Template(options.emptyDataTemplate[0]);
        } else {
            this.options.emptyDataTemplate = [];
        }

        this._textProperty = options.textProperty;
        this.set_nodeTemplate(new Template(options.nodeTemplate[0]));
        this.set_childProperty(options.childProperty);
    },

    createItem: function (dataItem) {
        var li = ControlsFactory.create('container');
        var shouldCreateInnerTree = this.get_childProperty() && Object.getPropertyValue(dataItem, this.get_childProperty());

        var bindings = {};
        bindings['*'] = 'node';

        if (shouldCreateInnerTree) {
            bindings[this.get_childProperty()] = function (sender, args) {
                this.innerTree.set_dataSource(args.newValue);

                if (this.node) {
                    this.node._innerTreeModified();
                }
            };
            bindings[this.get_childProperty() + '.changed'] = function (sender, args) { // TODO: sometimes datasource wouldn't contain "childProperty".changed 
                if (this.node) {
                    this.node._innerTreeModified();
                }
            };
        }

        li.initFromOptions({
            tag: 'LI',
            cssClass: 'tree_node tree_node_' + this._level,
            bindings: bindings,
            width: '100%',
            height: '?',
            onFree: function () {
                this.parentNode = null;
            }
        });

        var control = this._nodeTemplate.instantiate();
        control.parentNode = this.parent;
        li.appendControl(control);

        if (shouldCreateInnerTree) {
            var innerTree = this._createInnerTree();

            if (innerTree) {
                li.appendControl(innerTree);

                if (control instanceof Phoenix.UI.TreeNode) {
                    control.addCssClass('tree_node_' + this._level);
                    control.set_innerTree(innerTree);
                }
            }
        }

        this.get_childsHash().put(dataItem, li);

        li.set_dataSource(dataItem);

        return li;
    },

    _createInnerTree: function () {
        var tree = ControlsFactory.create('tree');
        tree._level = this._level + 1;

        var innerTreeOptions = {
            id: 'innerTree',
            width: '*',
            height: '?',
            margin: '10 0 0 0',
            cacheDisabled: this.options.cacheDisabled,
            childProperty: this._childProperty,
            textProperty: this._textProperty,
            nodeTemplate: (this.options.nodeTemplate.length == 1) ? this.options.nodeTemplate[0] : this.options.nodeTemplate.slice(1),
            emptyDataTemplate: (this.options.emptyDataTemplate.length > 1) ? this.options.emptyDataTemplate.slice(1) : undefined
        };

        this.raise_onInnerTreeOptionsCreated({ options: innerTreeOptions });

        tree.initFromOptions(innerTreeOptions);

        return tree;
    },

    free: function () {
        this.clear();
        this.__emptyDataControl = null;

        Phoenix.UI.Tree.callBase(this, 'free');
    }
};

Auto.Events(Phoenix.UI.Tree.prototype, [
    'onClick',
    'onInnerTreeOptionsCreated'
]);

Auto.Properties(Phoenix.UI.Tree.prototype, [
    'childProperty',
    'nodeTemplate'
]);

Phoenix.UI.Tree.createClass('Phoenix.UI.Tree', Control);
Trait.Apply(Phoenix.UI.Tree.prototype, BaseListDataControl);

ControlsFactory.registerControl('tree', Phoenix.UI.Tree);
