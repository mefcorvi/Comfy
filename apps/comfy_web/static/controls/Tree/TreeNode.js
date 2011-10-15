Type.createNamespace('Phoenix.UI');

Phoenix.UI.TreeNode = function() {
    Phoenix.UI.TreeNode.constructBase(this);
    this._innerTreeShowed = true;
};

Phoenix.UI.TreeNode.prototype = {
    _collapser: null,
    _innerTree: null,
    _innerTreeShowed: null,

    defaultOptions: {
        'width': '*',
        height: '?'
    },

    get_innerWidth: function() {
        return Math.max(Phoenix.UI.TreeNode.callBase(this, "get_innerWidth") - 13, 0);
    },

    initFromOptions: function (options) {
        Phoenix.UI.TreeNode.callBase(this, "initFromOptions", [options]);
        this.addCssClass('tree_node');
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div", domElement);
        this._initCollapser();

        Phoenix.UI.TreeNode.callBase(this, "instantiateInDom", [domElement]);

        this._innerTreeModified();
    },

    _initCollapser: function () {
        if (this._innerTree && !this._collapser && this.domElement) {
            this._collapser = DOM.create("span", this.domElement, { innerHTML: "&nbsp;", className: 'tree_collapser' });
            $(this._collapser).bind("click", this._collaperClick.bind(this));
            DOM.disableSelection(this._collapser);
        }
    },

    _collaperClick: function () {
        this._innerTreeShowed = !this._innerTreeShowed;

        if (this._innerTreeShowed) {
            this._innerTree.show(true);
            $(this._collapser).removeClass('tree_collapsed');
        } else {
            this._innerTree.hide(true);
            $(this._collapser).addClass('tree_collapsed');
        }
    },

    set_isSelected: function (value) {        
        if (this._isSelected === value) {
            return;
        }

        this._isSelected = value;

        if (value) {
            this.addCssClass('tree_node_selected');
        } else {
            this.removeCssClass('tree_node_selected');
        }
    },

    _innerTreeModified: function (sender, args) {
        this._initCollapser();

        if (this._collapser) {
            this._collapser.style.display = !this._innerTree.isEmpty() ? '' : 'none';
        }
    },

    set_innerTree: function (value) {
        if (this._innerTree === value) {
            return;
        }

        this._innerTree = value;
        this._innerTreeModified();
    },

    free: function () {
        Phoenix.UI.TreeNode.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.TreeNode.prototype, [
    { name: 'innerTree' },
    { name: 'isSelected' }
]);

Phoenix.UI.TreeNode.createClass('Phoenix.UI.TreeNode', Control);
ControlsFactory.registerControl('treeNode', Phoenix.UI.TreeNode);
