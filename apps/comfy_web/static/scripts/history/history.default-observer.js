Type.createNamespace('History');

History.DefaultObserver = function(itemName, relatedObject, page) {
    History.DefaultObserver.constructBase(this, [itemName, relatedObject, page]);
};

History.DefaultObserver.canObserve = function(obj, type) {
    return true;
};

History.DefaultObserver.prototype = {
};

History.DefaultObserver.createClass('History.DefaultObserver', History.BaseObserver);