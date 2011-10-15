var DepthManager = {
    __lastZIndex: 10000,

    getNewZIndex: function() {
        DepthManager.__lastZIndex++;

        return DepthManager.__lastZIndex;
    }
};