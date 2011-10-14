var CSSRules = function() {
    var styleElement = DOM.create("style");
    styleElement.type = "text/css";

    var add = function() {
        if (styleElement.styleSheet) {
            return function(selector, rule) {
                if (styleElement.styleSheet.cssText == '') {
                    styleElement.styleSheet.cssText = '';
                }
                styleElement.styleSheet.cssText += selector + " { " + rule + " }";
            }
        } else {
            return function(selector, rule) {
                styleElement.appendChild(document.createTextNode(selector + " { " + rule + " }"));
            }
        }
    } ();

    var init = function() {
        var headElement = document.getElementsByTagName("head")[0];
        headElement.insertBefore(styleElement, headElement.firstChild);
    };

    var dispose = function() {
        var headElement = document.getElementsByTagName("head")[0];
        headElement.removeChild(styleElement);
    };

    return {
        add: add,
        init: init,
        dispose: dispose
    }
} ();