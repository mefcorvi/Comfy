var ControlsTest = function() {
};

ControlsTest.prototype = {
    createContainer: function(width, height) {
        var container = document.createElement("div");
        container.id = "container";
        container.style.background = "#fff";
        container.style.border = "0px solid #000";
        container.style.width = width + "px";
        container.style.height = height + "px";
        container.style.left = "-32768px";
        container.style.top = "-32768px";
        container.style.position = "absolute";

        document.body.appendChild(container);

        return container;
    },

    removeContainer: function(container) {
        document.body.removeChild(container);
    },

    runTest: function(testCase) {
        if (!testCase.name || !testCase.run)
            alert('Wrong options in test case');

        var createContainer = this.createContainer,
            removeContainer = this.removeContainer;

        var oldOff = $.fx.off;
        $.fx.off = true;

        var testFunc = testCase.ignored ? QUnit.test.ignored : QUnit.test;

        testFunc(testCase.name, function() {
            var testContext = {
                container: createContainer(1024, 768),

                initPage: function(options) {
                    options.type = 'page';
                    options.width = '100%';
                    options.height = '100%';

                    var page = new Page();
                    Application._currentPage = page;

                    page.initFromOptions(options);
                    page.instantiateInDom(this.container);
                    page.update(null, true);

                    this.page = page;
                },

                setSize: function(width, height) {
                    this.container.style.width = width + "px";
                    this.container.style.height = height + "px";
                    Application._clientBoundingRect = null;

                    if (this.page) {
                        this.page.update();
                    }
                },

                checkRect: function(control, expectedWidth, expectedHeight) {
                    if (!control || !control.domElement) {
                        throw new Error('Function "checkRect": cannot get DOM node for control');
                    }

                    var controlId = control.options.type + (control.options.id ? ', ' + control.options.id : '') + ': ';

                    Control.forceUpdates();
                    var actualWidth = control.domElement.offsetWidth;
                    var actualHeight = control.domElement.offsetHeight;
                    var result = actualWidth == expectedWidth && actualHeight == expectedHeight;

                    var expectedStr = expectedWidth + "x" + expectedHeight;
                    var actualStr = actualWidth + "x" + actualHeight
                    ok(result, controlId + (result ? "okey, size is " + expectedStr : "failed, expected: " + expectedStr + ", actual: " + actualStr));
                },

                checkPos: function(control, expectedX, expectedY) {
                    if (!control || !control.domElement) {
                        throw new Error('Function "checkPos": cannot get DOM node for control');
                    }

                    var actualX = control.domElement.offsetLeft;
                    var actualY = control.domElement.offsetTop;
                    var result = actualX == expectedX && actualY == expectedY;

                    var expectedStr = "(" + expectedX + ", " + expectedY + ")";
                    var actualStr = "(" + actualX + ", " + actualY + ")";
                    ok(result, result ? "okey, position is " + expectedStr : "failed, expected: " + expectedStr + ", actual: " + actualStr);
                }
            };

            Application.set_configuration({
                domElement: $get("container")
            });

            Application._initCss();
            testCase.run.bind(testContext)();
        
            removeContainer(testContext.container);
            Application._disposeCss();
            $.fx.off = oldOff;
        });
    },

    run: function(testCases) {
        for (var i = 0; i < testCases.length; i++) {
            if (testCases[i])
                this.runTest(testCases[i]);
        }
    }
};