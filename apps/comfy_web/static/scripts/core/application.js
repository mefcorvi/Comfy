// public static Application
Application =
{
    _configuration: null,
    _context: new ApplicationContext(),
    _masterPage: null,

    _currentPage: null,

    _onError: null,

    _resizeTimeout: null,

    __updatesCount: 0,

    _resources: {},

    get_resource: function(resourceName) {
        var resourceInfo = resourceName.split(':');

        if (resourceInfo.length != 2) {
            throw new Error('The "resource" args should have a "<source>:<resourceName>" format');
        }
        
        if (resourceInfo[0] == 'page') {
            return Application.get_currentPage().get_resource(resourceInfo[1]);
        }
    },

    set_resource: function(resourceName, value) {
        
    },

    // ======== Properties ========
    /**
    * Returns configuration of the application
    * @return Configuration
    */
    get_configuration: function () {
        return Application._configuration;
    },

    get_context: function () {
        return Application._context;
    },

    get_currentUser: function () {
        return Application._context.get('currentUser');
    },

    isDebugMode: function() {
        return Application.get_configuration().get_isDebug();
    },

    /**
    * Returns information about system and environment
    */
    get_systemInfo: function () {
        var info = "UserAgent: " + navigator.userAgent;
        info += "\r\nPage: " + Application.get_currentPage().get_uri();

        return info;
    },

    /**
    * Set configuration of the application
    */
    set_configuration: function (value) {
        if (!Application._configuration) {
            Application._configuration = new Configuration();
        }

        Application._configuration.init(value);
    },

    get_siteMap: function () {
        return Application._siteMap.clone();
    },

    getPagesForMenu: function (items) {
        var siteMap = [].makeObservable();
        var items = items || Application._siteMap;

        for (var i = 0; i < items.length; i++) {
            var menuItem = items[i];

            if (menuItem.showInMenu) {
                siteMap.add({
                    href: menuItem.href,
                    title: menuItem.title,
                    childs: Application.getPagesForMenu(menuItem.childs)
                });
            }
        }

        return siteMap;
    },

    getPageInfoByUrl: function (href) {
        var items = Application._siteMap.clone();

        while (items.length > 0) {
            var menuItem = items.pop();

            if (menuItem.childs) {
                items.add(menuItem.childs);
            }

            if (menuItem.showInMenu && menuItem.href == href) {
                return menuItem;
            }
        }

        return null;
    },

    /**
    * Returns master page
    */
    get_masterPage: function () {
        return Application._masterPage;
    },

    /**
    * Returns current page
    */
    get_currentPage: function () {
        return Application._currentPage;
    },

    get_error403Uri: function () {
        return Application.get_configuration().get_error403Uri();
    },

    get_error404Uri: function () {
        return Application.get_configuration().get_error404Uri();
    },

    load404: function() {
        Application.loadPage(Application.get_error404Uri());
    },

    // ============ Methods ============
    loadPage: function (pageUri) {
        $.history.load(pageUri);
    },

    pageIsAvailable: function (pageUri) {
        var tempUri = "page:" + pageUri;

        var stack = this._siteMap.clone();

        while (stack.length > 0) {
            var item = stack.pop();

            if (item.href === tempUri) {
                return true;
            }

            if (item.childs && item.childs.length > 0) {
                stack.add(item.childs);
            }
        }

        return false;
    },

    reloadPage: function () {
        Application._loadPage(Application._currentPage.get_fullUri(), true);
    },

    _unloadCurrentPage: function() {
        if (Application._currentPage) {
            Application._clearUpdatingEvent();
            var container = Application._masterPage['page_container'],
                containerStyle = container.domElement.style,
                curPage = Application._currentPage;

            containerStyle.position = "absolute";
            containerStyle.left = "-5000px";
            containerStyle.top = "-5000px";
            containerStyle.display = "none";

            Application._addUpdatingEvent();
            curPage.free();

            if (container.controls.contains(curPage))
                container.controls.remove(curPage);

            Application.raise_onPageUnload();
            Application._removeUpdatingEvent();
        }
    },

    _loadNewPage: function(pageUri) {
        Application._loadingPageUri = pageUri;
        var newPage = new Page();
        Application._currentPage = newPage;

        newPage.add_fullUriChanged(function(sender, args) {
            $.history.load(args.newValue);
        });
        newPage.add_initComplete(Application._onPageLoaded);

        Application._addUpdatingEvent();
        Application.raise_onPageLoading();
        newPage.initFromUri(pageUri);
    },

    /**
    * Function which will be invoked when page is loaded from server and initialized
    */
    _onPageLoaded: function() {
        Application._loadingPageUri = null;

        var container = Application._masterPage['page_container'];
        var containerStyle = container.domElement.style;
        container.controls.add(Application._currentPage);
        Application.raise_onPageLoad();
        Application._removeUpdatingEvent();

        window.setTimeout(function () {
            containerStyle.position = "";
            containerStyle.left = "";
            containerStyle.top = "";
            containerStyle.display = "";
        }, 0);
    },

    /**
    * Load page by uri
    */
    _loadPage: function (pageUri, forceReload) {
        var args = { pageUri: pageUri, cancel: false };
        Application.raise_onPagePreLoading(args);

        // support for cancelation
        if (args.cancel) {
            return;
        }        

        // pageUri can be changed in page pre loading
        pageUri = args.pageUri || Application.get_configuration().get_startPageUri();

        var curUri = Application._currentPage ? Application._currentPage.get_fullUri() : null;

        // if uri is the same
        if (!forceReload) {
            if (Application._loadingPageUri == pageUri || curUri == pageUri) {
                return;
            }

            // if a base parts of uri are equal
            if (Application._currentPage && Application._currentPage.isSameUri(pageUri)) {
                Application._currentPage.set_fullUri(pageUri);
                return;
            }
        }

        Application._unloadCurrentPage();
        Application._loadNewPage(pageUri);
    },

    _addUpdatingEvent: function () {
        Application.__updatesCount++;

        if (Application.__updatesCount == 1) {
            if (!Application.get_silentMode()) {
                Application.raise_onUpdating();
            }
        }
    },

    _removeUpdatingEvent: function () {
        if (Application.__updatesCount == 0) {
            return;
        }

        Application.__updatesCount--;

        if (Application.__updatesCount == 0) {
            if (!Application.get_silentMode()) {
                Application.raise_onUpdated();
            }
        }
    },

    _clearUpdatingEvent: function () {
        if (Application.__updatesCount == 0) {
            return;
        }

        Application.__updatesCount = 0;
        Application.raise_onUpdated();
    },

    _initPages: function () {
        Application._masterPage = new Page();
        //Application._currentPage = new Page();

        Application._masterPage.add_initComplete(function () {
            var domElement = Application.get_configuration().get_domElement();
            Application._masterPage.instantiateInDom(domElement);
            Application._masterPage.update();
            Application._initHistoryHanders();

            Application.raise_onInitialized();
            Application._removeUpdatingEvent();
        });

        Application._addUpdatingEvent();
        Application._masterPage.initFromUri(Application.get_configuration().get_masterPage());
    },

    /**
    * Initializes the application
    */
    init: function (onSuccess) {
        // get available pages
        Services.PagesService.getPagesList(function (pages) {
            Application._siteMap = pages;
            Application.raise_onSiteMapInitialized();

            Application._initPages();
            Application._initHandlers();
            Application._initCss();

        }.bind(this), function () {
            Application.throwError("Access is denied");
        }.bind(this));

        if (Application.get_configuration().get_globalErrorHandling()) {
            window.onerror = function(errorMsg, url, lineNumber) {
                Application.throwError("Error at line " + lineNumber + ": " + errorMsg);
            };
        }
    },

    _initCss: function () {
        CSSRules.init();
        CSSRules.add("div, a, span, li, ul, ol", "overflow: hidden; border: 0 solid #000; word-wrap: break-word;");
        //CSSRules.add("div, a, span, li, ul, ol", "overflow: hidden; border-color: #000; border-style: solid;");
    },

    _disposeCss: function() {
        CSSRules.dispose();
    },

    _initHandlers: function () {
        $(window).bind("resize", function () {
            if (Application._resizeTimeout) {
                clearTimeout(Application._resizeTimeout);
                Application._resizeTimeout = null;
            }

            Application._resizeTimeout = setTimeout(function () {
                Application._clientBoundingRect = null;
                Application._masterPage.update();
            }, 50);
        });

        $(document).bind('keydown', function (event) {
            Application.raise_onKeyDown({ keyCode: event.keyCode });
        });
    },

    _initHistoryHanders: function () {
        $(document).ready(function () {
            function pageload(hash) {
                Application._loadPage(hash);
            }

            $.history.init(pageload, {
                unescape: true
            });
        });
    },

    /**
    * Throw an error
    */
    throwError: function (error) {
        Application.raise_onError(error);
        Application._clearUpdatingEvent();
    },

    _urlFilters: {},

    registerUrlFilter: function (protocol, filter) {
        Application._urlFilters[protocol] = filter;
    },

    /**
    * Converts url from it virtual representation to absolute
    */
    resolveUrl: function (virtualUrl) {
        virtualUrl = virtualUrl || '';

        var rurl = /^(\w+:)?(?:\/\/)?(.*)/;

        var parts = rurl.exec(virtualUrl);

        if (parts && parts[1]) {
            var protocol = parts[1];

            if (Application._urlFilters[protocol]) {
                virtualUrl = Application._urlFilters[protocol](virtualUrl, parts[2]);
            }
        }

        return virtualUrl.replace('~/', Application.get_configuration().get_rootUrl());
    },

    showConfirm: function (title, text, callback) {
        var confirmPopup = Application._masterPage['confirmPopup'];

        if (!isFunction(callback)) {
            throw new Error('You cannot show confirm without callback function!');
        }

        if (confirmPopup) {
            var onCommand = function (sender, args) {
                confirmPopup.remove_onCommand(onCommand);
                callback(args.button == 'Yes');
                confirmPopup.close();
            };

            confirmPopup.add_onCommand(onCommand);

            confirmPopup.set_title(title);
            confirmPopup.container['popupText'].set_text(text);
            confirmPopup.open();
        }
    },

    showError: function (title, text, callback) {
        var errorPopup = null;

        if (Application._masterPage && Application._masterPage.controls) {
            errorPopup = Application._masterPage['errorPopup'];
        }

        if (errorPopup) {
            var onCommand = function (sender, args) {
                errorPopup.remove_onCommand(onCommand);

                if (callback) {
                    callback();
                }

                errorPopup.close();
            };

            errorPopup.add_onCommand(onCommand);

            errorPopup.set_title(title);
            errorPopup.container['popupText'].set_text(text);
            errorPopup.open();
        } else {
            alert(text);
        }
    },

    _clientBoundingRect: null,

    _get_boundingRect: function () {
        $appDom = $(this.get_configuration().get_domElement());
        $appDom.children('*').hide(); // we must hide the content of dom element to properly handle the size

        Application._clientBoundingRect = {
            width: $appDom.width(),
            height: $appDom.height()
        }

        $appDom.children('*').show();

        return Application._clientBoundingRect;
    },

    /**
    * Returns the width of the application
    */
    get_clientWidth: function () {
        if (!Application._clientBoundingRect) {
            Application._clientBoundingRect = Application._get_boundingRect();
        }

        return Application._clientBoundingRect.width;
    },

    /**
    * Returns the height of the application
    */
    get_clientHeight: function () {
        if (!Application._clientBoundingRect) {
            Application._clientBoundingRect = Application._get_boundingRect();
        }

        return Application._clientBoundingRect.height;
    }
};

Application.registerUrlFilter("javascript:", function(fullUrl, part) {
    return fullUrl;
});

Application.registerUrlFilter("page:", function(fullUrl, part) {
    return String.format("javascript:Application.loadPage(\"{0}\");", part);
});

Auto.Properties(Application, [
    'silentMode'
]);

Auto.Events(Application, [
    'onInitialized',
    'onPagePreLoading',
    'onPageLoading',
    'onPageLoad',
    'onPageUnload',
    'onError',
    'onKeyDown',
    'onUpdating',
    'onUpdated',
    'onSiteMapInitialized'
]);