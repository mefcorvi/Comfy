Type.createNamespace('Phoenix.UI');

Phoenix.UI.Pager = function() {
    Phoenix.UI.Pager.constructBase(this);
};

Phoenix.UI.Pager.prototype = {
    defaultOptions: {
        'height': '24px',
        'width': '*',
        'padding': '5 3 2 2',
        'border': '1'
    },

    initFromOptions: function(options) {
        var thisObj = this;
        
        this.addCssClass('pager_control');

        options.orientation = 'horizontal';
        options.valign = 'middle';

        this.add_onFree(function() {
            this.clearPagingTimeOut();
        }, this);
        
        options.customFunctions = {
            '_setCount': function(result) {
                var sender = this.get_dataSource();
                this.pageSize = sender.get_pageSize() || 10;
                this.pagesCount = Math.ceil(result / this.pageSize);
                this._updatePaging();

                if (this.get_page() > this.pagesCount) {
                    this.set_page(1);
                }
            },

            'clearPagingTimeOut': function() {
                if (this._updatePagingTimeOut) {
                    clearTimeout(this._updatePagingTimeOut);
                    this._updatePagingTimeOut = null;
                }
            },

            '_updatePaging': function() {
                var ds = this.pagesNavigation.get_dataSource(),
                    pagesCount = this.pagesCount,
                    page = this.get_dataSource().get_page()*1;

                this.set_page(page);

                if (!pagesCount || !page) {
                    if (ds) {
                        ds.clear();
                    }

                    return;
                }

                if (ds) {
                    ds.clear();
                } else {
                    ds = [].makeObservable();
                    this.pagesNavigation.set_dataSource(ds);
                }
                
                var pages = [];

                pages.add({ text: '< Prev', enabled: page > 1, delta: -1 });
                pages.add({ text: 'Next >', enabled: page < pagesCount, delta: 1 });

                var leftIdx = Math.max(page - 2, 1),
                    rightIdx = Math.min(page + 2, pagesCount);

                if (leftIdx > 1) {
                    pages.add(1);
                }

                if (leftIdx == 3) {
                    pages.add(2);
                } else if (leftIdx > 2) {
                    pages.add({ separator: true });
                }

                for (var i = leftIdx; i <= rightIdx; i++) {
                    pages.add(i);
                }

                if (rightIdx == pagesCount - 2) {
                    pages.add(pagesCount - 1);
                } else if (rightIdx < pagesCount - 1) {
                    pages.add({ separator: true });
                }

                if (rightIdx < pagesCount) {
                    pages.add(pagesCount);
                }

                ds.add(pages);
                
                var curSelectedPage = this.pagesNavigation.findByDataItem(page);

                if (curSelectedPage) {
                    curSelectedPage.addCssClass('pager_link_selected');
                };
            }
        };
        
        var newBindings = {
            '*': function(sender, args) {
                args.newValue.set_page(this.get_page());
            },
            'count': function(sender, args) {
                this._setCount(args.newValue);
            },
            'page': function(sender, args) {
                this.clearPagingTimeOut();
                this._updatePagingTimeOut = setTimeout(this._updatePaging.bind(this), 0);
            }
        };

        if (options.bindings) {
            Object.extend(options.bindings, newBindings);
        } else {
            options.bindings = newBindings;
        }
        
        options.controls = [
            {
                id: 'pagesNavigation',
                type: 'repeater',
                height: '18',
                width: '*',
                orientation: 'horizontal',
                cssClass: 'pager_nav',
                templateSelector: function(dataItem) {
                    if (isNumber(dataItem)) {
                        return {
                            bindings: {
                                '*': function (sender, args) {
                                    var value = args.newValue;
                                    this.set_text(value);
                                    this.removeCssClass('pager_link_selected');
                                }
                            },
                            type: 'link',
                            height: '*',
                            cssClass: 'pager_link',
                            margin: '0 0 3 0',
                            onClick: function() {
                                thisObj._pagingCommand(this.get_dataSource());
                            }
                        }
                    }

                    if (dataItem.separator) {
                        return {
                            type: 'label',
                            bindings: {
                                '*': undefined
                            },
                            text: '...',
                            height: '*',
                            width: 15,
                            margin: '0 0 3 0'
                        }
                    }

                    return {
                        bindings: {
                            'text': 'text',
                            'enabled': 'enabled'
                        },
                        type: 'link',
                        height: '*',
                        cssClass: 'pager_link',
                        margin: '0 0 10 0',
                        onClick: function() {
                            var ds = thisObj.get_dataSource();

                            if (ds) {
                                thisObj._pagingCommand(ds.get_page() + this.get_dataSource().delta);
                            }
                        }
                    }
                }
            }
        ];
        
        Phoenix.UI.Pager.callBase(this, "initFromOptions", [ options ]);
    },

    set_page: function(page) {
        var oldPage = this._page,
            ds = this.get_dataSource();

        if (oldPage === page) {
            return;
        }

        this._page = page;

        if (ds) {
            ds.set_page(page);
        }

        this.raise_pageChanged({ newValue: page, oldValue: oldPage });
    },
    
    _pagingCommand: function(pageNum) {
        this.set_page(pageNum);
    }
};

Auto.Properties(Phoenix.UI.Pager.prototype, [
    { name: 'page', autoEvent: true, defaultValue: 1 }
]);

Phoenix.UI.Pager.createClass('Phoenix.UI.Pager', Phoenix.UI.Panel);
ControlsFactory.registerControl('pager', Phoenix.UI.Pager);
