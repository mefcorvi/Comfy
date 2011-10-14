({
    title: "Test page",
    layout: "stack",
    orientation: 'horizontal',
    halign: 'stretch',
    cssClass: 'knowledge_base_page',
    
    data: [        
        { name: 'pagesInfos', value: [].makeObservable() },
        'selectedPage',
        { name: 'pageId', value: 1, saveHistory: true }
    ],

    bindings: {    
        'data:pageId': function() {
            this.loadPage(this.get_data().get_pageId() * 1);
        }
    },

    customFunctions: {
        'loadPagesList': function() {
            Services.HelpPageService.GetHelpPagesInfos(function(result) {            
                this.get_data().get_pagesInfos().synchronize(result);
            }.bind(this));
        },
        'loadPage': function(id) {
            var page = Repository.Load('HelpPage', id, function(result) {
                this.get_data().set_selectedPage(result);
            }.bind(this));
        }
    },
    
    onLoad: function() {
        this.pagesListPanel.innerPanel.repeater.set_dataSource(this.get_data().get_pagesInfos());
        this.loadPagesList();        
    },
    
    controls: [
        {
            id: 'pageEdit',
            type: 'popup',
            title: 'Edit page',
            width: '90%',
            height: '90%',
            buttons: [
                'Save', 'Cancel'
            ],
            onCommand: function(sender, args) {
                if (args.button == 'Cancel') {
                    this.close();
                }
                
                if (args.button == 'Save') {
                    this.page.saveData(function(result) {
                        this.get_window().loadPagesList();
                        this.close();
                    }.bind(this));
                }
            }
        },
        {
            type: 'collapsablePanel',
            id: 'pagesListPanel',
            showCollapseButton: false,
            scrolling: true,
            cssClass: 'pages_listPanel',
            title: 'Documents',
            width: '300px',
            height: '*',
            margin: '0 0 3 0',
            showStatusBar: true,
            onCommand: function(sender, args) {
                if (args.button == 'Add') {
                    this.get_window().pageEdit.set_uri('knowledgeBaseEdit');
                    this.get_window().pageEdit.open();
                }
                
                if (args.button == 'Delete') {
                    var page = this.get_data().get_selectedPage();
                    var thisWindow = this.get_window();

                    if (page) {
                        Application.showConfirm('Warning', 'Do you really want to delete this page?', function(result) {
                            if (result) {
                                Repository.Delete(page, function() {
                                    thisWindow.get_data().get_pagesInfos().remove(page);
                                    thisWindow.get_data().set_selectedPage(thisWindow.get_data().get_pagesInfos().first());
                                }.bind(this));
                            }
                        }.bind(this));
                    }
                }

                if (args.button == 'Edit') {
                    var page = this.get_data().get_selectedPage();

                    if (page) {
                        this.get_window().pageEdit.set_uri('knowledgeBaseEdit|pageId=' + page.get_id());
                        this.get_window().pageEdit.open();
                    }
                }
            },
            bindings: {
                'data:selectedPage': function(sender, args) {
                    var value = args.newValue;
                    var currentUser = Application.get_currentUser();

                    if (!currentUser.get_isSuperUser()) {
                        return;
                    }

                    if (value) {
                        this.set_buttons(['Add', 'Edit', 'Delete']);
                    } else {
                        this.set_buttons(['Add']);
                    }
                }
            },
            onLoad: function() {
                var currentUser = Application.get_currentUser();

                if (currentUser.get_isSuperUser()) {
                    this.set_buttons(['Add', 'Edit', 'Delete']);
                } else {
                    this.statusBar.hide();
                    this.innerPanel.set_height('100%-29');
                }
            },
            controls: [
                {
                    type: 'repeater',
                    id: 'repeater',
                    height: '?',
                    width: '100%',
                    orientation: 'vertical',
                    cssClass: 'helppages_list',
                    emptyDataText: 'Documents have not been found',
                    template: {
                        type: 'link',
                        width: '*',
                        height: '?',
                        padding: '5',
                        cssClass: 'helppages_item',
                        bindings: {
                            'title': 'text',
                            'isUnread': function(sender, args) {
                                var value = args.newValue;
                                if (value) {
                                    this.addCssClass('helppages_item_unread');
                                } else {
                                    this.removeCssClass('helppages_item_unread');
                                }
                            },                            
                            'data:selectedPage': function(sender, args) {
                                var value = args.newValue;
                                if (value === this.get_dataSource()) {
                                    this.addCssClass('helppages_item_selected');
                                } else {
                                    this.removeCssClass('helppages_item_selected');
                                }
                            }
                        },
                        onClick: function () {
                            this.get_data().set_pageId(this.get_dataSource().get_id());
                        }
                    }
                }
            ]           
        },
        {
            type: 'collapsablePanel',
            id: 'pageContentPanel',
            scrolling: true,
            showCollapseButton: false,
            title: '',
            orientation: 'horizontal',
            padding: '5',
            cssClass: 'helppage_content',
            bindings: {
                'data:selectedPage': function(sender, args) {
                    var value = args.newValue;
                    if (value) {
                        Services.HelpPageService.LoadAndMarkAsRead(value.get_id(), function(result) {
                            this.pageContentLiteral.set_dataSource(result);
                            this.focus();
                            this.set_title(result.get_title());
                        }.bind(this));
                    } else {
                        this.set_title('Empty');
                    }
                }
            },
            controls: [
                {
                    type: 'literal',
                    dataType: 'html',
                    height: '?',
                    width: '100%',
                    id: 'pageContentLiteral',
                    text: '<i>Empty page</i>',
                    bindings: {
                        'content': function(sender, args) {
                            var value = args.newValue;
                            this.set_text('<div>' + value + '</div>');
                            this.update();
                        }
                    }
                }
            ]
        }
    ]
})