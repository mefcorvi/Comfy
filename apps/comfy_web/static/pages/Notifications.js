({
    data: [
        {
            name: 'dsMessages',
            value: [].makeDataSource('Message').filterBy('Receiver.Id = ' + Application.get_currentUser().get_id()).orderBy('dateSent desc').paginate(15).simple()
        },
        { name: 'selectedMessage', value: null, autoEvent: true }        
    ],

    customFunctions: {
        'deleteNotification': function(notification) {
            Repository.Delete(notification);            
            this.get_data().set_selectedMessage(null);
        }
    },

    cssClass: 'notifications_page',

    bindings: {
        'data:selectedMessage': function(sender, args) {
            if (args.newValue) {
                var message = args.newValue;
                if (!message.get_isRead())
                {
                    var user = Application.get_currentUser();
                    var stat = user.get_messageStatistics();
                    user.set_messageStatistics({ _all: stat._all, _read: stat._read + 1 });

                    message.set_isRead(true);
                    Repository.Save(message);
                }
            }
        },
        'data:dsMessages.changed': function(sender, args) {
            if (sender.length == 0) {
                this.get_data().set_selectedMessage(null);
            }
        }
    },

    onLoad: function() {
        this.get_data().get_dsMessages().load(function(result) {
            this.get_data().set_selectedMessage(result.length > 0 ? result.first() : null);
        }.bind(this));
    },

    controls: [
        {
            type: 'panel',
            orientation: 'horizontal',
            controls: [
                {
                    type: 'collapsablePanel',
                    width: '30%',
                    margin: '0 0 3 0',
                    title: 'List of notifications',
                    showCollapseButton: false,
                    bindings: {
                        'data:dsMessages': 'pager'
                    },
                    controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    type: 'repeater',
                                    emptyDataText: 'You do not have any notification',
                                    template: {
                                        type: 'panel',
                                        cssClass: 'notification_item',
                                        border: '0 0 0 1',
                                        padding: '0 6 0 6',
                                        orientation: 'horizontal',
                                        height: '?',
                                        domHandlers: {
                                            'mouseover': function() {
                                                this.addCssClass('notification_item_hover');
                                            },
                                            'mouseout': function() {
                                                this.removeCssClass('notification_item_hover');
                                            },
                                            'click': function() {
                                                var message = this.get_dataSource();                                                
                                                this.get_data().set_selectedMessage(message);
                                            }
                                        },
                                        bindings: {
                                            '*': '*',
                                            'isRead': function(sender, args) {                                                
                                                if (args.newValue)
                                                    this.removeCssClass('notification_item_unread');
                                                else
                                                    this.addCssClass('notification_item_unread');                                                
                                            },
                                            'data:selectedMessage': function(sender, args) {
                                                this.removeCssClass('notification_item_selected');

                                                if (args.newValue == this.get_dataSource()) {
                                                    this.addCssClass('notification_item_selected');
                                                }
                                            }
                                        },
                                        controls: [                                            
                                            {
                                                type: 'panel',
                                                width: '*',
                                                height: '?',
                                                padding: '10 0 0 0',
                                                bindings: {
                                                    '*': 'info',
                                                    'title': 'title'
                                                },
                                                controls: [
                                                    {
                                                        id: 'info',
                                                        type: 'panel',
                                                        height: '16',
                                                        orientation: 'horizontal',
                                                        bindings: {
                                                            'dateSent': 'dateSent'                                                            
                                                        },
                                                        controls: [                                                            
                                                            {
                                                                id: 'dateSent',
                                                                cssClass: 'posted_by',
                                                                width: '100',
                                                                height: '*',
                                                                format: '{0:g}',
                                                                type: 'label'
                                                            }
                                                        ]
                                                    },
                                                    {
                                                        id: 'title',
                                                        cssClass: 'notification_title',
                                                        type: 'label',
                                                        width: '*',
                                                        emptyText: '<Untitled>'
                                                    }
                                                ]
                                            },
                                            {
                                                type: 'link',                                            
                                                width: '15',
                                                height: '15',
                                                margin: '10 0 10 0',
                                                valign: 'middle',                                                
                                                cssClass: 'deleteButton',
                                                domHandlers: {
                                                    'click': function (sender, args) {                                                        
                                                        args.stopPropagation();
                                                        this.get_window().deleteNotification(this.get_dataSource());                                                        
                                                    }
                                                }
                                            }
                                        ]
                                    },
                                    onLoad: function() {
                                        this.set_dataSource(this.get_data().get_dsMessages());
                                    }
                                }
                            ]
                        },
                        {
                            id: 'pager',
                            type: 'pager'
                        },
                        {
                            type: 'searchPane',                            
                            onSearch: function (sender, args) {
                                var filterQuery = args.query || '';
                                this.get_data().get_dsMessages().set_searchQuery(filterQuery, true);
                            }
                        }
                    ]
                },
                {
                    type: 'multiView',
                    activeView: 'emptyView', // maybe not the best way to show emptyView but initially 'null' selectedFeedback doesn't trigger the binding below
                    bindings: {
                        'data:selectedMessage': function (sender, args) {                              
                            var value = args.newValue;                                                
                            if (value) {
                                this.set_activeView('normalView');
                                this.normalView.set_dataSource(value);                                                 
                            }
                            else {
                                this.set_activeView('emptyView');
                            }
                        }
                    },
                    views: [
                        {         
                            id: 'normalView',
                            type: 'collapsablePanel',
                            cssClass: 'notification_details',
                            showCollapseButton: false,
                            title: 'Notification:',
                            padding: '10',
                            bindings: {
                                '*': [function(sender, args) {
                                    this.set_title('Notification: ' + (args.newValue ? args.newValue.get_title() : 'null'));
                                }, '*']
                            },
                            controls: [
                                {
                                    type: 'form',
                                    height: '?',
                                    margin: '0 0 0 20',
                                    labelWidth: 60,
                                    bindings: {
                                        'dateSent': 'sent' 
                                    },
                                    controls: [
                                        {
                                            id: 'sent',
                                            type: 'label',
                                            height: '16',
                                            format: '{0:f}',
                                            emptyText: '-',
                                            'form.label': 'Sent:'
                                        }
                                    ]
                                },
                                {
                                    id: 'title',
                                    type: 'label',
                                    height: '20',
                                    width: '*',
                                    margin: '0 0 0 10',
                                    border: '0 0 0 1',
                                    cssClass: 'notification_title',
                                    emptyText: '<Untitled>',
                                    bindings: {
                                        'title': 'text'
                                    }
                                },
                                {
                                    type: 'scrollablePanel',
                                    bindings: {
                                        '*': 'message'
                                    },
                                    controls: [
                                        {
                                            id: 'message',
                                            cssClass: 'notification_content',
                                            emptyText: 'Please, select a notification from the left column',
                                            type: 'label',
                                            width: '*',
                                            multiline: true,
                                            bindings: {
                                                'content': function(sender, args) {
                                                    var text = args.newValue || '';
                                                    this.set_text(text.trim());
                                                }
                                            }
                                        }
                                    ]
                                }
                            ]
                        },
                        {
                            id: 'emptyView',
                            type: 'panel',
                            orientation: 'horizontal',
                            cssClass: 'empty_view',
                            controls: [
                                {
                                    type: 'label',
                                    width: '*',
                                    height: '30',
                                    margin: '0 10 0 0',
                                    halign: 'center',
                                    text: 'Select a notification from left pane'
                                }
                            ]
                        }                        
                    ]
                }                
            ]
        }
    ]
})