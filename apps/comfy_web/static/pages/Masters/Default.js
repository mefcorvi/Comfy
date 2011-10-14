({
    title: "Master Page",
    layout: "stack",
    orientation: 'vertical',
    cssClass: 'master_page',
    valign: 'stretch',
    padding: '0 5px 0 0',

    customFunctions: {
        'showFeedbackForm': function() {
            this.feedbackForm.open();
        },

        'showEventList': function() {
            this.eventList.open();
        },

        'showUserFeedbacks': function() {
            this.userFeedbacks.open();
        }
    },

    controls:
    [
        {
            type: 'popup',
            id: 'confirmPopup',
            minWidth: '100',
            width: '30%',
            height: '?',
            scrolling: true,
            buttons: ['Yes', 'No'],
            controls: [
                {
                    type: 'label',
                    padding: '5',
                    width: '*',
                    height: '?',
                    id: 'popupText'
                }
            ]
        },

        {
            type: 'popup',
            id: 'feedbackForm',
            width: '30%',
            height: '300',
            title: 'Send Feedback',
            buttons: ['Send', 'Close'],
            controls: [
                { type: 'feedbackForm', id: 'form', padding: '5' }
            ],
            onCommand: function(sender, args) {
                if (args.button === 'Close') {
                    this.close();
                }

                if (args.button === 'Send') {
                    this.container.form.send();
                    this.close();
                }
            }
        },

        {
            type: 'popup',
            id: 'eventList',
            title: 'Events',
            width: '800',
            height: '600',
            uri: 'notifications',
            buttons: [
                { text: 'Remove all', cssClass: 'link_button red_button' },
                { text: 'Mark all as read', cssClass: 'link_button red_button' },
                { text: 'Close' }
            ],
            onCommand: function (sender, args) {
                if (args.button == 'Remove all') {
                    Application.showConfirm('Warning', 'Do you really want to remove all notifications?', function(result) {
                        if (result) {
                            Services.UsersService.Notifications.RemoveAll();
                        }
                    }.bind(this));
                }

                if (args.button == 'Mark all as read') {
                    Services.UsersService.Notifications.MarkAllAsRead();
                }

                if (args.button == 'Close') {
                    this.close();
                }
            }
        },

        {
            type: 'popup',
            id: 'userFeedbacks',
            title: 'Feedbacks',
            width: '800',
            height: '600',
            uri: 'userFeedbacks',
            buttons: ['Close'],
            onCommand: function (sender, args) {
                if (args.button == 'Close') {
                    this.close();
                }
            }
        },

        {
            type: 'popup',
            id: 'errorPopup',
            minWidth: 100,
            width: '30%',
            height: '?',
            scrolling: true,
            buttons: ['OK'],
            controls: [
                {
                    type: 'label',
                    padding: '5',
                    width: '*',
                    height: '?',
                    id: 'popupText'
                }
            ]
        },

    // top panel
        {
        id: 'siteHeader',
        cssClass: "site_header",
        type: "panel",
        layout: 'stack',
        orientation: 'horizontal',
        width: "100%-9",
        height: 37,
        controls: [
            {
                type: "link",
                url: "page:",
                width: 127,
                height: 37,
                controls: [
                    {
                        cssClass: "logo",
                        type: "image",
                        src: "~/images/logo.png",
                        width: 127,
                        height: 37
                    }
                ]
            },
            {
                type: "welcomePane",
                height: 27,
                valign: 'middle'
            },
            {
                type: "panel",
                cssClass: "page_top_nav",
                width: '?',
                margin: '20 0 0 0',
                height: 18,
                valign: 'middle',
                orientation: 'horizontal',
                onLoad: function() {
                    var currentUser = Application.get_currentUser();
                    this.helpLink.set_dataSource(currentUser);
                },
                controls:
                    [
                        { type: "link", url: "page:myProfile", text: "My Profile", margin: '3px 0' },
                        { type: "label", text: "|" },
                        { type: "link", text: "Relogin", margin: '3px 0',
                            domHandlers: {
                                'focus': function () { // some kind of magic to make it work in IE6
                                    this.set_url("Relogin.aspx?oldUrl=" + encodeURIComponent(window.location.href) + "&rnd=" + Math.random());
                                },
                                'mousedown': function () { // focus and mousedown is different events in Chrome
                                    this.set_url("Relogin.aspx?oldUrl=" + encodeURIComponent(window.location.href) + "&rnd=" + Math.random());
                                }
                            }
                        },
                        { type: "label", text: "|" },
                        {
                            type: "link",
                            text: "Feedback",
                            margin: '3px 0',
                            onClick: function() {
                                this.get_window().showFeedbackForm();
                            }
                        },
                        { type: "label", text: "|" },
                        {
                            type: 'link',
                            text: 'Notifications',
                            margin: '3px 0',
                            bindings: {
                                'messageStatistics': function(sender, args) {
                                    var result = args.newValue;
                                    var unreadCount = result._all - result._read;
                                    if (unreadCount > 0) {
                                        this.set_text('Notifications (' + unreadCount + ')');
                                        this.addCssClass('helpLink_unreaded');
                                    } else {
                                        this.set_text('Notifications');
                                        this.removeCssClass('helpLink_unreaded');
                                    }    
                                }
                            },
                            onLoad: function() {   
                                this.set_dataSource(Application.get_currentUser());
                            },
                            onClick: function() {
                                this.get_window().showEventList();
                            }
                        },
                        { type: "label", text: "|" },
                        {
                            id: 'helpLink',
                            type: "link",
                            url: "page:knowledgeBase",
                            text: "Help",
                            margin: '3px 0',
                            customFunctions: {
                                '_updateLink': function(count) {
                                    if (count > 0) {
                                        this.set_text('What\'s new (' + count + ')');
                                        this.addCssClass('helpLink_unreaded');
                                    } else {
                                        this.set_text('Help');
                                        this.removeCssClass('helpLink_unreaded');
                                    }
                                }
                            },
                            bindings: {
                                'unreadHelpPages': function(sender, args) {
                                    var value = args.newValue;
                                    this._updateLink(value.length);
                                },
                                'unreadHelpPages.changed': function(sender, args) {
                                    this._updateLink(sender.length);
                                }
                            }
                        }
                    ]
            }
            ]
    },
        {
            id: 'menu',
            type: "menu",
            width: "100%-9",
            height: "60",
            onLoad: function () {
                var pages = Application.getPagesForMenu();
                this.set_dataSource(pages);

                if (pages.length == 0) {
                    this.hide(true);
                    this.set_height(new DimensionUnit(0));
                }
            }
        },
        {
            id: 'collapser',
            type: 'link',
            cssClass: 'page_top_collapser',
            text: 'collapse',
            width: '*',
            height: '5',
            onClick: function () {
                if (!this.__collapsed) {
                    this.parent.siteHeader.hide();
                    this.parent.menu.hide();
                    this.addCssClass('page_top_collapser_collapsed');
                    this.parent.update();

                    this.__collapsed = true;
                } else {
                    this.parent.siteHeader.show();
                    this.parent.menu.show();
                    this.removeCssClass('page_top_collapser_collapsed');
                    this.parent.update();

                    this.__collapsed = false;
                }
            }
        },
        {
            id: "page_container",
            type: "box",
            width: '*',
            height: '*'
        }
    ]
})