({
    title: 'User Management',
    layout: 'stack',
    orientation: 'horizontal',
    cssClass: 'user_management_page',

    data: [
        'usersFilter',
        'activeUser',
        'addNewUserState',
        'editRolesState',
        { name: 'awaitingApprovalUsers', value: [].makeDataSource('User')
                                           .filterBy('awaitingApproval;').orderBy('name').paginate(15).simple()
        } // link to the awaiting approval control
    ],

    customFunctions: {
        '__userRoleSaved': function(sender, args) {
            var awaiting = this.get_data().get_awaitingApprovalUsers();
            
            for (var i = 0; i < args.entities.length; i++) {
                var userRole = args.entities[i];

                for (var j = awaiting.length - 1; j >= 0; j--) {
                    if (awaiting[j].get_id() == userRole.get_userId()) {
                        awaiting.load();
                        break;
                    }
                }
            }
        }
    },

    onLoad: function () {
        this.get_data().set_activeUser(Application.get_context().get('currentUser'));
        Repository.add_userRoleSaved(this.__userRoleSaved, this);
    },

    onFree: function() {
        Repository.remove_userRoleSaved(this.__userRoleSaved);
    },

    controls: [
        { // ========= Popup "Add new User"
            type: 'popup',
            title: 'Add new user',
            width: '300',
            height: '146',
            controls: [
                {
                    type: 'panel',
                    cssClass: 'add_new_user_popup',
                    width: '100%',
                    height: '100%',
                    id: 'innerPanel',
                    padding: '5',
                    controls: [
                        {
                            type: 'label',
                            width: '*',
                            height: '18',
                            text: 'Enter the user name in active directory:'
                        },
                        {
                            type: 'textBox',
                            text: '',
                            width: '100%',
                            id: 'userName'
                        }
                    ]
                }
            ],
            buttons: ['Add', 'Cancel'],
            onLoad: function () {
                this.parent.get_data().add_addNewUserStateChanged(function (sender, args) {
                    if (args.newValue) {
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onCommand: function (sender, args) {
                if (args.button == 'Add') {
                    var container = this.container.innerPanel;
                    var userName = container.userName.get_text();
                    var thisObj = this;

                    if (!userName) {
                        return;
                    }

                    Services.UsersService.RegisterFromAD(userName, function(user) {
                        thisObj.parent.get_data().set_activeUser(user);
                        thisObj.parent.get_data().set_addNewUserState(false);
                    });

                    return;
                }

                this.parent.get_data().set_addNewUserState(false);
            }
        },
        { // ======== Page Content
            type: 'panel',
            cssClass: 'user_management_content',
            width: '9*',
            bindings: {
                'data:activeUser': 'businessCard'
            },
            controls: [
                { // ======= Page Header
                    type: 'pageHeader',
                    title: 'User Management',
                    description: 'Some description for user management page'
                },
                {
                    id: 'businessCard',
                    type: 'userBusinessCard'
                }
            ]
        },
        {
            type: "tabPanel",
            height: "100%",
            width: "2*",
            margin: '3 0 0 0',
            tabs: [
            //#region System Users Tab
                {
                title: 'System Users',
                controls: [
                        {
                            type: 'scrollablePanel',
                            height: '*',
                            controls: [
                                {
                                    id: 'usersList',
                                    type: 'repeater',
                                    cssClass: 'users_list',
                                    emptyDataText: 'Users have not been found',
                                    template: {
                                        type: 'panel',
                                        height: '40',
                                        padding: '5',
                                        bindings: {
                                            '*': 'namePanel',
                                            'imageId': 'avatar'
                                        },
                                        orientation: 'horizontal',
                                        cssClass: 'user_link',
                                        domHandlers: {
                                            'click': function () {
                                                this.get_data().set_activeUser(this.get_dataSource());
                                            }
                                        },
                                        controls: [
                                            {
                                                type: 'entityImage',
                                                width: '25',
                                                height: '25',
                                                valign: 'middle',
                                                id: 'avatar'
                                            },
                                            {
                                                id: 'namePanel',
                                                type: 'panel',
                                                orientation: 'vertical',
                                                margin: '5 0 0 0',
                                                bindings: {
                                                    'name': 'name',
                                                    'login': 'login'
                                                },
                                                controls: [
                                                    {
                                                        type: 'label',
                                                        id: 'name',
                                                        width: '*',
                                                        height: '*',
                                                        cssClass: 'user_name'
                                                    },
                                                    {
                                                        type: 'label',
                                                        id: 'login',
                                                        width: '*',
                                                        height: '*',
                                                        cssClass: 'user_login'
                                                    }
                                                ]
                                            }
                                        ]
                                    },
                                    onLoad: function () {
                                        var ds = [].makeDataSource('User')
                                            .filterBy('').orderBy('name').paginate(15).simple();

                                        this.set_dataSource(ds);
                                        this.parent.parent.pager.set_dataSource(ds);

                                        ds.load();

                                        this.__usersFilterChanged = function (sender, args) {
                                            this.get_dataSource().set_searchQuery(args.newValue, true);
                                        };

                                        this.get_data().add_usersFilterChanged(this.__usersFilterChanged, this);
                                    },
                                    onFree: function () {
                                        this.get_data().remove_usersFilterChanged(this.__usersFilterChanged, this);

                                        if (this.get_dataSource())
                                            this.get_dataSource().dispose();
                                    }
                                }
                            ]
                        },
                        {
                            id: 'pager',
                            type: 'pager'
                        },
                        { // =============== Search Pane ===================
                            type: "searchPane",
                            onSearch: function (sender, args) {
                                this.get_data().set_usersFilter(args.query);
                            }
                        },
                        { // ============= Navigation Bar ================
                            type: "panel",
                            height: 42,
                            cssClass: 'tab_nav_panel',
                            orientation: 'horizontal',
                            controls: [
                                {
                                    type: "link",
                                    text: "Add new user",
                                    height: '18',
                                    width: '50%',
                                    valign: 'middle',
                                    padding: '7 0 0 0',
                                    onClick: function () {
                                        this.get_data().set_addNewUserState(true);
                                    }
                                }
                            ]
                        }
                    ]
            },
            //#endregion
                {
                title: 'Awaiting Approval',
                controls: [
                        { // ============== Users in right panel
                            type: 'scrollablePanel',
                            height: '*',
                            controls: [
                                {
                                    id: 'usersList',
                                    type: 'repeater',
                                    cssClass: 'users_list',
                                    emptyDataText: 'Users have not been found',
                                    template: {
                                        type: 'panel',
                                        height: '40',
                                        padding: '5',
                                        bindings: {
                                            '*': 'namePanel',
                                            'imageId': 'avatar'
                                        },
                                        orientation: 'horizontal',
                                        cssClass: 'user_link',
                                        domHandlers: {
                                            'click': function () {
                                                this.get_data().set_activeUser(this.get_dataSource());
                                            }
                                        },
                                        controls: [
                                            {
                                                type: 'entityImage',
                                                width: '25',
                                                height: '25',
                                                valign: 'middle',
                                                id: 'avatar'
                                            },
                                            {
                                                id: 'namePanel',
                                                type: 'panel',
                                                orientation: 'vertical',
                                                margin: '5 0 0 0',
                                                bindings: {
                                                    'name': 'name',
                                                    'login': 'login'
                                                },
                                                controls: [
                                                    {
                                                        type: 'label',
                                                        id: 'name',
                                                        width: '*',
                                                        height: '*',
                                                        cssClass: 'user_name'
                                                    },
                                                    {
                                                        type: 'label',
                                                        id: 'login',
                                                        width: '*',
                                                        height: '*',
                                                        cssClass: 'user_login'
                                                    }
                                                ]
                                            }
                                        ]
                                    },
                                    onLoad: function () {
                                        var ds = this.get_data().get_awaitingApprovalUsers();

                                        this.set_dataSource(ds);
                                        this.parent.parent.pager.set_dataSource(ds);

                                        ds.load();
                                    }
                                }
                            ]
                        },
                        {
                            id: 'pager',
                            type: 'pager'
                        }
                    ]
            }
            ]
        }
    ]
})