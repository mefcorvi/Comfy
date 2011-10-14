({
    title: "Welcome",
    cssClass: 'register_page',

    data: [
        'currentUser'
    ],

    onLoad: function () {
        if (Application.get_currentUser()) {
            Application.loadPage('');
        }

        Services.UsersService.GetFromAD('', function (user) {
            if (user) {
                this.get_data().set_currentUser(user);
            } else {
                Application.showError('Error', 'Cannot find user in the AD');
            }
        } .bind(this));
    },

    controls:
    [
        {
            type: 'wizard',

            onFinish: function () {
                var currentUser = this.get_data().get_currentUser();

                Services.UsersService.Register(currentUser, function () {
                    Application._removeUpdatingEvent();
                    document.location.reload(true);
                });
            },

            onStepChanged: function (sender, args) {
                if (args.newValue === 0) {
                    this.getStep(0).edit();
                }

                if (args.oldValue === 0) {
                    this.getStep(0).save();
                }
            },

            steps: [
                {
                    title: 'Configure Profile',
                    description: 'Specification of the general user details that are necessary for further operations with ' +
                                 'the system or simplifying the system use. For LGE domain users a part of data is read from LGE Active Directory.',
                    type: 'profileDetails',
                    padding: '5px',
                    cssClass: 'user_details',
                    onInit: function () {
                        this.get_data().add_currentUserChanged(function (sender, args) {
                            this.set_dataSource(args.newValue);
                        }, this);
                    }
                },
                {
                    title: 'Select Roles',
                    description: 'Declaration of the user roles at the existing system workgroups that user expect to have. ' +
                                 'The roles self-assignment will be approved later by the responsible approval manager',
                    controls: [
                        {
                            type: 'userRolesEdit',
                            id: 'userRolesEdit',
                            showConstraints: false,
                            emptyText: 'Please select some roles that you expect to have',
                            width: '100%',
                            height: '100%',
                            onRoleAdding: function (sender, args) {
                                var role = args.role;
                                role.set_isApproved(false);

                                this.get_dataSource().add(args.role);
                            },
                            onRoleDeleting: function (sender, args) {
                                this.get_dataSource().remove(args.role);
                            },
                            onInit: function () {
                                this.get_data().add_currentUserChanged(function (sender, args) {
                                    this.set_dataSource(args.newValue.get_roles());
                                }, this);
                            }
                        }
                    ]
                },
                {
                    title: 'Confirm',
                    description: 'Submitting the registration request to the list of approval managers. ' +
                                 'System access will be granted after the approval has been processed.',
                    controls: [
                        {
                            type: 'collapsablePanel',
                            height: '*',
                            scrolling: true,
                            title: 'Profile Details',
                            padding: '5',
                            margin: '0 0 0 3',
                            controls: [
                                {
                                    type: 'profileDetails',
                                    cssClass: 'user_details',
                                    height: '?',
                                    onInit: function () {
                                        this.get_data().add_currentUserChanged(function (sender, args) {
                                            this.set_dataSource(args.newValue);
                                        }, this);
                                    }
                                }
                            ]
                        },
                        {
                            type: 'collapsablePanel',
                            title: 'Roles',
                            height: '*',
                            scrolling: true,
                            padding: '5',
                            controls: [
                                {
                                    type: 'userRoles',
                                    height: '?',
                                    onInit: function () {
                                        this.get_data().add_currentUserChanged(function (sender, args) {
                                            this.set_dataSource(args.newValue.get_roles());
                                        }, this);
                                    }
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]
})