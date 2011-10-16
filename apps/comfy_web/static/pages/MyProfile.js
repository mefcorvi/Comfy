({
    title: "My Profile",
    orientation: 'vertical',
    cssClass: 'my_profile_page',
    layout: 'grid',
    columns: ['*', '*', '*'],
    rows: ['?', '?', '*'],

    data: [
        'editRolesState'
    ],

    controls: [
        {
            type: "pageHeader",
            title: "My Profile",
            'grid.row': 1,
            'grid.column': 1,
            width: '300%'
        },
        { // ======== Popup "Edit Roles"
            type: 'popup',
            title: 'Edit Roles',
            width: '245',
            height: '50%',
            buttons: ['OK'],
            controls: [
                {
                    type: 'userRolesEdit',
                    id: 'userRolesEdit',
                    mode: 'currentUser',
                    width: '100%',
                    height: '100%',
                    onRoleAdding: function (sender, args) {
                        Repository.Save(args.role);
                    },
                    onRoleDeleting: function (sender, args) {
                        Repository.Delete(args.role);
                    }
                }
            ],
            onLoad: function () {
                var ds = [].makeDataSource('UserRole', null, [ ConstraintedUserRole ]).filterBy('');
                this.container.userRolesEdit.set_dataSource(ds);

                this.parent.get_data().add_editRolesStateChanged(function (sender, args) {
                    if (args.newValue) {
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onOpen: function () {
                var editRoles = this.container.userRolesEdit;
                var userId = Application.get_context().get('currentUser').get_id();
                editRoles.set_userId(userId);
                editRoles.get_dataSource().set_filterString('Project = null AND User.Id = ' + userId);
                editRoles.get_dataSource().load();
            },
            onCommand: function (sender, args) {
                this.parent.get_data().set_editRolesState(false);
            }
        },        
        { // <<< User Details
            type: "collapsablePanel",
            title: "Details",
            width: "300%",
            height: '260',
            'grid.column': 1,
            'grid.row': 2,
            padding: '10px',
            margin: '0 0 0 2',
            cssClass: 'user_details',
            orientation: 'horizontal',
            controls: [
                {
                    type: 'panel',
                    width: 615,
                    height: '100%',
                    orientation: 'vertical',
                    cssClass: 'user_data',
                    controls: [
                        { // ======= User Data
                            type: 'profileDetails',
                            id: 'profileDetails',
                            width: '100%',
                            height: '100%-50',
                            onLoad: function () {
                                this.set_dataSource(Application.get_context().get('currentUser'));
                            }
                        },
                        {
                            type: 'panel',
                            width: '100%',
                            height: '50',
                            orientation: 'horizontal',
                            controls: [
                                {
                                    type: 'button',
                                    width: '?',
                                    text: 'Edit Details',
                                    id: 'btnEdit',
                                    onClick: function () {
                                        var detailsControl = this.parent.parent.profileDetails;

                                        if (!detailsControl.get_editMode()) {
                                            detailsControl.edit();
                                            this.parent.btnCancel.show();
                                            this.set_text('Save Details');
                                        } else {
                                            this.parent.btnCancel.hide();
                                            detailsControl.save(true);
                                            this.set_text('Edit Details');
                                        }
                                    }
                                },
                                {
                                    type: 'button',
                                    width: '?',
                                    visible: false,
                                    text: 'Cancel',
                                    id: 'btnCancel',
                                    onClick: function () {
                                        this.parent.parent.profileDetails.cancel();
                                        this.hide();
                                        this.parent.btnEdit.set_text('Edit Details');
                                    }
                                }
                            ]
                        }
                    ]
                },
                { // ======= User Photo
                    type: 'panel',
                    cssClass: 'user_photo',
                    width: '*',
                    height: '*',
                    orientation: 'vertical',
                    controls: [
                        {
                            type: 'entityImage',
                            width: '150',
                            height: '150',
                            imageId: -1,
                            onLoad: function () {
                                var imageChanged = function (sender, args) {
                                    this.set_imageId(args.newValue);
                                };

                                var currentUser = Application.get_context().get('currentUser');
                                currentUser.add_imageIdChanged(imageChanged, this);

                                this.set_imageId(currentUser.get_imageId());
                            }
                        },
                        {
                            type: 'panel',
                            orientation: 'horizontal',
                            height: '100%',
                            controls: [
                                {
                                    type: 'fileUpload',
                                    height: '33',
                                    width: '130',
                                    text: 'Change',
                                    onComplete: function (sender, args) {
                                        var user = Application.get_context().get('currentUser');
                                        user.set_imageId(args.id);

                                        Repository.Save(user);
                                    }
                                },
                                {
                                    id: 'buttonPhotoSilverlight',
                                    type: 'silverlight',
                                    url: 'silverlight:Phoenix.PhotoUploadButton.xap',
                                    width: '10',
                                    height: '10',
                                    onSlLoad: function (sender, args) {
                                        var checkCamera = function() {
                                            if (!this.get_content() || !this.get_content().Content) {
                                                window.setTimeout(checkCamera, 500);
                                                return;
                                            }

                                            if (this.get_content().Content.IsCameraAvailable())
                                            {
                                                this.parent.photoButton.set_visible(true);
                                                this.parent.photoButton.update();
                                            }
                                        }.bind(this);

                                        checkCamera();
                                    }
                                },
                                {
                                    type: 'button',
                                    id: 'photoButton',
                                    text: 'Take a photo',
                                    width: '130',
                                    height: '33',
                                    visible: false,                                    
                                    onClick: function(sender, args) {
                                        this.photoPopup.open();
                                    },
                                    controls: [                                                
                                        {
                                            type: 'popup',
                                            id: 'photoPopup',
                                            height: '320',
                                            width: '320',
                                            controls: [
                                                {
                                                    type: 'photoUpload', 
                                                    id: 'photoUpload',
                                                    heigth: '100%',
                                                    width: '100%',
                                                    onComplete: function (sender, args) {                                                                
                                                        var user = Application.get_context().get('currentUser');
                                                        user.set_imageId(args.id);
                                                        Repository.Save(user);                                 
                                                                
                                                        var button = this.parent.parent.parent.parent.parent;
                                                        button.set_enabled(true);
                                                        button.set_text('Take a photo');
                                                        this.parent.parent.parent.parent.close();                                                                                    
                                                    },
                                                    onProgress: function (sender, args) {
                                                        var button = this.parent.parent.parent.parent.parent;
                                                        button.set_enabled(false);
                                                        button.set_text('Uploading... ' + args.progress + '%');
                                                    },
                                                    onShotClicked: function(sender, args) {
                                                        this.parent.parent.parent.parent.set_width(0);
                                                        this.parent.parent.parent.parent.set_height(0);
                                                        //this.parent.parent.parent.parent.close();   this line causes FF and Chrome kill SL control... and it can't upload a photo
                                                    },
                                                    onFatalError: function (sender, args) {
                                                        this.parent.parent.parent.parent.close();
                                                        Application.showError(args.caption, args.text);                                                                
                                                    }
                                                }
                                            ],
                                            onOpen: function(sender, args) {      
                                                this.set_width(320);
                                                this.set_height(320);
                                                this.container.photoUpload.init();
                                            },
                                            onClose: function(sender, args) {                                                            
                                                this.container.photoUpload.deInit();                                                                                                                
                                            }                                                    
                                        }
                                    ]
                                }
                            ]
                        }                                
                    ]
                }
            ]
        }, // User Details >>>
        { // <<< My Roles
            type: "collapsablePanel",
            title: "Roles",
            margin: '0 0 3 0',
            'grid.column': 1,
            'grid.row': 3,
            showCollapseButton: false,
            cssClass: 'roles_panel',
            controls: [
                {
                    type: 'scrollablePanel',
                    controls: [
                        {
                            type: 'userRoles',
                            width: '*',
                            height: '?',
                            padding: '5',
                            onLoad: function () {
                                var user = Application.get_context().get('currentUser');
                                this.set_dataSource(user.get_roles());
                            }
                        }
                    ]
                },
                {
                    type: 'button',
                    text: 'Request More Roles',
                    onClick: function () {
                        this.get_data().set_editRolesState(true);
                    }
                }
            ]
        }, // My Roles >>>
        { // <<< Activity
            type: "collapsablePanel",
            title: "Activity",
            padding: '5px',
            margin: '0 0 3 0',
            'grid.column': 2,
            'grid.row': 3,
            showCollapseButton: false,            
            orientation: 'vertical',

            controls: [
                {
                    id: 'feedbacks',
                    type: 'panel',
                    width: '100%',
                    height: '25',
                    orientation: 'horizontal',                    
                    controls: [
                        {
                            type: 'label',
                            text: 'Feedbacks:',
                            cssClass: 'form_label',
                            width: '120'
                        },
                        {
                            id: 'link',
                            type: 'link',
                            width: '*',
                            height: '*',                            
                            onLoad: function() {
                                Services.UsersService.GetFeedbackStatistics(function(result) {
                                    this.set_text(result._resolved + '/' + result._all + ' Resolved');
                                }.bind(this));
                            },
                            onClick: function() {
                                Application.get_masterPage().showUserFeedbacks();
                            }
                        }
                    ]
                },
                {
                    id: 'notifications',
                    type: 'panel',
                    width: '100%',
                    height: '25',
                    orientation: 'horizontal',                    
                    controls: [
                        {
                            type: 'label',
                            text: 'Notifications:',
                            cssClass: 'form_label',
                            width: '120'
                        },
                        {
                            id: 'link',
                            type: 'link',
                            width: '*',
                            height: '*',        
                            bindings: {
                                'messageStatistics': function(sender, args) {
                                    var statistics = args.newValue;
                                    this.set_text(statistics._read + '/' + statistics._all + ' Read');
                                }
                            },                            
                            onLoad: function() {                                
                                this.set_dataSource(Application.get_currentUser());
                            },
                            onClick: function() {
                                Application.get_masterPage().showEventList();
                            }
                        }
                    ]
                }
            ]
        }, // Activity >>>
        { // <<< Preferences
            type: "collapsablePanel",
            title: "Preferences",
            padding: '5px',
            'grid.column': 3,
            'grid.row': 3,
            showCollapseButton: false,
            cssClass: 'preferences',
            orientation: 'vertical',
            bindings: {
                'startPage': function (sender, args) {
                    var value = args.newValue;
                    this.innerPanel.startPage.set_dataSource(value);
                }
            },
            onLoad: function () {
                var user = Application.get_currentUser();
                var countries = [].makeDataSource('Country').filterBy('TimeZone != null').orderBy('name');
                this.innerPanel.firstCountry.set_dataSource(countries);
                this.innerPanel.secondCountry.set_dataSource(countries);
                this.set_dataSource(user);

                countries.load(function (result) {
                    var countryId = user.get_countryId();
                    var secondCountryId = user.get_secondCountryId();

                    var firstCountry = countryId ? Repository.GetCached('Country', countryId) : null;
                    var secondCountry = secondCountryId ? Repository.GetCached('Country', secondCountryId) : null;

                    this.innerPanel.firstCountry.list.set_selectedValue(firstCountry);
                    this.innerPanel.secondCountry.list.set_selectedValue(secondCountry);
                } .bind(this));
            },
            controls: [ // TODO: First and second country dropdownlists should be extracted to separate control
                {
                    id: 'firstCountry',
                    type: 'panel',
                    width: '100%',
                    height: '25',
                    orientation: 'horizontal',
                    bindings: {
                        '*': 'list'
                    },
                    controls: [
                        {
                            type: 'label',
                            text: 'Country:',
                            cssClass: 'form_label',
                            width: '120'
                        },
                        {
                            id: 'list',
                            type: 'dropDownList',
                            width: '?',
                            bindings: {
                                '*': 'items'
                            },
                            template: {
                                type: 'panel',
                                height: '20',
                                padding: '2',
                                orientation: 'horizontal',
                                cssClass: 'myProfile_country_item',
                                controls: [
                                    {
                                        id: 'imageId',
                                        type: 'entityImage',
                                        mode: 'exactly',
                                        width: '25px',
                                        height: '15px',
                                        margin: '0 0 3 0'
                                    },
                                    {
                                        id: 'label',
                                        type: 'label',
                                        height: '*'
                                    }
                                ],
                                bindings: {
                                    'imageId': 'imageId',
                                    'name': 'label'
                                }
                            },
                            notSelectedItem: {
                                name: '-',
                                imageId: -1
                            },
                            textProperty: 'name'
                        }
                    ]
                },
                {
                    id: 'secondCountry',
                    type: 'panel',
                    width: '100%',
                    height: '25',
                    orientation: 'horizontal',
                    bindings: {
                        '*': 'list'
                    },
                    controls: [
                        {
                            type: 'label',
                            text: 'Second country:',
                            cssClass: 'form_label',
                            width: '120'
                        },
                        {
                            id: 'list',
                            type: 'dropDownList',
                            width: '?',
                            bindings: {
                                '*': 'items'
                            },
                            template: {
                                type: 'panel',
                                cssClass: 'myProfile_country_item',
                                height: '20',
                                padding: '2',
                                orientation: 'horizontal',
                                controls: [
                                    {
                                        type: 'entityImage',
                                        bindings: {
                                            'imageId': 'imageId'
                                        },
                                        mode: 'exactly',
                                        width: '25px',
                                        height: '15px',
                                        margin: '0 0 3 0'
                                    },
                                    {
                                        type: 'label',
                                        height: '*',
                                        bindings: {
                                            'name': 'text'
                                        }
                                    }
                                ],
                                bindings: {
                                    '*': '*'
                                }
                            },
                            notSelectedItem: {
                                name: '-',
                                imageId: -1
                            },
                            textProperty: 'name'
                        }
                    ]
                },
                {
                    id: 'startPagePopup',
                    title: 'Select start page',
                    type: 'popup',
                    width: '300',
                    scrolling: true,
                    height: '80%',
                    buttons: ['OK', 'Cancel'],
                    onCommand: function (sender, args) {
                        if (args.button == 'OK') {
                            var user = Application.get_currentUser();
                            var item = this.container.selector.get_selectedItem();

                            user.set_startPage(item ? item.href : null);

                            if (item) {
                                Application.get_configuration().set_startPageUri(item.href.replace('page:', ''));
                            }

                            Repository.Save(user);
                        }

                        this.close();
                    },
                    controls: [
                        { id: 'selector', type: 'startPageSelector', width: '100%', height: '100%' }
                    ]
                },
                {
                    id: 'startPage',
                    type: 'panel',
                    width: '100%',
                    height: '25',
                    orientation: 'horizontal',
                    bindings: {
                        '*': function (sender, args) {
                            var value = args.newValue;
                            if (!value) {
                                this.link.set_text('Not Selected');
                            } else {
                                var pageInfo = Application.getPageInfoByUrl(value);
                                this.link.set_text(pageInfo ? pageInfo.title : 'Not Selected');
                            }
                        }
                    },
                    controls: [
                        {
                            type: 'label',
                            text: 'Start Page:',
                            cssClass: 'form_label',
                            width: '120'
                        },
                        {
                            id: 'link',
                            type: 'link',
                            width: '*',
                            height: '*',
                            text: 'Not Selected',
                            onClick: function () {
                                var user = Application.get_currentUser();
                                var popup = this.parent.parent.startPagePopup;
                                popup.container.selector.set_curentStartPage(user.get_startPage());
                                popup.open();
                            }
                        }
                    ]
                },
                {
                    id: 'notifications',
                    type: 'panel',
                    width: '100%',
                    height: '25',
                    orientation: 'horizontal',
                    controls: [
                        {
                            type: 'label',
                            text: 'Notification:',
                            cssClass: 'form_label',
                            width: '120'
                        },
                        {
                            id: 'link',
                            type: 'link',
                            width: '*',
                            height: '*',
                            text: 'Browser/Email',
                            onClick: function () {
                                var popup = this.parent.notificationsPopup;                                
                                popup.open();
                            }
                        },
                        {
                            id: 'notificationsPopup',
                            width: '350',
                            height: '?',
                            title: 'Notifications',
                            type: 'popup',
                            uri: 'notificationSettings',
                            buttons: [ 'OK', 'Cancel' ],
                            onCommand: function (sender, args) {
                                if (args.button == 'OK') {                                                                     
                                    Repository.Save(this.$get_page().eventsScroll.eventsRepeater.get_dataSource());
                                }

                                this.close();   
                            }                            
                        }
                    ]
                },
                {
                    type: 'button',
                    text: 'Save',
                    onClick: function (sender, args) {
                        var firstCountry = this.parent.firstCountry.list.get_selectedValue();
                        var secondCountry = this.parent.secondCountry.list.get_selectedValue();

                        var user = Application.get_context().get('currentUser');
                        user.set_countryId(firstCountry ? firstCountry.get_id() : 0);
                        user.set_secondCountryId(secondCountry ? secondCountry.get_id() : 0);

                        Repository.Save(user);
                    }
                }
            ]
        } // Preferences >>>
    ]
})