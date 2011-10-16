({
    title: "Security",
    orientation: 'horizontal',
    cssClass: 'security_page',
    data: [
        'activeWorkgroup',
        'editedRole',
        'workgroupFilter'
    ],

    onLoad: function() {},

    controls:
    [
        { // Popup for a role editing
            id: 'rolePopup',
            type: 'popup',
            width: '350',
            height: '70%',
            title: 'Edit Role',
            controls: [{ type: 'rolePermissions', id: 'permissions' }],
            bindings: {
                'data:editedRole': function(sender, args) {
                    if (args.newValue) {
                        this.container["permissions"].set_dataSource(args.newValue);
                        this.set_title('Edit role: ' + args.newValue.get_name());

                        if (!this.get_opened()) {
                            this.open();
                        }
                    } else {
                        this.close();
                    }
                }
            },
            onOpen: function() {
                var editedRole = this.container['permissions'].get_dataSource();

                // we should change button set if popup opened for a new role
                if (editedRole.get_id()) {
                    this.set_buttons(['Save', 'Close', { text: 'Delete', cssClass: 'link_button red_button'}]);
                } else {
                    this.set_title('Add new role');
                    this.set_buttons(['Save', 'Close']);
                }
            },
            onCommand: function(sender, args) {
                var data = this.parent.get_data();

                // save the role
                if (args.button === 'Save') {
                    var editedRole = this.container['permissions'].get_dataSource();

                    if (!editedRole.get_workgroupId()) {
                        var workgroup = data.get_activeWorkgroup();
                        editedRole.set_workgroupId(workgroup.get_id());
                        workgroup.get_roles().add(editedRole);
                    }

                    this.container['permissions'].saveData();
                }

                // delete the role
                if (args.button === 'Delete') {
                    var editedRole = this.container['permissions'].get_dataSource();

                    Application.showConfirm('Warning!', 'Do you really want to delete role "' + editedRole.get_name() + '"? All it related data will be also deleted.', function(result) {
                        if (result) {
                            Repository.Delete(editedRole);
                            data.set_editedRole(null);
                        }
                    });

                    return;
                }

                data.set_editedRole(null);
            }
        },
        {
            id: "content",
            type: "panel",
            orientation: 'vertical',
            margin: '0 0 3 0',
            cssClass: "security_page_content",
            bindings: {
                'data:activeWorkgroup': 'workgroupPanel'
            },
            controls:
            [
                {
                    type: "pageHeader",
                    title: "Security Management",
                    description: "Some security management text"
                },
                {
                    id: 'workgroupPanel',
                    type: "collapsablePanel",
                    title: "Workgroup",
                    height: "70",
                    bindings: {
                        'name': 'title',
                        '*': '*'
                    },
                    controls:
                    [
                        {
                            type: 'panel',
                            layout: 'stack',
                            orientation: 'horizontal',
                            padding: '5',
                            bindings: {
                                'data:activeWorkgroup': function() {
                                    this.btnSaveWorkgroup.set_enabled(false);
                                },
                                'name': 'txtWorkgroupName'
                            },
                            controls: [
                                {
                                    type: 'label', text: 'Workgroup name: ', width: 105, height: 18, valign: 'middle'
                                },
                                {
                                    type: 'textBox',
                                    width: "50%-105",
                                    height: '24',
                                    valign: 'middle',
                                    id: 'txtWorkgroupName',
                                    onKeyPress: function(sender, args) {
                                        this.parent['btnSaveWorkgroup'].set_enabled(true);
                                    }
                                },
                                {
                                    type: 'button', text: 'Save', width: 80,
                                    valign: 'middle',
                                    id: 'btnSaveWorkgroup',

                                    onClick: function() {
                                        var wg = this.get_data().get_activeWorkgroup();
                                        Repository.Save(wg);
                                        this.set_enabled(false);
                                    }
                                },
                                {
                                    type: 'button',
                                    valign: 'middle',
                                    width: '80',
                                    cssClass: 'red_button link_button',
                                    text: 'Delete',

                                    onClick: function() {
                                        var wg = this.get_data().get_activeWorkgroup();

                                        Application.showConfirm('Warning!', 'Do you really want to delete workgroup "' + wg.get_name() + '"? All it related data will be also deleted.', function(result) {
                                            if (result) {
                                                Repository.Delete(wg);
                                            }
                                        });
                                    }
                                }
                            ]
                        }
                    ]
                },
                {
                    id: "wokgroupPanel",
                    type: "panel",
                    controls: [
                        {
                            type: "collapsablePanel",
                            orientation: 'horizontal',
                            title: "Roles",
                            controls: [
                                {
                                    type: 'scrollablePanel',
                                    bindings: {
                                        'data:activeWorkgroup': function(sender, args) {
                                            this.repeater.set_dataSource(args.newValue ? args.newValue.get_roles() : [].makeObservable());
                                        }
                                    },
                                    controls: [
                                        { // Roles List
                                            type: 'repeater',
                                            id: 'repeater',
                                            layout: 'wrap',
                                            width: '*',
                                            padding: '5',
                                            height: '?',
                                            orientation: 'horizontal',
                                            cssClass: 'entity_cards',
                                            template: { // Roles Card
                                                type: 'entityCard',
                                                width: '50%',
                                                height: '110',
                                                margin: '0 0 5 5',
                                                minWidth: '100',
                                                maxWidth: '300',

                                                onClick: function() {
                                                    Repository.LoadRole(this.get_dataSource().get_id(), function(result) {
                                                        this.get_data().set_editedRole(result);
                                                    } .bind(this));
                                                },
                                                
                                                customFunctions: {
                                                    showPermissions: function(items) {
                                                        var objects = items.select(function(item) {
                                                            return item.get_permissionObjectId();
                                                        });
                                                        
                                                        Repository.Get('PermissionObject', objects, function(result) {
                                                            var permissions = items.select(
                                                                function(item) {
                                                                    return item.get_permissionLevel() + ' <b>' + 
                                                                        Repository.GetCached('PermissionObject', item.get_permissionObjectId()).get_name() + '</b>';
                                                                }
                                                            );

                                                            if (permissions.length > 0) {
                                                                this['permissions'].set_text('<i>Can ' + permissions.join(', ') + '</i>');
                                                            } else {
                                                                this['permissions'].set_text('<i>Can do nothing</i>');
                                                            }
                                                        }.bind(this));   
                                                    }
                                                },

                                                bindings: {
                                                    '*': 'titlePanel',
                                                    
                                                    'permissions': function(sender, args) {
                                                        var value = args.newValue;
                                                        this.showPermissions(value);
                                                    },

                                                    'permissions.changed': function(sender, args) {                                                        
                                                        this.showPermissions(sender);
                                                    }
                                                },

                                                controls: [
                                                    {
                                                        type: "panel",
                                                        id: "titlePanel",
                                                        height: '20',
                                                        cssClass: "workgroup_header",
                                                        controls: [
                                                            { type: "label", text: "", id: "nameBox", cssClass: "workgroup_name", width: '*' },
                                                            {
                                                                type: "label",
                                                                text: "",
                                                                width: '*',
                                                                height: '*',
                                                                id: "levelBox",
                                                                cssClass: "workgroup_level",
                                                                bindings: {
                                                                    '*': function(sender, args) {
                                                                        var value = args.newValue;
                                                                        this.set_text('(' + value + ')');
                                                                    }
                                                                }
                                                            }
                                                        ],
                                                        
                                                        bindings: {
                                                            'name' : 'nameBox',
                                                            'level' : 'levelBox'
                                                        }
                                                    },
                                                    { type: "literal", id: 'permissions', height: '*', width: '*', text: "", dataType: 'html' }
                                                ]
                                            },
                                            onLoad: function() {
                                                // create "Add role" button if it isn't exist yet
                                                if (!this.__addRoleButton) {
                                                    var control = ControlsFactory.create("button");
                                                    control.initFromOptions({
                                                        text: 'Add new role',
                                                        width: 136,
                                                        height: 40,
                                                        maxHeight: 40,
                                                        halign: 'center',
                                                        valign: 'middle',
                                                        cssClass: 'entity_card_button',
                                                        onClick: function() {
                                                            var role = new Role();
                                                            role.set_name('');
                                                            role.set_level(0);

                                                            this.get_data().set_editedRole(role);
                                                        }
                                                    });

                                                    this.__addRoleButton = control;
                                                    this.controls.add(control, { alwaysLast: true });
                                                }
                                            }
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        },
        { // ================= Tab Panel =================
            type: "tabPanel",
            width: "240px",
            tabs:
            [
                { // ================== Workgroups & Roles =================
                    title: "Workgroups & Roles",
                    controls:
                    [
                        {
                            type: 'scrollablePanel',
                            controls:
                            [
                                {
                                    type: "tree",
                                    padding: '5',
                                    cssClass: 'workgroup_tree',
                                    childProperty: 'roles',
                                        emptyDataTemplate: {
                                             type: 'label',
                                             cssClass: 'data_not_found_label',
                                             text: 'Workgroups have not been found'
                                        },
                                        nodeTemplate: {
                                            bindings: {
                                                'name': 'name'
                                            },
                                            controls: [
                                                {
                                                    type: "link",
                                                    text: "",
                                                    id: "name",
                                                    height: '20',
                                                    padding: '3',
                                                
                                                    bindings: { '*': 'text' },
                                                
                                                    onClick: function() {
                                                        var data = this.parent.get_dataSource();

                                                        if (data instanceof Role) {
                                                            Repository.Get('Workgroup', data.get_workgroupId(), function(entity) {
                                                                this.get_data().set_activeWorkgroup(entity);                                                        
                                                            }.bind(this));

                                                            Repository.LoadRole(data.get_id(), function(result) {
                                                                this.get_data().set_editedRole(result);
                                                            } .bind(this));

                                                            return;
                                                        }

                                                        this.get_data().set_activeWorkgroup(data);
                                                    }
                                                }
                                            ]
                                        },
                                    
                                    onLoad: function() {
                                        var ds = [].makeDataSource('Workgroup').filterBy('').orderBy('Name');                                        

                                        ds.add_changed(function(sender, args) {
                                            if (!this.get_dataSource()) {
                                                return;
                                            }
                                            
                                            var data = this.get_data();
                                            var activeWorkgroup = data.get_activeWorkgroup();
                                        
                                            var entity = this.get_dataSource().first();
                                            
                                            if (entity && !this.get_dataSource().contains(activeWorkgroup)) {
                                                this.get_data().set_activeWorkgroup(entity);
                                            }
                                        }, this);
                                        
                                        this.set_dataSource(ds);
                                        
                                        ds.load();
                                        
                                        this.__workgroupFilterChanged = function(sender, args) {
                                            this.get_dataSource().set_filterString(args.newValue);
                                            this.get_dataSource().load();
                                        };

                                        this.get_data().add_workgroupFilterChanged(this.__workgroupFilterChanged, this);
                                    },
                                    
                                    onFree: function() {
                                        this.get_data().remove_workgroupFilterChanged(this.__workgroupFilterChanged, this);
                                        
                                        //TODO: Think about ds disposing
                                        if(this.get_dataSource())
                                            this.get_dataSource().dispose();
                                    }
                                }
                            ]
                        },
                        { // =============== Search Pane ===================
                            type: "searchPane",
                            onSearch: function(sender, args) {
                                var query = args.query;

                                if (!query) {
                                    query = '';
                                } else {
                                    query = 'Name StartsWith "' + args.query + '"';
                                }

                                this.get_data().set_workgroupFilter(query);
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
                                    text: "Add new role",
                                    width: '*',
                                    height: '20',
                                    valign: 'middle',
                                    cssClass: 'add_new_role',
                                    onClick: function() {
                                        var role = new Role();
                                        role.set_name('');
                                        role.set_level(0);

                                        this.get_data().set_editedRole(role);
                                    }
                                },
                                {
                                    type: "link",
                                    text: "Add new group",
                                    width: '*',
                                    height: '20',
                                    valign: 'middle',
                                    cssClass: 'add_new_group',
                                    onClick: function() {
                                        var workgroup = new Workgroup();
                                        workgroup.set_name('New Workgroup');
                                        window.debug = true;

                                        Repository.Save(workgroup);

                                        this.get_data().set_activeWorkgroup(workgroup);
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