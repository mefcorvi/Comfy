({ // start of page markup
    title: "Departments",
    layout: 'stack',
    orientation: 'vertical',
    valign: 'stretch',
    cssClass: 'departments_page',

    data: [
        'departmentsFilter',
        'editedDepartment',
        'departmentDataSource',
    ],
        
    controls: [
    {
        type: 'popup',
        width: '300',
        height: '150',
        title: 'Edit Department',
        buttons: ['Save', 'Cancel'],
        controls: [
            {
                id: 'departmentEdit',
                type: 'departmentEdit',
                width: '100%',
                height: '100%'
            }
        ],
        bindings: {
            'data:editedDepartment': function (sender, args) {
                if (args.newValue) {
                    this.container.departmentEdit.set_dataSource(args.newValue);
                    this.open();
                } else {
                    this.close();
                }
            }
        },
        onOpen: function () {
            var department = this.get_data().get_editedDepartment();

            if (department && !department.isNew()) {
                this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                this.set_title('Edit department: ' + department.get_name());
            } else {
                this.set_buttons(['Save', 'Cancel']);
                this.set_title('New department');
            }
        },
        onCommand: function (sender, args) {
            var entity = this.container.departmentEdit.get_dataSource();

            if (args.button == 'Save') {
                if(entity.isNew()) {
                    var contract = { name: entity.get_name(), isArchived: entity.get_isArchived() };
                    Services.DepartmentService.CreateDepartment(contract, function () {
                        this.get_data().set_editedDepartment(null);
                        this.get_data().departmentDataSource.load();
                    }.bind(this));
                } else {
                    var contract = { departmentId : entity.get_id(), name: entity.get_name(), isArchived: entity.get_isArchived() };
                    Services.DepartmentService.UpdateDepartment(contract, function () {
                        this.get_data().set_editedDepartment(null);
                        this.get_data().departmentDataSource.load();
                    }.bind(this));
                }

                return;
            }

            if (args.button == 'Cancel' && !entity.isNew()) {
                Repository.Reload(entity);
            }

            if (args.button == 'Delete' && !entity.isNew()) {
                Application.showConfirm('Warning', 'Do you really want to delete this department?', function (result) {
                    if (result) {
                        Repository.Delete(entity);
                        this.get_data().set_editedDepartment(null);
                    }
                } .bind(this));

                return;
            }

            this.get_data().set_editedDepartment(null);
        }
    },    
    {
        type: 'panel',
        layout: 'stack',
        width: '100%',
        orientation: 'horizontal',
        controls: [
            {
                type: 'collapsablePanel',
                title: 'Departments',
                width: '3*',
                height: '100%',
                margin: '0 0 3 0',
                showCollapseButton: false,
                buttons: ['Add department'],
                showStatusBar: true,
                cssClass: 'departments_block',
                onCommand: function (sender, args) {
                    if (args.button == 'Add department') {
                        this.get_data().set_editedDepartment(new Department());
                    }
                },
                controls: [
                    {
                        type: 'scrollablePanel',
                        horizontal: true,
                        vertical: false,
                        controls: [
                            {
                                type: 'repeater',
                                layout: 'wrap',
                                padding: '5',
                                width: '?',
                                height: '100%',
                                orientation: 'vertical',
                                emptyDataText: 'Departments have been not found',
                                template: {
                                    type: 'panel',
                                    width: '300px',
                                    height: '25px',
                                    border: '1',
                                    padding: '3',
                                    orientation: 'horizontal',
                                    cssClass: 'department_item',
                                    domHandlers: {
                                        click: function () {
                                            this.get_data().set_editedDepartment(this.get_dataSource());
                                        },
                                        mouseover: function () {
                                            this.addCssClass('department_item_hover');
                                        },
                                        mouseout: function () {
                                            this.removeCssClass('department_item_hover');
                                        }
                                    },
                                    bindings: {
                                        'name': 'departmentName',
                                        'isArchived': function(sender,args) {
                                            var isArchived = args.newValue;
                                            if(isArchived) {
                                                this.departmentName.addCssClass('department_name_archived');
                                            } else {
                                                this.departmentName.removeCssClass('department_name_archived');
                                            }
                                        }
                                    },
                                    controls: [
                                        {
                                            id: 'departmentName',
                                            type: 'label',
                                            width: '*',
                                            cssClass: 'department_name',
                                            text: ''
                                        }
                                    ]
                                },
                                onFree: function () {
                                    this.get_dataSource().dispose();
                                },
                                onLoad: function () {
                                    var ds = [].makeDataSource('Department').orderBy('name asc').filterBy('');
                                    this.set_dataSource(ds);
                                    ds.load();
                                    this.get_data().departmentDataSource = ds;
                                }
                            }
                        ]
                    }
                ]
            }
        ]
    }]
})
