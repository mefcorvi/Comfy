({
    title: "Operators",
    layout: 'stack',
    orientation: 'vertical',
    valign: 'stretch',
    cssClass: 'operators_page',

    data: [
        'editedLocalOperator',
        'editedGlobalOperator',
        'editedSubsidiaryOperator',
        'localOperatorsFilter',
        'globalOperatorsFilter',
        'subsidiaryOperatorsFilter',
        'selectedGlobalOperator'
    ],
    onLoad: function () { },

    customFunctions: {
        'deleteGlobalOperator': function (operator) {
            Application.showConfirm('Warining', 'Do you really want to delete this global operator?', function (result) {
                if (result) {
                    Repository.Delete(operator);
                    this.get_data().set_editedGlobalOperator(null);
                    this.get_data().set_selectedGlobalOperator(null);
                }
            } .bind(this));
        }
    },

    controls: [
        {
            type: "pageHeader",
            title: "Operators",
            description: "Operators"
        },
        {
            type: 'popup',
            width: '300',
            height: '300',
            title: 'Edit local operator',
            buttons: ['Save', 'Cancel'],
            controls: [
                {
                    id: 'localOperatorEdit',
                    type: 'localOperatorEdit',
                    width: '100%',
                    height: '100%'
                }
            ],
            onOpen: function () {
                var localOperator = this.get_data().get_editedLocalOperator();

                if (localOperator && !localOperator.isNew()) {
                    this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                } else {
                    this.set_buttons(['Save', 'Cancel']);
                }
            },
            onLoad: function () {
                this.get_data().add_editedLocalOperatorChanged(function (sender, args) {
                    if (args.newValue) {
                        this.container.localOperatorEdit.set_dataSource(args.newValue);
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onCommand: function (sender, args) {
                var entity = this.container.localOperatorEdit.get_dataSource();

                if (args.button == 'Save') {
                    this.container.localOperatorEdit.save(function () {
                        this.get_data().set_editedLocalOperator(null);
                    } .bind(this));
                    return;
                }

                if (args.button == 'Cancel' && !entity.isNew()) {
                     this.container.localOperatorEdit.cancel();
                }

                if (args.button == 'Delete' && !entity.isNew()) {
                    Application.showConfirm('Warning', 'Do you really want to delete this local operator?', function (result) {
                        if (result) {
                            Repository.Delete(entity);
                            this.get_data().set_editedLocalOperator(null);
                        }
                    } .bind(this));

                    return;
                }

                this.get_data().set_editedLocalOperator(null);
            }
        },
        {
            type: 'popup',
            width: '300',
            height: '300',
            title: 'Edit subsidiary operator',
            buttons: ['Save', 'Cancel'],
            controls: [
                {
                    id: 'subsidiaryOperatorEdit',
                    type: 'localOperatorEdit',
                    isConstraintsDisabled: true,
                    width: '100%',
                    height: '100%'
                }
            ],
            onOpen: function () {
                var subsidiaryOperator = this.get_data().get_editedSubsidiaryOperator();

                if (subsidiaryOperator && !subsidiaryOperator.isNew()) {
                    this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                } else {
                    this.set_buttons(['Save', 'Cancel']);
                }
            },
            onLoad: function () {
                this.get_data().add_editedSubsidiaryOperatorChanged(function (sender, args) {
                    if (args.newValue) {
                        this.container.subsidiaryOperatorEdit.set_dataSource(args.newValue);
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onCommand: function (sender, args) {
                var entity = this.container.subsidiaryOperatorEdit.get_dataSource();

                if (args.button == 'Save') {
                    this.container.subsidiaryOperatorEdit.save(function () {
                        this.get_data().set_editedSubsidiaryOperator(null);
                    } .bind(this));
                    return;
                }

                if (args.button == 'Cancel' && !entity.isNew()) {
                    this.container.subsidiaryOperatorEdit.cancel();
                }

                if (args.button == 'Delete' && !entity.isNew()) {
                    Application.showConfirm('Warining', 'Do you really want to delete this subsidiary operator?', function (result) {
                        if (result) {
                            Repository.Delete(entity);
                            this.get_data().set_editedSubsidiaryOperator(null);
                        }
                    } .bind(this));

                    return;
                }

                this.get_data().set_editedSubsidiaryOperator(null);
            }
        },
        {
            type: 'popup',
            width: '300',
            height: '300',
            title: 'Edit global operator',
            buttons: ['Save', 'Cancel'],
            controls: [
                {
                    id: 'globalOperatorEdit',
                    type: 'globalOperatorEdit',
                    width: '100%',
                    height: '100%'
                }
            ],
            onOpen: function () {
                var globalOperator = this.get_data().get_editedGlobalOperator();

                if (globalOperator && !globalOperator.isNew()) {
                    this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                } else {
                    this.set_buttons(['Save', 'Cancel']);
                }
            },
            onLoad: function () {
                this.get_data().add_editedGlobalOperatorChanged(function (sender, args) {
                    if (args.newValue) {
                        this.container.globalOperatorEdit.set_dataSource(args.newValue);
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onCommand: function (sender, args) {
                var entity = this.container.globalOperatorEdit.get_dataSource();

                if (args.button == 'Save') {
                    this.container.globalOperatorEdit.save(function () {
                        this.get_data().set_editedGlobalOperator(null);
                    } .bind(this));
                    return;
                }

                if (args.button == 'Cancel' && !entity.isNew()) {
                    this.container.globalOperatorEdit.cancel();
                }

                if (args.button == 'Delete' && !entity.isNew()) {
                    this.get_window().deleteGlobalOperator(entity);
                    return;
                }

                this.get_data().set_editedGlobalOperator(null);
            }
        },
        {
            type: 'panel',
            layout: 'stack',
            width: '100%',
            orientation: 'horizontal',
            controls: [
                {
                    type: 'multiView',
                    width: '*',
                    height: '100%',
                    margin: '0 0 3 0',
                    customFunctions: {
                        'selectedChanged': function (sender, args) {
                            if (args.newValue) {
                                this.set_activeView('subsidiaryOperators');
                            } else {
                                this.set_activeView('localOperators');
                            }
                        }
                    },
                    onLoad: function () {
                        this.get_data().add_selectedGlobalOperatorChanged(this.selectedChanged, this);
                    },
                    onFree: function () {
                        this.get_data().remove_selectedGlobalOperatorChanged(this.selectedChanged, this);
                    },
                    views: [
                        { //++ Local operators
                            id: 'localOperators',
                            type: 'collapsablePanel',
                            title: 'Local operators',
                            width: '100%',
                            height: '100%',
                            showCollapseButton: false,
                            buttons: ['Add operator'],
                            showStatusBar: true,
                            cssClass: 'local_operators_block',
                            onCommand: function (sender, args) {
                                if (args.button == 'Add operator') {
                                    this.get_data().set_editedLocalOperator(new LocalOperator());
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
                                            width: '?',
                                            height: '100%',
                                            orientation: 'vertical',
                                            emptyDataText: 'Local operators have been not found',
                                            template: {
                                                type: 'panel',
                                                width: '300px',
                                                height: '30px',
                                                padding: '3px',
                                                cssClass: 'local_operator_item',
                                                orientation: 'horizontal',
                                                domHandlers: {
                                                    click: function () {
                                                        this.get_data().set_editedLocalOperator(this.get_dataSource());
                                                    },
                                                    mouseover: function () {
                                                        this.addCssClass('local_operator_item_hover');
                                                    },
                                                    mouseout: function () {
                                                        this.removeCssClass('local_operator_item_hover');
                                                    }
                                                },
                                                bindings: {
                                                    'countryId': function (sender, args) {
                                                        var value = args.newValue;
                                                        Repository.Get('Country', value, function (result) {
                                                            this.countryName.set_dataSource(result);
                                                        } .bind(this));
                                                    },
                                                    'name': 'name',
                                                    'code': 'code',
                                                    'displayedImageId': 'image',
                                                    'isArchived': function (sender, args) {
                                                        if (args.newValue) {
                                                            
                                                            this.isArchived.show(); 
                                                        } else {
                                                            this.isArchived.hide(); 
                                                        }
                                                    }
                                                },
                                                controls: [
                                                    {
                                                        id: 'countryName',
                                                        type: 'label',
                                                        margin: '0 0 5 0',
                                                        width: '100',
                                                        height: '18',
                                                        valign: 'middle',
                                                        cssClass: 'country_name',
                                                        bindings: {
                                                            'name': 'text'
                                                        }
                                                    },
                                                    {
                                                        id: 'image',
                                                        type: 'entityImage',
                                                        mode: 'bounded',
                                                        width: 32,
                                                        height: 24
                                                    },
                                                    {
                                                        type: 'label',
                                                        id: 'code',
                                                        width: '30',
                                                        height: '*',
                                                        cssClass: 'operator_code'
                                                    },
                                                    {
                                                        type: 'label',
                                                        id: 'name',
                                                        height: '*',
                                                        width: '*',
                                                        cssClass: 'operator_name'
                                                    },
                                                    {                           
                                                        id: 'isArchived',     
                                                        cssClass: 'archived_star',
                                                        type: 'label',
                                                        width: '8',
                                                        text: '*',       
                                                        visible: false 
                                                    }
                                                ],
                                                onLoad: function () {
                                                },
                                                onFree: function () {
                                                }
                                            },
                                            customFunctions: {
                                                'filterChanged': function (sender, args) {
                                                    this.get_dataSource().set_searchQuery(args.newValue, true);
                                                }
                                            },
                                            onFree: function () {
                                                this.get_data().remove_localOperatorsFilterChanged(this.filterChanged, this);
                                            },
                                            onLoad: function () {
                                                var ds = [].makeDataSource('LocalOperator').orderBy('name').filterBy('this isnot SubsidiaryOperator');
                                                this.set_dataSource(ds);
                                                ds.load();

                                                this.get_data().add_localOperatorsFilterChanged(this.filterChanged, this);
                                            }
                                        }
                                    ]
                                },
                                {
                                    type: 'searchPane',
                                    onSearch: function (sender, args) {
                                        this.get_data().set_localOperatorsFilter(args.query);
                                    }
                                }
                            ]
                        }, //-- Local Operators
                        { //++ Subsidiary operators
                        id: 'subsidiaryOperators',
                        type: 'collapsablePanel',
                        title: 'Subsidiary operators',
                        width: '100%',
                        height: '100%',
                        showCollapseButton: false,
                        buttons: ['Add operator'],
                        showStatusBar: true,
                        cssClass: 'subsidiary_operators_block',
                        onCommand: function (sender, args) {
                            if (args.button == 'Add operator') {
                                var parent = this.get_data().get_selectedGlobalOperator();

                                var operator = new SubsidiaryOperator();
                                operator.set_operatorId(parent.get_id());
                                this.get_data().set_editedSubsidiaryOperator(operator);
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
                                            width: '?',
                                            height: '100%',
                                            layout: 'wrap',
                                            emptyDataText: 'Subsidiary operators have been not found',
                                            template: {
                                                type: 'panel',
                                                width: '300px',
                                                height: '30px',
                                                padding: '3px',
                                                orientation: 'horizontal',
                                                cssClass: 'subsidiary_operator_item',
                                                domHandlers: {
                                                    click: function () {
                                                        this.get_data().set_editedSubsidiaryOperator(this.get_dataSource());
                                                    },
                                                    mouseover: function () {
                                                        this.addCssClass('subsidiary_operator_item_hover');
                                                    },
                                                    mouseout: function () {
                                                        this.removeCssClass('subsidiary_operator_item_hover');
                                                    }
                                                },
                                                bindings: {
                                                    'countryId': function (sender, args) {
                                                        var value = args.newValue;
                                                        Repository.Get('Country', value, function (result) {
                                                            this.countryName.set_dataSource(result);
                                                        } .bind(this));
                                                    },
                                                    'fullName': 'name',
                                                    'code': 'code',
                                                    'displayedImageId': 'image',
                                                    'isArchived': function (sender, args) {                                                        
                                                        if (args.newValue){
                                                            this.isArchived.show(); 
                                                        } else {
                                                            this.isArchived.hide(); 
                                                        }
                                                    }
                                                },
                                                controls: [
                                                    {
                                                        id: 'countryName',
                                                        margin: '0 0 5 0',
                                                        width: '100',
                                                        height: '18',
                                                        valign: 'middle',
                                                        type: 'label',
                                                        cssClass: 'country_name',
                                                        bindings: {
                                                            'name': 'text'
                                                        }
                                                    },
                                                    {
                                                        id: 'image',
                                                        type: 'entityImage',
                                                        mode: 'bounded',
                                                        width: 32,
                                                        height: 24
                                                    },
                                                    {
                                                        type: 'label',
                                                        id: 'code',
                                                        width: '30',
                                                        height: '*',
                                                        cssClass: 'operator_code'
                                                    },
                                                    {
                                                        type: 'label',
                                                        id: 'name',
                                                        width: '*',
                                                        height: '*',
                                                        cssClass: 'operator_name'
                                                    },
                                                    {                           
                                                        id: 'isArchived',     
                                                        cssClass: 'archived_star',
                                                        type: 'label',                                
                                                        text: '*',       
                                                        visible: false 
                                                    }
                                                ]
                                            },
                                            customFunctions: {
                                                'filterChanged': function (sender, args) {
                                                    var parent = this.get_data().get_selectedGlobalOperator();
                                                    this.get_dataSource().set_filterString('Operator.Id = ' + parent.get_id());
                                                    this.get_dataSource().set_searchQuery(args.newValue, true);
                                                },
                                                'operatorChanged': function (sender, args) {
                                                    if (args.newValue) {
                                                        this.filterChanged(sender, { newValue: '' });
                                                    }
                                                }
                                            },
                                            onFree: function () {
                                                this.get_data().remove_subsidiaryOperatorsFilterChanged(this.filterChanged, this);
                                                this.get_data().remove_selectedGlobalOperatorChanged(this.operatorChanged, this);
                                            },
                                            onLoad: function () {
                                                var parent = this.get_data().get_selectedGlobalOperator();

                                                var ds = [].makeDataSource('SubsidiaryOperator').orderBy('name').filterBy('');
                                                this.set_dataSource(ds);

                                                this.get_data().add_subsidiaryOperatorsFilterChanged(this.filterChanged, this);
                                                this.get_data().add_selectedGlobalOperatorChanged(this.operatorChanged, this);
                                            }
                                        }
                                    ]
                                },
                                {
                                    type: 'searchPane',
                                    onSearch: function (sender, args) {
                                        this.get_data().set_subsidiaryOperatorsFilter(args.query);
                                    }
                                }
                            ]
                    } //-- Subsidiary Operators
                    ]
                },
                { //++ Global Operators
                    type: 'collapsablePanel',
                    buttons: ['Add new', 'Edit', 'Delete'],
                    width: '270',
                    showStatusBar: true,
                    showCollapseButton: false,
                    title: 'Global Operators',
                    cssClass: 'global_operators_block',
                    onCommand: function (sender, args) {
                        if (args.button == 'Add new') {
                            this.get_data().set_editedGlobalOperator(new GlobalOperator());
                            return;
                        }

                        var selectedOperator = this.get_data().get_selectedGlobalOperator();

                        if (args.button == 'Edit' && selectedOperator) {
                            this.get_data().set_editedGlobalOperator(selectedOperator);
                        }

                        if (args.button == 'Delete' && selectedOperator) {
                            this.get_window().deleteGlobalOperator(selectedOperator);
                        }
                    },
                    controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    type: 'repeater',
                                    orientation: 'vertical',
                                    emptyDataText: 'Global operators have been not found',
                                    template: {
                                        type: 'panel',
                                        height: '52px',
                                        padding: '4px',
                                        cssClass: 'global_operator_item',
                                        orientation: 'horizontal',
                                        domHandlers: {
                                            click: function () {
                                                var selected = this.get_data().get_selectedGlobalOperator();

                                                if (selected == this.get_dataSource()) {
                                                    this.get_data().set_selectedGlobalOperator(null);
                                                } else {
                                                    this.get_data().set_selectedGlobalOperator(this.get_dataSource());
                                                }
                                            },
                                            mouseover: function () {
                                                this.addCssClass('global_operator_item_hover');
                                            },
                                            mouseout: function () {
                                                this.removeCssClass('global_operator_item_hover');
                                            }
                                        },
                                        bindings: {
                                            'name': 'namePanel',
                                            'code': 'code',
                                            'imageId': 'image',
                                            'operators': function (sender, args) {
                                                var value = args.newValue;
                                                this.namePanel.operates.setOperatesInText(value);
                                            },
                                            'operators.changed': function (sender, args) {
                                                this.namePanel.operates.setOperatesInText(sender);
                                            }
                                        },
                                        controls: [
                                            {
                                                id: 'image',
                                                type: 'entityImage',
                                                width: '64',
                                                height: '*',
                                                margin: '0 0 4 0'
                                            },
                                            {
                                                id: 'code',
                                                type: 'label',
                                                width: '50',
                                                height: '18',
                                                valign: 'middle',
                                                cssClass: 'code'
                                            },
                                            {
                                                type: 'panel',
                                                id: 'namePanel',
                                                orientation: 'vertical',
                                                bindings: {
                                                    '*': 'name'
                                                },
                                                controls: [
                                                    {
                                                        id: 'name',
                                                        width: '*',
                                                        type: 'label',
                                                        cssClass: 'name',
                                                        margin: '0 0 0 5'
                                                    },
                                                    {
                                                        id: 'operates',
                                                        type: 'label',
                                                        width: '*',
                                                        cssClass: 'operates',
                                                        customFunctions: {
                                                            'setOperatesInText': function (countries) {
                                                                this.set_text('Operates in ' + countries.length.pluralize('country', 'countries'));
                                                            }
                                                        }
                                                    }
                                                ]
                                            }
                                        ],
                                        customFunctions: {
                                            'selectedChanged': function (sender, args) {
                                                if (args.newValue == this.get_dataSource()) {
                                                    this.addCssClass('global_operator_item_selected');
                                                }

                                                if (args.oldValue == this.get_dataSource()) {
                                                    this.removeCssClass('global_operator_item_selected');
                                                }
                                            }
                                        },
                                        onFree: function () {
                                            this.get_data().remove_selectedGlobalOperatorChanged(this.selectedChanged, this);
                                        },
                                        onLoad: function () {
                                            this.get_data().add_selectedGlobalOperatorChanged(this.selectedChanged, this);
                                        }
                                    },
                                    customFunctions: {
                                        'filterChanged': function (sender, args) {
                                            this.get_dataSource().set_searchQuery(args.newValue, true);
                                        }
                                    },
                                    onFree: function () {
                                        this.get_data().remove_globalOperatorsFilterChanged(this.filterChanged, this);
                                    },
                                    onLoad: function () {
                                        this.get_data().add_globalOperatorsFilterChanged(this.filterChanged, this);
                                        var ds = [].makeDataSource('GlobalOperator').orderBy('name').filterBy('');
                                        this.set_dataSource(ds);
                                        ds.load();
                                    }
                                }
                            ]
                        },
                        {
                            type: 'searchPane',
                            width: '100%',
                            height: 27,
                            onSearch: function (sender, args) {
                                this.get_data().set_globalOperatorsFilter(args.query);
                            }
                        }
                    ]
                }
            ]
        }
    ]
})