({
    title: "Constraints",
    cssClass: 'constraints_page',
    orientation: 'horizontal',
    data: [
        'selectedItem',
        'countries',
        'handsets',
        'operators',
        'selectedRoles'
    ],
    customFunctions: {
        'loadData': function() {
            var data = this.get_data();
            data.set_countries([].makeDataSource('Country').filterBy('').orderBy('name').load());
            data.set_handsets([].makeDataSource('Handset').filterBy('').orderBy('code, name').load(function(result) {
                if (!data.get_selectedItem()) {
                    data.set_selectedItem(result.first());
                }
            }));
            data.set_operators([].makeDataSource('Operator').filterBy('this isnot SubsidiaryOperator').orderBy('name').load());
        }
    },
    onLoad: function() {
        this.loadData();
    },
    controls: [
        {
            id: 'tabPanel',
            type: 'tabPanel',
            width: '30%',
            minWidth: 200,
            maxWidth: 300,
            margin: '0 0 3 0',
            tabs: [
                // #region Handsets
                {
                    title: 'Handsets',
                    controls: [
                        {
                            type: 'listView',
                            template: {
                                type: 'panel',
                                orientation: 'horizontal',
                                height: '25',
                                padding: '5',
                                bindings: {
                                    'imageId': 'imageId',
                                    'fullName': 'label'
                                },
                                controls: [
                                    {
                                        id: 'imageId',
                                        type: 'entityImage',
                                        width: '25',
                                        height: '15',
                                        margin: '0 0 5 0'
                                    },
                                    {
                                        id: 'label',
                                        type: 'label',
                                        width: '*',
                                        height: '*'
                                    }
                                ]
                            },
                            bindings: {
                                'data:selectedItem': 'selectedValue',
                                'data:handsets': 'items'
                            }
                        },
                        {
                            type: 'searchPane',
                            onSearch: function(sender, args) {
                                var handsets = this.get_data().get_handsets();
                                handsets.set_searchQuery(args.query);
                                handsets.load();
                            }
                        }
                    ]
                },
                // #endregion
                // #region Countries
                {
                    title: 'Countries',
                    controls: [
                        {
                            type: 'listView',
                            template: {
                                type: 'panel',
                                orientation: 'horizontal',
                                height: '25',
                                padding: '5',
                                bindings: {
                                    'imageId': 'imageId',
                                    'name': 'label'
                                },
                                controls: [
                                    {
                                        id: 'imageId',
                                        type: 'entityImage',
                                        width: '25',
                                        height: '15',
                                        mode: 'crop',
                                        margin: '0 0 5 0'
                                    },
                                    {
                                        id: 'label',
                                        type: 'label',
                                        width: '*',
                                        height: '*'
                                    }
                                ]
                            },
                            bindings: {
                                'data:selectedItem': 'selectedValue',
                                'data:countries': 'items'
                            }
                        },
                        {
                            type: 'searchPane',
                            onSearch: function(sender, args) {
                                var ds = this.get_data().get_countries();
                                ds.set_searchQuery(args.query);
                                ds.load();
                            }
                        }
                    ]
                },
                // #endregion
                // #region Operators
                {
                    title: 'Operators',
                    controls: [
                        {
                            type: 'listView',
                            template: {
                                type: 'panel',
                                orientation: 'horizontal',
                                height: '25',
                                padding: '5',
                                bindings: {
                                    'displayedImageId': 'imageId',
                                    'fullName': 'label'
                                },
                                controls: [
                                    {
                                        id: 'imageId',
                                        type: 'entityImage',
                                        width: '25',
                                        height: '15',
                                        margin: '0 0 5 0'
                                    },
                                    {
                                        id: 'label',
                                        type: 'label',
                                        width: '*',
                                        height: '*'
                                    }
                                ]
                            },
                            bindings: {
                                'data:selectedItem': 'selectedValue',
                                'data:operators': 'items'
                            }
                        },
                        {
                            type: 'searchPane',
                            onSearch: function(sender, args) {
                                var ds = this.get_data().get_operators();
                                ds.set_searchQuery(args.query);
                                ds.load();
                            }
                        }
                    ]
                }
                // #endregion
            ]
        },
        {
            id: 'usersSelector',
            type: 'collapsablePanel',
            title: 'Team presets',
            showCollapseButton: false,
            bindings: {
                'data:selectedItem': 'constraintsEdit'
            },
            controls: [
                {
                    type: 'panel',
                    cssClass: 'preset_topPanel',
                    orientation: 'horizontal',
                    height: '?',
                    padding: '10 5',
                    border: '0 0 0 1',
                    margin: '0 0 0 5',
                    bindings: {
                        'data:selectedItem': function(sender, args) {
                            var selectedItem = args.newValue;
                            if (selectedItem instanceof Handset) {
                                this.label.set_text("Selected handset: ");
                                this.selectedItem.set_text(selectedItem.get_fullName());
                                this.imageId.set_dataSource(selectedItem.get_imageId());
                            } else if (selectedItem instanceof LocalOperator || selectedItem instanceof GlobalOperator) {
                                this.label.set_text("Selected operator: ");
                                this.selectedItem.set_text(selectedItem.get_fullName());
                                this.imageId.set_dataSource(selectedItem.get_displayedImageId());
                            } else if (selectedItem instanceof Country) {
                                this.label.set_text("Selected country: ");
                                this.selectedItem.set_text(selectedItem.get_name());
                                this.imageId.set_dataSource(selectedItem.get_imageId());
                            }
                        }
                    },
                    controls: [
                        { type: 'label', text: '', id: 'label', width: '110', valign: 'middle', cssClass: 'form_label' },
                        { id: 'imageId', type: 'entityImage', width: '25', height: '15', margin: '0 0 5 0', valign: 'middle' },
                        { type: 'label', text: '', id: 'selectedItem', width: '*', valign: 'middle' }
                    ]
                },
                {
                    id: 'constraintsEdit',
                    type: 'constraintsEdit',
                    onRolesChanged: function() {
                        this.save();
                    }
                }
            ]
        }
    ]
})
