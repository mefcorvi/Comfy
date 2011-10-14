({
    title: 'Edit constraints',
    layout: 'stack',
    orientation: 'vertical',
    cssClass: 'edit_constraints_page',
    data:
    [
        'userRoleId',
        'userRole',
        'role',
        { name: 'countries', value: [].makeDataSource('Country').filterBy('').orderBy('name') },
        { name: 'operators', value: [].makeDataSource('Operator').filterBy('this isnot SubsidiaryOperator').orderBy('name') },
        { name: 'handsets', value: [].makeDataSource('Handset').filterBy('').orderBy('code,name') },
        { name: 'selectedCountries', value: [].makeObservable() },
        { name: 'selectedOperators', value: [].makeObservable() },
        { name: 'selectedHandsets', value: [].makeSorted('code') }
    ],

    customFunctions: {
        'save': function () {
            this.get_data().get_userRole().get_countryConstraints().synchronize(this.get_data().get_selectedCountries());
            this.get_data().get_userRole().get_operatorConstraints().synchronize(this.get_data().get_selectedOperators());
            this.get_data().get_userRole().get_handsetConstraints().synchronize(this.get_data().get_selectedHandsets());
            Repository.Save(this.get_data().get_userRole(),
                function (result) {
                    if (result) {
                        this.get_window().close();
                    }
                    else {
                        Application.showError('error');
                    }
                } .bind(this)
            );
        }
    },

    onLoad: function () {
        var data = this.get_data();
        var userRoleId = this.get_param('userRoleId') * 1;

        var thisPage = this;
        this.addCssClass('constraints_edit_page');

        data.set_userRoleId(userRoleId);

        Repository.Get('ConstraintedUserRole', userRoleId, function (result) {
            data.set_userRole(result);

            Repository.Reload(result);

            result.get_countryConstraints(function (res) {
                data.get_selectedCountries().synchronize(res)
            });

            result.get_operatorConstraints(function (res) {
                data.get_selectedOperators().synchronize(res)
            });

            result.get_handsetConstraints(function (res) {
                data.get_selectedHandsets().synchronize(res)
            });

            Repository.Get('Role', result.get_roleId(), function (role) {
                data.set_role(role);                

                if (!role.get_hasCountryConstraint() && !role.get_hasOperatorConstraint() && !role.get_hasHandsetConstraint()) {
                    Application.showError('Error', 'Cant have constraints');
                    this.get_window().close();
                }

                if (role.get_hasCountryConstraint()) {
                    thisPage.get_data().get_countries().load();
                    thisPage.container.countriesPanel.show();
                } else {
                    thisPage.container.countriesPanel.hide();
                }

                if (role.get_hasOperatorConstraint()) {
                    thisPage.get_data().get_operators().load();
                    thisPage.container.operatorsPanel.show();
                } else {
                    thisPage.container.operatorsPanel.hide();
                }
                
                if (role.get_hasHandsetConstraint()) {
                    thisPage.get_data().get_handsets().load(function() {
                        thisPage.get_data().get_selectedHandsets().exclusiveAttach(thisPage.get_data().get_handsets());
                    });
                    thisPage.container.handsetsPanel.show();
                } else {
                    thisPage.container.handsetsPanel.hide();
                }
            });
        });
    },
    controls:
    [
        {
            id: 'container',
            type: 'panel',
            orientation: 'vertical',
            padding: '3 2 3 0',
            controls:
            [
                {
                    id: 'operatorsPanel',
                    type: 'collapsablePanel',
                    cssClass: 'operators_panel',
                    title: 'Select a subset of operators',
                    showStatusBar: false,
                    margin: '0 0 0 3',
                    controls: [
                        {
                            type: 'allOperatorsSelector',
                            onLoad: function () {
                                this.set_dataSource(this.parent.get_data().get_operators());
                                this.set_selectedOperators(this.parent.get_data().get_selectedOperators());
                            }
                        }
                    ]
                },
                {
                    id: 'countriesPanel',
                    type: 'collapsablePanel',
                    title: 'Select a subset of countries',
                    showStatusBar: false,
                    controls: [
                        {
                            type: 'countriesSelector',
                            countryItemWidth: 200,
                            onLoad: function () {
                                this.set_dataSource(this.parent.get_data().get_countries());
                                this.set_selectedCountries(this.parent.get_data().get_selectedCountries());
                            }                                
                        }
                    ]
                },
                {
                    id: 'handsetsPanel',
                    type: 'collapsablePanel',
                    title: 'Select a subset of handsets',
                    showStatusBar: false,
                    orientation: 'horizontal',
                    controls: [
                        {
                            type: 'panel',
                            margin: '0 0 5 0',
                            controls: [
                                { type: 'label', text: 'Available handsets:', width: '*', height: '25', padding: '5', border: '0 0 0 1', margin: '0 0 0 3', cssClass: 'label_availableHandsets' },
                                {
                                    type: 'listView',
                                    bindings: {
                                        'data:handsets': 'items'
                                    },
                                    onItemClick: function(sender, args) {
                                        this.get_data().get_handsets().remove(args.item);
                                    },
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
                                    }
                                }
                            ]
                        },
                        {
                            type: 'panel',
                            controls: [
                                { type: 'label', text: 'Selected handsets:', width: '*', height: '25', padding: '5', border: '0 0 0 1', margin: '0 0 0 3', cssClass: 'label_selectedHandsets' },
                                {
                                    type: 'listView',
                                    bindings: {
                                        'data:selectedHandsets': 'items'
                                    },
                                    onItemClick: function(sender, args) {
                                        this.get_data().get_handsets().add(args.item);
                                    },
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
                                    }
                                }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            type: 'button',
            width: '100',
            text: 'OK',
            onClick: function () {
                this.get_window().save();
            }
        }
    ]
})