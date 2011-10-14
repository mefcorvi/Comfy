({
    title: 'Detach from global',
    layout: 'stack',
    orientation: 'vertical',
    cssClass: 'detach_from_global_page',

    data: [
        'project',
        'projectId',
        { name: 'selectedOperators', value: [].makeObservable() },
        { name: 'selectedCountries', value: [].makeObservable() },
        { name : 'handset' },
        { name: 'team', value: [].makeObservable() }
    ],

    customFunctions: {
        'processDetach': function(callback) {            
            var teamContract = this.get_data().get_team().select(function(ur) { return { m_Item1: ur.get_roleId(), m_Item2: ur.get_userId() } });

            var contract = { projectId: this.get_data().get_project().get_id(), team: teamContract };

            Services.ProjectService.Branch(contract, function (res) {
                //res.get_userRoles().synchronize(this.get_data().get_team());
                Repository._removeFromCache(res.get_type().get_name(), res.get_id());
                callback(res);
            }.bind(this));
        }
    },

    onLoad: function() {
        var data = this.get_data();

        var projectId = this.get_param('projectId') * 1;      

        Services.UserRoleService.CreateTeamForDetaching(projectId, function(result) {
            data.get_team().synchronize(result.makeObservable());
        }.bind(this));

        Repository.Load('LocalProject', projectId, function(result) {            
            data.set_project(result);

            Repository.Get('LocalOperator', result.get_operatorId(), function (operator) {
                data.get_selectedOperators().add(operator);

                Repository.Get('Country', operator.get_countryId(), function (country) {
                    data.get_selectedCountries().add(country);
                });
            });

            Repository.Get('Handset', result.get_handsetId(), function (handset) {
                data.set_handset(handset);
            });

        });
    },

    controls: [
        { 
            type: 'panel',
            orientation: 'vertical',
            margin: '5 5 5 5',
            controls: [
                {
                    type: 'label',
                    width: '*',
                    text: 'Do you really want to process branching? Please be adviced that team will be inherited as follows.',
                    margin: '0 0 0 5'
                },
                {
                    type: 'multiView',   
                    views: [
                        {
                            id: 'teamViewing',
                            type: 'collapsablePanel',
                            showCollapseButton: false,
                            showStatusBar: true,
                            showHeader: false,
                            buttons: ['Edit'],
                            bindings: {
                                '*': 'teamTree'
                            },
                            onCommand: function(sender, args) {
                                switch (args.button) {
                                    case 'Edit': {
                                        this.parent.set_activeView('teamEditing');
                                        break;
                                    }                                    
                                }
                            },
                            controls: [
                                {
                                    id: 'teamTree',
                                    type: 'teamTree',
                                    padding: '5',
                                    height: '?',
                                    showCheckBoxes: false,
                                    showLinks: true,
                                    onLoad: function() {
                                        this.set_dataSource(this.get_data().get_team());
                                    }
                                }                                
                            ]
                        },
                        {
                            id: 'teamEditing',
                            type: 'teamSelector',
                            cssClass: 'team_selector',
                            padding: '3',
                            border: '1',
                            onLoad: function() {
                                this.set_selectedCountries(this.get_data().get_selectedCountries());
                                this.set_selectedOperators(this.get_data().get_selectedOperators());
                                this._handsetToSelectedLink = Auto.Property.CreateLink(this.get_data(), this, "handset", "selectedHandset");
                                this.set_dataSource(this.get_data().get_team());                                
                            },
                            onRolesRequired: function(callback) {                               
                                Repository.Filter('ProjectTeamTemplate', 'Scope = ' + ProjectScope.local, function (result) {
                                    result.first().get_roles(function(templateRoles) {
                                        Repository.Get('Role', templateRoles.select('roleId'), function(roles) {
                                            callback(roles);
                                        });
                                    });
                                });                               
                            }
                        }                        
                    ]
                }
            ]
        }        
    ]
})