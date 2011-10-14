({
    title: "Project",
    layout: "stack",
    orientation: 'vertical',
    valign: 'stretch',
    data: [ // something like a model...
        "handset",
        "scope",
        "projectType",
        { name: 'pageView', value: 'team' },
        { name: 'team', value: [].makeObservable() },
        { name: 'selectedCountries', value: [].makeSorted('name') },
        { name: 'selectedOperators', value: [].makeObservable() },
        'project',
        'validationError',
        'description',
        'templateId',
        { name: 'taskAlignType', value: 'from' },
        'alignDate',
        'showedProjectId' // identifier for popup with project details
    ],
    customFunctions: {
        // #region Controller 
        'getProjectData': function () {
            var data = this.get_data();

            return {
                projectId: data.get_project().get_id(),
                scope: data.get_scope(),                
                countries: data.get_selectedCountries().clone(),
                operators: data.get_selectedOperators().clone(),
                userRoles: data.get_team().clone(),
                projectType: data.get_projectType(),                                               
                description: data.get_description()             
            };
        },
        'saveProject': function (withValidate) {
            var data = this.get_data();
            withValidate = isNullOrUndefined(withValidate) ? true : withValidate;            

            Services.ProjectService.EditProject(
                this.getProjectData(),
                !withValidate,
                function (result) {
                    if (result.get_projectType() == ProjectType["global"] || result.get_projectType() == ProjectType["open"]) {
                        for ( var i=0; i<result.get_relatedProjects().length; i++ ) {
                            var childProject = result.get_relatedProjects()[i];
                            Repository._removeFromCache(childProject.get_type().get_name(), childProject.get_id());
                        }
                    }
                    Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
                }, 
                withValidate ? function (validationError) {
                    data.set_validationError(validationError);
                } : undefined);
        },
        'createBranch': function (globalProject) {
            Services.ProjectService.CreateAsBranch(this.getProjectData(), globalProject, function (result) {
                Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
            });
        },
        'promote': function (localProject) {
            Services.ProjectService.Promote(this.getProjectData(), localProject, function (result) {
                Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
            });
        },
        '_countriesChanged': function (sender, args) {
            var scope = this.get_data().get_scope();

            if (scope == ProjectScope.open) {
                var operators = this.get_data().get_selectedOperators();
                var countriesIds = sender.select("id");

                for (var i = operators.length - 1; i >= 0; i--) {
                    if (!countriesIds.contains(operators[i].get_countryId())) {
                        operators.remove(operators[i]);
                    }
                }
            }
        },
        'loadProject': function(projectId) {         
            var data = this.get_data();
            this.pageHeader.set_title('Edit Project');

            Repository.Load('HandsetProject', projectId, function (result) {
                if (!result) {
                    Application.showError('Error', 'Project have not been found. It is possible that it has already been removed...', function() {
                        Application.loadPage('planning/managePlans');
                    });
                    return;
                }

                data.set_project(result);      
                if (!Application.get_currentUser().canEdit(result) || !result.isEditable()) {
                    Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
                }

                Repository.Get('Handset', result.get_handsetId(), function (handset) {
                    data.set_handset(handset);
                });

                data.get_selectedOperators().clear();
                data.get_selectedCountries().clear();

                var scope = ProjectScope.getByProject(result);
                data.set_scope(scope);

                data.set_projectType(result.get_modelLine());

                if (result instanceof OpenProject) {  
                                   
                    data.set_scope(ProjectScope.open);

                    result.get_countries(function (r) {
                        data.get_selectedCountries().synchronize(r);
                    });
                    result.get_operators(function (r) {
                        data.get_selectedOperators().synchronize(r);
                    });
                }

                if (result instanceof GlobalProject) {
                    data.set_scope(ProjectScope.global);

                    var dataOperators = result.get_operators(function (r) {
                        if (r.length > 0) {
                            Repository.Get('GlobalOperator', r[0].get_operatorId(), function (operator) {
                                data.get_selectedOperators().add(operator);

                                var countries = [];

                                for (var i = 0; i < r.length; i++) {
                                    countries.add(r[i].get_countryId());
                                }

                                Repository.Get('Country', countries, function (countries) {
                                    data.get_selectedCountries().synchronize(countries);
                                });
                            });
                        }
                    });
                }

                if (result instanceof LocalProject) {
                    data.set_scope(ProjectScope.local);

                    Repository.Get('LocalOperator', result.get_operatorId(), function (operator) {
                        data.get_selectedOperators().add(operator);

                        Repository.Get('Country', operator.get_countryId(), function (country) {
                            data.get_selectedCountries().add(country);
                        });
                    });
                }

                result.get_userRoles(function (r) {
                    data.get_team().synchronize(r);
                });

                if (this.isAlignedProject()) {
                    data.set_pageView('empty');
                }
            }.bind(this));
        },
        'isAlignedProject': function() {
            return this.get_data().get_scope() == ProjectScope.local && this.get_data().get_project().get_isAligned();
        }
        // #endregion
    },
    cssClass: 'create_project_page edit_project_page',

    onLoad: function () {
        var projectId = this.get_param('projectId') * 1;

        if (projectId) {
            this.loadProject(projectId);
        }

        this.get_data().get_selectedCountries().add_changed(this._countriesChanged, this);
    },

    onFree: function () {
        this.get_data().get_selectedCountries().remove_changed(this._countriesChanged, this);
    },

    controls:
    [
        // #region Project Details Popup
        {
            id: 'projectDetailsPopup',
            type: 'popup',
            title: 'Conflict details',
            uri: '',
            width: '90%',
            height: '90%',
            buttons: ['Close'],
            onCommand: function () {
                this.get_data().set_showedProjectId(null);
            },
            customFunctions: {
                '_projectIdChanged': function (sender, args) {
                    this.close();                    

                    if (args.newValue) {
                        this.set_uri('planning/projectDetails|projectId=' + args.newValue);
                        this.open();
                    }
                }
            },
            onLoad: function () {
                this.get_data().add_showedProjectIdChanged(this._projectIdChanged, this);
            },
            onFree: function () {
                this.get_data().remove_showedProjectIdChanged(this._projectIdChanged, this);
            }
        },
        // #endregion
        // #region Validation error Popup
        {
            id: 'validationErrorPopup',
            type: 'popup',
            title: 'Validation error',
            width: 400,
            height: '?',
            controls: [
                {
                    id: 'lblError',
                    type: 'label',
                    width: '*',
                    height: '?',
                    padding: '5',
                    cssClass: 'lbl_marketConflict',
                    text: ''
                },
                {
                    id: 'scrollBox',
                    type: 'scrollablePanel',
                    height: '?',
                    width: '*',
                    maxHeight: 500,
                    controls: [
                        {
                            id: 'multiView',
                            type: 'multiView',
                            width: '*',
                            height: '?',
                            views: [
                                { id: 'plainError', type: 'panel' }, // dummy
                                {
                                    id: 'marketsConflict',
                                    type: 'marketConflictList',
                                    height: '?'
                                }
                            ]
                        }
                    ]
                }
            ],
            onCommand: function (sender, args) {
                var multiView = this.container.scrollBox.multiView;

                if (args.button == 'Close') {
                    this.get_data().set_validationError(null);
                }
                if (args.button == 'Continue saving') {
                    this.get_data().set_validationError(null);
                    this.get_window().saveProject(false);
                }

                if (args.button == 'Create branch') {
                    this.get_window().createBranch(multiView.marketsConflict.get_dataSource().single());
                }

                if (args.button == 'Promote') {
                    this.get_window().promote(multiView.marketsConflict.get_dataSource().single());
                }
            },
            onLoad: function () {
                this.get_data().add_validationErrorChanged(function (sender, args) {
                    var errors = args.newValue;
                    var multiView = this.container.scrollBox.multiView;

                    if (errors) {
                        if (errors instanceof Array && errors.length > 0) {
                            var marketsConflict = errors.ofType(MarketsConflictFaultContract).first();
                            var duplicateError = errors.ofType(DuplicateProjectFaultContract).first();
                            var globalProjectJoinSuggestion = errors.ofType(GlobalProjectJoinSuggestionContract).first();

                            if (marketsConflict != null) {
                                this.container.lblError.set_text("Project targets for selected market already exists.");
                                multiView.set_activeView('marketsConflict');

                                if (!this.get_data().get_project() || this.get_data().get_project().isNew()) {
                                    if (this.get_data().get_scope() == ProjectScope.open) {
                                        this.set_buttons(["Close"]);
                                    }

                                    if (this.get_data().get_scope() == ProjectScope.global) {
                                        this.set_buttons(["Promote", "Close"]);
                                    }

                                    if (this.get_data().get_scope() == ProjectScope.local) {
                                        this.set_buttons(["Create branch", "Close"]);
                                    }
                                } else {
                                    this.set_buttons(["Close"]);
                                }

                                multiView.marketsConflict.set_dataSource(marketsConflict.get_conflictProjects());
                            } else if (duplicateError != null) {
                                multiView.set_activeView('marketsConflict');

                                if (this.get_data().get_scope() == ProjectScope.open) {
                                    this.container.lblError.set_text("Similar project target already exists. Would you like to continue?");
                                    this.set_buttons(["Continue saving", "Close"]);
                                }

                                if (this.get_data().get_scope() == ProjectScope.global) {
                                    this.container.lblError.set_text("Similar project target already exists.");
                                    this.set_buttons(["Close"]);
                                }

                                if (this.get_data().get_scope() == ProjectScope.local) {
                                    this.container.lblError.set_text("Similar project target already exists.");
                                    this.set_buttons(["Close"]);
                                }

                                multiView.marketsConflict.set_dataSource([duplicateError.get_existingProject()].makeObservable());
                            } else if (globalProjectJoinSuggestion != null) {
                                this.container.lblError.set_text("Data is corrupted. Please contact with administrator as soon as possible.");
                                this.set_buttons(["Close"]);
                            }
                        } else {
                            this.container.lblError.set_text(errors);
                            this.set_buttons(["Close"]);
                        }

                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            }
        },
        // #endregion
        // #region Page Title
        {
            id: 'pageHeader',
            type: "pageHeader",
            title: "Edit Project"
        },
        // #endregion
        {
            type: 'panel',
            layout: 'stack',
            orientation: 'horizontal',
            halign: 'stretch',
            controls: [
                {
                    type: 'panel',
                    margin: '0 0 3 0',
                    width: '330',
                    cssClass: 'project_page_left_column',
                    controls: [
                        {
                            type: 'collapsablePanel',
                            title: 'Basic Properties',
                            orientation: 'horizontal',
                            cssClass: 'project_scope_panel',
                            cookieKey: 'project_scope',
                            height: '?',
                            padding: '5',
                            margin: '0 0 0 3',
                            bindings: {
                                'data:project': 'basicProperties'
                            },
                            controls: [
                                {
                                    id: 'basicProperties',
                                    type: 'form',
                                    height: '?',
                                    bindings: {
                                        'scope': 'scope',
                                        'modelLine': function(sender, args) {
                                            var modelLine = args.newValue;

                                            if (modelLine === HandsetModelLine.runningChange){                                            
                                                this.projectType.specialProjectType.set_text(modelLine);
                                                this.projectType.generalProjectType.hide();
                                                this.projectType.specialProjectType.show();
                                            } else {
                                                if (!modelLine) {
                                                    this.projectType.generalProjectType.set_notSelectedItem("-");
                                                }
                                                this.projectType.generalProjectType.set_selectedValue(modelLine);
                                                this.projectType.specialProjectType.hide();
                                                this.projectType.generalProjectType.show();                                            
                                            }
                                        },
                                        'description': 'description'
                                    },
                                    controls: [
                                        {
                                            id: 'scope',
                                            type: 'label',
                                            width: '*',
                                            height: '18',
                                            'form.label': 'Scope:'
                                        },
                                        {
                                            id: 'projectType',
                                            type: 'panel',
                                            'form.label': 'Type:',
                                            height: '20',
                                            controls: [
                                                {
                                                    id: 'generalProjectType',                                    
                                                    type: 'dropDownList',
                                                    width: '*',
                                                    height: '20',
                                                    items: [
                                                        HandsetModelLine.base, HandsetModelLine.countryAdaptation
                                                    ],
                                                    onChanged: function (sender, args) {
                                                        this.get_data().set_projectType(args.newValue);
                                                    }
                                                },
                                                {
                                                    type: 'label',                                    
                                                    id: 'specialProjectType',
                                                    width: '100%-80px',
                                                    text: ' ',
                                                    cssClass: 'form_label',
                                                    height: 18
                                                }
                                            ]
                                        },
                                        {
                                            id: 'description',
                                            type: 'textBox',
                                            mode: 'multiline',
                                            'form.label': 'Description:',
                                            height: '43',
                                            onChanged: function (sender, args) {
                                                this.get_data().set_description(args.newValue);
                                            }
                                        }
                                    ]
                                }
                            ]
                        },
                        {
                            type: 'collapsablePanel',
                            title: 'Handset Model',
                            height: '?',
                            orientation: 'horizontal',
                            cookieKey: 'handsetPanel',
                            padding: '5',
                            margin: '0 0 0 3',
                            cssClass: 'project_target_panel',
                            bindings: {
                                'data:handset': function (sender, args) {
                                    var inner = this.innerPanel;
                                    var handsetInfo = inner.handsetInfo;
                                    var handset = args.newValue;

                                    if (handset) {
                                        var details = handsetInfo.details;
                                        var title = handset.get_code() + '/' + handset.get_name();

                                        inner.image.set_imageId(handset.get_imageId());
                                        handsetInfo.name.set_text(title);

                                        details["releaseDate"].set_text(handset.get_releaseDate());
                                        details["class"].set_text(handset.get_class());
                                    } else {
                                        inner.image.set_imageId(-1);
                                    }
                                }
                            },
                            controls: [
                                {
                                    id: 'image',
                                    type: 'entityImage',
                                    width: '40',
                                    height: '65',
                                    mode: "bounded",
                                    margin: '0 0 5 0'
                                },
                                {
                                    id: 'handsetInfo',
                                    type: 'panel',
                                    height: '65',
                                    controls: [
                                        {
                                            id: 'name',
                                            type: 'label',
                                            text: '',
                                            cssClass: 'handset_name',
                                            width: '*',
                                            height: '23',
                                            margin: '0 0 0 2'
                                        },
                                        {
                                            id: 'details',
                                            type: 'form',
                                            cssClass: 'handset_details',
                                            spacing: 0,
                                            controls: [
                                                {
                                                    id: 'releaseDate',
                                                    type: 'label',
                                                    'form.label': 'Release Date:',
                                                    format: '{0:d}',
                                                    height: 16,
                                                    text: ''
                                                },
                                                {
                                                    id: 'class',
                                                    type: 'label',
                                                    'form.label': 'Class:',
                                                    height: 16,
                                                    text: '',
                                                    emptyText: '-'
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        },                        
                        {
                            type: 'collapsablePanel',
                            title: 'Team',
                            orientation: 'horizontal',
                            cssClass: 'project_team_panel',
                            cookieKey: 'teamPanel',
                            showCollapseButton: true,
                            controls: [
                                {
                                    type: 'scrollablePanel',
                                    id: 'teamScroll',
                                    controls: [
                                        {
                                            id: 'teamTree',
                                            type: 'teamTree',
                                            padding: '5',
                                            height: '?',
                                            showCheckBoxes: false,
                                            onLoad: function () {
                                                this.set_dataSource(this.parent.get_data().get_team());
                                            }
                                        }
                                    ]
                                }
                            ],
                            blockName: 'team',
                            margin: '0 0 0 3',
                            headerAsCollapseButton: false,
                            bindings: {
                                'data:pageView': function(sender, args) {
                                    if (args.newValue == this.options.blockName) {
                                        this.addCssClass('block_selected');
                                    } else if (args.oldValue == this.options.blockName) {
                                        this.removeCssClass('block_selected');
                                    }
                                }
                            },
                            domHandlers: {
                                'click': function (sender, args) {
                                    if (this.get_window().isAlignedProject()) {
                                        return;
                                    }

                                    this.parent.get_data().set_pageView(this.options.blockName);
                                },
                                'mouseover': function() {
                                    this.addCssClass('block_hovered');
                                },
                                'mouseout': function() {
                                    this.removeCssClass('block_hovered');
                                }
                            }
                        },
                        {
                            type: 'multiView',
                            margin: '0 0 0 3',
                            views: [
                                {
                                    id: 'openTarget',
                                    type: 'panel',
                                    orientation: 'vertical',
                                    cssClass: 'open_target',
                                    controls: [
                                        {
                                            type: 'collapsablePanel',
                                            title: 'Countries',
                                            cookieKey: 'countriesPanel',
                                            cssClass: 'countries_list',
                                            controls: [
                                                {
                                                    type: 'scrollablePanel',
                                                    controls: [
                                                        {
                                                            id: 'countries',
                                                            type: 'repeater',
                                                            layout: 'wrap',
                                                            orientation: 'horizontal',
                                                            width: '100%',
                                                            height: '?',
                                                            padding: '5',
                                                            emptyDataText: 'No countries selected',
                                                            template: {
                                                                type: 'panel',
                                                                width: '50%-1',
                                                                height: '20',
                                                                orientation: 'horizontal',
                                                                cssClass: 'country_row',
                                                                controls: [
                                                                    {
                                                                        id: 'image',
                                                                        type: 'entityImage',
                                                                        mode: 'exactly',
                                                                        width: 20,
                                                                        height: 15,
                                                                        margin: '0 0 5 0'
                                                                    },
                                                                    {
                                                                        id: 'countryName',
                                                                        type: 'label',
                                                                        cssClass: 'countryName',
                                                                        width: '*',
                                                                        height: '*',
                                                                        text: ''
                                                                    }
                                                                ],
                                                                bindings: {
                                                                    '*': function (sender, args) {
                                                                        var dataItem = args.newValue;
                                                                        this.countryName.set_text(dataItem.get_name());
                                                                        this.image.set_imageId(dataItem.get_imageId());
                                                                    }
                                                                }
                                                            },
                                                            onLoad: function () {
                                                                this.set_dataSource(this.get_data().get_selectedCountries());
                                                            }
                                                        }
                                                    ]
                                                }
                                            ],
                                            blockName: 'countries',
                                            headerAsCollapseButton: false,
                                            margin: '0 0 0 3',
                                            bindings: {
                                                'data:pageView': function(sender, args) {
                                                    if (args.newValue == this.options.blockName) {
                                                        this.addCssClass('block_selected');
                                                    } else if (args.oldValue == this.options.blockName) {
                                                        this.removeCssClass('block_selected');
                                                    }
                                                }
                                            },
                                            domHandlers: {
                                                'click': function (sender, args) {
                                                    this.parent.get_data().set_pageView(this.options.blockName);
                                                },
                                                'mouseover': function() {
                                                    this.addCssClass('block_hovered');
                                                },
                                                'mouseout': function() {
                                                    this.removeCssClass('block_hovered');
                                                }
                                            }
                                        },
                                        {
                                            type: 'collapsablePanel',
                                            title: 'Operators',
                                            cookieKey: 'operatorsPanel',
                                            controls: [
                                                {
                                                    type: 'scrollablePanel',
                                                    controls: [
                                                        {
                                                            type: 'operatorsTree',
                                                            width: '*',
                                                            height: '?',
                                                            padding: '5',
                                                            onLoad: function () {
                                                                this.set_dataSource(this.get_data().get_selectedOperators());
                                                            }
                                                        }
                                                    ]
                                                }
                                            ],
                                            blockName: 'operators',
                                            headerAsCollapseButton: false,
                                            bindings: {
                                                'data:pageView': function(sender, args) {
                                                    if (args.newValue == this.options.blockName) {
                                                        this.addCssClass('block_selected');
                                                    } else if (args.oldValue == this.options.blockName) {
                                                        this.removeCssClass('block_selected');
                                                    }
                                                }
                                            },
                                            domHandlers: {
                                                'click': function (sender, args) {
                                                    this.parent.get_data().set_pageView(this.options.blockName);
                                                },
                                                'mouseover': function() {
                                                    this.addCssClass('block_hovered');
                                                },
                                                'mouseout': function() {
                                                    this.removeCssClass('block_hovered');
                                                }
                                            }
                                        }
                                    ]
                                },
                                {
                                    id: 'localTarget',
                                    type: 'collapsablePanel',
                                    title: 'Local Market',
                                    cookieKey: 'localMarketPanel',
                                    cssClass: 'local_target',
                                    controls: [
                                        {
                                            type: 'panel',
                                            layout: 'stack',
                                            padding: '5',
                                            orientation: 'horizontal',
                                            controls: [
                                                {
                                                    type: 'label',
                                                    text: 'Country:',
                                                    width: '80'
                                                },
                                                {
                                                    type: 'label',
                                                    width: '100%-80',
                                                    text: 'not selected',
                                                    onLoad: function () {
                                                        if (!this.__changed) {
                                                            this.__changed = function (sender, args) {
                                                                if (sender.length == 1) {
                                                                    this.set_text(sender[0].get_name());
                                                                } else {
                                                                    this.set_text('not selected');
                                                                }
                                                            }
                                                        }

                                                        this.get_data().get_selectedCountries().add_changed(this.__changed, this);
                                                    },
                                                    onFree: function () {
                                                        if (this.__changed) {
                                                            this.get_data().get_selectedCountries().remove_changed(this.__changed);
                                                        }
                                                    }
                                                },
                                                {
                                                    type: 'label',
                                                    text: 'Operator:',
                                                    width: '80'
                                                },
                                                {
                                                    type: 'label',
                                                    text: 'not selected',
                                                    width: '100%-80',
                                                    onLoad: function () {
                                                        if (!this.__changed) {
                                                            this.__changed = function (sender, args) {
                                                                if (sender.length == 1) {
                                                                    var operator = sender[0];

                                                                    if (operator instanceof SubsidiaryOperator) {
                                                                        Repository.Get('GlobalOperator', operator.get_operatorId(), function (result) {
                                                                            this.set_text(result.get_name());
                                                                        } .bind(this));
                                                                    } else {
                                                                        this.set_text(operator.get_name());
                                                                    }
                                                                } else {
                                                                    this.set_text('not selected');
                                                                }
                                                            }
                                                        }

                                                        this.get_data().get_selectedOperators().add_changed(this.__changed, this);
                                                    },
                                                    onFree: function () {
                                                        if (this.__changed) {
                                                            this.get_data().get_selectedOperators().remove_changed(this.__changed);
                                                        }
                                                    }
                                                }
                                            ]
                                        }
                                    ],
                                    blockName: 'localMarket',
                                    headerAsCollapseButton: false,
                                    bindings: {
                                        'data:pageView': function(sender, args) {
                                            if (args.newValue == this.options.blockName) {
                                                this.addCssClass('block_selected');
                                            } else if (args.oldValue == this.options.blockName) {
                                                this.removeCssClass('block_selected');
                                            }
                                        }
                                    }
                                },
                                {
                                    id: 'globalTarget',
                                    type: 'collapsablePanel',
                                    title: 'Global Market',
                                    cookieKey: 'globalMarketPanel',
                                    cssClass: 'global_target',
                                    controls: [
                                        {
                                            type: 'panel',
                                            cssClass: 'operator_name',
                                            height: 22,
                                            padding: '3',
                                            border: '0 0 0 1',
                                            orientation: 'horizontal',
                                            controls: [
                                                {
                                                    id: 'image',
                                                    type: 'entityImage',
                                                    width: 25,
                                                    height: 16,
                                                    margin: '0 0 5 0'
                                                },
                                                {
                                                    type: 'label',
                                                    text: 'Operator is not selected',
                                                    width: '*',
                                                    height: '*',
                                                    onLoad: function () {
                                                        if (!this.__changed) {
                                                            this.__changed = function (sender, args) {
                                                                if (sender.length == 1) {
                                                                    this.parent.image.set_imageId(sender[0].get_imageId());
                                                                    this.set_text(sender[0].get_name());
                                                                } else {
                                                                    this.parent.image.set_imageId(0);
                                                                    this.set_text('Operator is not selected');
                                                                }
                                                            }
                                                        }

                                                        this.get_data().get_selectedOperators().add_changed(this.__changed, this);
                                                    },
                                                    onFree: function () {
                                                        if (this.__changed) {
                                                            this.get_data().get_selectedOperators().remove_changed(this.__changed);
                                                        }
                                                    }
                                                }
                                            ]
                                        },
                                        {
                                            type: 'scrollablePanel',
                                            controls: [
                                                {
                                                    id: 'countries',
                                                    cssClass: 'global_countries_list',
                                                    type: 'repeater',
                                                    layout: 'wrap',
                                                    orientation: 'horizontal',
                                                    padding: '5',
                                                    width: '100%',
                                                    emptyDataText: 'No countries selected',
                                                    template: {
                                                        type: 'panel',
                                                        width: '50%-1',
                                                        height: '20',
                                                        cssClass: 'country_row',
                                                        orientation: 'horizontal',
                                                        controls: [
                                                            {
                                                                id: 'image',
                                                                type: 'entityImage',
                                                                mode: 'exactly',
                                                                width: 25,
                                                                height: 15,
                                                                margin: '0 0 3 0'
                                                            },
                                                            {
                                                                id: 'countryName',
                                                                type: 'label',
                                                                cssClass: 'countryName',
                                                                width: '*',
                                                                height: '*',
                                                                text: ''
                                                            }
                                                        ],
                                                        bindings: {
                                                            '*': function (sender, args) {
                                                                var dataItem = args.newValue;
                                                                this.countryName.set_text(dataItem.get_name());
                                                                this.image.set_imageId(dataItem.get_imageId());
                                                            }
                                                        }
                                                    },
                                                    onLoad: function () {
                                                        this.set_dataSource(this.get_data().get_selectedCountries());
                                                    }
                                                }
                                            ]
                                        }
                                    ],
                                    blockName: 'globalMarket',
                                    headerAsCollapseButton: false,
                                    bindings: {
                                        'data:pageView': function(sender, args) {
                                            if (args.newValue == this.options.blockName) {
                                                this.addCssClass('block_selected');
                                            } else if (args.oldValue == this.options.blockName) {
                                                this.removeCssClass('block_selected');
                                            }
                                        }
                                    },
                                    domHandlers: {
                                        'click': function (sender, args) {
                                            this.parent.get_data().set_pageView(this.options.blockName);
                                        },
                                        'mouseover': function() {
                                            this.addCssClass('block_hovered');
                                        },
                                        'mouseout': function() {
                                            this.removeCssClass('block_hovered');
                                        }
                                    }
                                }
                            ],
                            onLoad: function () {
                                if (!this.__changed) {
                                    this.__changed = function (sender, args) {
                                        if (args.newValue == ProjectScope.open) {
                                            this.set_activeView('openTarget');
                                        }

                                        if (args.newValue == ProjectScope.local) {
                                            this.set_activeView('localTarget');
                                        }

                                        if (args.newValue == ProjectScope.global) {
                                            this.set_activeView('globalTarget');
                                        }
                                    };
                                }

                                this.get_data().add_scopeChanged(this.__changed, this);
                            },
                            onFree: function () {
                                this.get_data().remove_scopeChanged(this.__changed, this);
                            }
                        },
                        {
                            type: 'panel',
                            height: 25,
                            orientation: 'horizontal',
                            layout: 'wrap',
                            cssClass: 'actionbar',
                            controls: [
                                {
                                    type: 'button',
                                    width: '*',
                                    height: '*',
                                    padding: '0',
                                    border: '1',
                                    text: 'Save',
                                    onClick: function () {
                                        this.get_window().saveProject();
                                    }
                                },
                                {
                                    type: 'button',
                                    width: '*',
                                    height: '*',
                                    padding: '0',
                                    border: '1',
                                    text: 'Reset',
                                    onClick: function () {
                                        Application.showConfirm('Warning', 'Do you really want to remove all project data?', function (result) {
                                            if (result) {
                                                var data = this.get_data();
                                                data.set_handset(null);
                                                data.get_team().clear();
                                                data.get_selectedCountries().clear();
                                                data.get_selectedOperators().clear();
                                            }
                                        } .bind(this));
                                    }
                                },
                                {
                                    type: 'button',
                                    text: 'Cancel',
                                    width: '*',
                                    height: '*',
                                    padding: '0',
                                    border: '1',
                                    onClick: function () {
                                        var prj = this.get_data().get_project();

                                        if (prj) {
                                            Application.loadPage('planning/projectDetails|projectId=' + prj.get_id());
                                        } else {
                                            Application.loadPage('planning/managePlans');
                                        }
                                    }
                                }
                            ]
                        }
                    ]
                },
                {
                    type: 'collapsablePanel',
                    title: 'Team',
                    showCollapseButton: false,
                    controls: [
                        {
                            type: 'multiView',
                            id: 'multiView',
                            activeView: 'team',
                            views: [
                                {
                                    id: 'empty',
                                    type: 'panel'
                                },
                                {
                                    id: 'team',
                                    type: 'panel',
                                    layout: 'stack',
                                    orientation: 'vertical',
                                    controls: [
                                        {
                                            type: 'panel',
                                            id: 'tabPanel',
                                            layout: 'stack',
                                            orientation: 'horizontal',
                                            height: '28',
                                            padding: '2',
                                            controls: [
                                                {
                                                    type: 'button',
                                                    text: 'Prefill users',
                                                    height: '*',
                                                    margin: '0 0 5 0',
                                                    padding: '0',
                                                    border: '1',
                                                    width: '120',
                                                    onClick: function () {
                                                        this.parent.parent.teamSelector.prefillUsers();
                                                    }
                                                },
                                                {
                                                    type: 'checkbox',
                                                    id: 'cbAllUsers',
                                                    width: '120',
                                                    height: '16',
                                                    valign: 'middle',
                                                    cssClass: 'allUsersCheckbox',
                                                    text: 'Display all users',
                                                    onChanged: function (value) {
                                                        this.parent.parent.teamSelector.set_showAllUsers(value.get_state() == CheckBoxStates.checked);
                                                    }
                                                }
                                            ]
                                        },
                                        {
                                            id: 'teamSelector',
                                            type: 'teamSelector',
                                            customFunctions: {
                                                '_scopeChanged': function() {
                                                    this.updateRoles();
                                                }
                                            },
                                            handlers: {
                                                selectedRoleChanged: function (sender, args) {
                                                    var role = args.newValue;  

                                                    if (!role) {
                                                        return;
                                                    }

                                                    var cb = this.parent.tabPanel.cbAllUsers;        
                                                                                      
                                                    if (role.get_hasCountryConstraint() || role.get_hasOperatorConstraint()) {                                                
                                                        cb.set_state(CheckBoxStates.empty);
                                                        cb.raise_onChanged();
                                                        cb.set_enabled(false);
                                                    }
                                                    else
                                                        cb.set_enabled(true);                                                    
                                                }
                                            },
                                            onRolesRequired: function(callback) {
                                                var scope = this.get_data().get_scope();
                                                if (scope) {
                                                    Repository.Filter('ProjectTeamTemplate', 'Scope = ' + scope, function (result) {
                                                        result.first().get_roles(function(templateRoles) {
                                                            Repository.Get('Role', templateRoles.select('roleId'), function(roles) {
                                                                callback(roles);
                                                            });
                                                        });
                                                    });
                                                }
                                            },
                                            onLoad: function () {   
                                                this.set_selectedCountries(this.parent.get_data().get_selectedCountries());
                                                this.set_selectedOperators(this.parent.get_data().get_selectedOperators());
                                                this._handsetToSelectedLink = Auto.Property.CreateLink(this.get_data(), this, "handset", "selectedHandset");
                                                this.set_dataSource(this.parent.get_data().get_team());
                                                this.get_data().add_scopeChanged(this._scopeChanged, this);
                                            },
                                            onFree: function() {
                                                this._handsetToSelectedLink.dispose();
                                                this.set_selectedCountries([].makeObservable());
                                                this.set_selectedOperators([].makeObservable());
                                                this.get_data().remove_scopeChanged(this._scopeChanged, this);                                            }
                                        }
                                    ]
                                },
                                { // view countries
                                    id: 'countries',
                                    type: 'countriesSelector',
                                    onLoad: function () {
                                        
                                        var project = this.get_window().get_data().get_project();
                                        var filter = 
                                            (project.get_scope() == ProjectScope.open && project.get_modelLine() == HandsetModelLine.runningChange) ?
                                            'forRCEdit(' + project.get_id() + ');' :
                                            '';
                                                                                    
                                        var ds = [].makeDataSource('Country').filterBy(filter).orderBy('name');
                                        this.set_dataSource(ds);
                                        ds.load();
                                    }
                                },
                                { // view operators
                                    id: 'operators',
                                    type: 'operatorsSelector'
                                },
                                { // local market view
                                    id: 'localMarket',
                                    type: 'localMarket',
                                    onDataInit: function () {
                                        this.get_data().set_selectedCountries(this.parent.get_data().get_selectedCountries());
                                        this.get_data().set_selectedOperators(this.parent.get_data().get_selectedOperators());
                                    }
                                },
                                { // global market view
                                    id: 'globalMarket',
                                    type: 'globalMarket',                                    
                                    onLoad: function () {    
                                        var project = this.get_window().get_data().get_project();

                                        if (project.get_modelLine() == HandsetModelLine.runningChange) {
                                            this.set_countriesFilter({ name: 'forRCEdit', params: project.get_id() });
                                        }
                                             
                                        if (project.get_operators().length > 0)
                                            this.set_operatorsFilter('Id = ' + project.get_operators()[0].get_operatorId());
                                        else
                                            Application.showError('Error', 'Project with zero operators. Please check your data');                                            
                                                      
                                        this.set_selectedCountries(this.parent.get_data().get_selectedCountries());                                                                                

                                        this.add_selectedOperatorChanged(function(sender, args) {                                                              
                                            this.get_data().get_selectedOperators().clear();
                                            this.get_data().get_selectedOperators().add(args.newValue);
                                        }.bind(this));

                                        this.set_selectedOperator(this.parent.get_data().get_selectedOperators()[0]);
                                    }
                                }
                            ],
                            onActiveViewChanged: function (sender, args) {
                                if (args.newValue == 'team') {
                                    if (this.get_window().isAlignedProject()) {
                                        this.get_data().set_pageView('empty');
                                    }
                                }

                                if (args.newValue == 'countries') {
                                    var countriesSelector = args.view;
                                    countriesSelector.set_selectedCountries(this.parent.get_data().get_selectedCountries());
                                }

                                if (args.newValue == 'operators') {
                                    var operatorsSelector = args.view;

                                    if (!operatorsSelector.get_dataSource()) {
                                        operatorsSelector.set_dataSource(this.parent.get_data().get_selectedCountries());
                                        operatorsSelector.set_selectedOperators(this.parent.get_data().get_selectedOperators());
                                    }
                                }

                                if (args.newValue == 'localMarket') {
                                    args.view.init();
                                }

                                if (args.newValue == 'globalMarket') {
                                    args.view.init();
                                }
                            },
                            onLoad: function () {
                                if (!this.__changed) {
                                    this.__changed = function (sender, args) {
                                        this.set_activeView(args.newValue);
                                    };
                                }

                                this.get_data().add_pageViewChanged(this.__changed, this);
                            },
                            onFree: function () {
                                this.get_data().remove_pageViewChanged(this.__changed, this);
                            }
                        }
                    ],
                    onLoad: function () {
                        if (!this.__changed) {
                            this.__changed = function (sender, args) {
                                if (args.newValue == 'team') {
                                    this.set_title('Team');
                                }

                                if (args.newValue == 'countries') {
                                    this.set_title('Countries');
                                }

                                if (args.newValue == 'operators') {
                                    this.set_title('Operators');
                                }

                                if (args.newValue == 'localMarket') {
                                    this.set_title('Local Market');
                                }

                                if (args.newValue == 'globalMarket') {
                                    this.set_title('Global Market');
                                }
                                
                                if (args.newValue == 'empty') {
                                    this.set_title('');
                                }
                            };
                        }

                        this.get_data().add_pageViewChanged(this.__changed, this);
                    },
                    onFree: function () {
                        this.get_data().remove_pageViewChanged(this.__changed);
                    }
                }
            ]
        }
    ]
})