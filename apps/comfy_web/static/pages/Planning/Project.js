({
    title: "Project",
    layout: "stack",
    orientation: 'vertical',
    valign: 'stretch',
    data: [ // something like a model...
        "handset",
        { name: "scope", value: ProjectScope.open },
        { name: 'pageView', value: 'handsets' },
        { name: 'team', value: [].makeObservable() },
        { name: 'selectedCountries', value: [].makeSorted('name') },
        { name: 'selectedOperators', value: [].makeObservable() },
        'validationError',
        'planSource',
        { name: 'taskAlignType', value: 'from' },
        'alignDate',
        'description',
        { name: 'inheritType', value: PlanOwnerType.template },
        { name: 'projectType', value: HandsetModelLine.base },
        'departmentId',
        'showedProjectId' // identifier for popup with project details
    ],
    customFunctions: {
        //#region Controller
        'getProjectData': function () {
            var data = this.get_data();

            return {
                handset: data.get_handset(),
                countries: data.get_selectedCountries().clone(),
                operators: data.get_selectedOperators().clone(),
                userRoles: data.get_team().clone(),
                scope: data.get_scope(),
                inheritType: data.get_planSource() instanceof ProjectTemplate ? PlanOwnerType.template : PlanOwnerType.project,
                planSourceId: data.get_planSource() ? data.get_planSource().get_id() : 0,
                taskAlignType: data.get_taskAlignType() || "from",
                alignDate: data.get_alignDate(),
                projectType: data.get_projectType(),
                departmentId: data.get_departmentId(),
                description: data.get_description()
            };
        },
        'saveProjectData': function (withValidate) {
            var data = this.get_data();
            withValidate = isNullOrUndefined(withValidate) ? true : withValidate;

            Services.ProjectService.CreateNewProject(               
                this.getProjectData(), 
                !withValidate,
                function (result) {
                    Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
                },
                function (validationError) {
                    data.set_validationError(validationError);
                }
            );
        },
        'canInheritStartDateFromBaseProject': function () {
            var data = this.get_data();
            return !data.get_alignDate() &&
                data.get_planSource() &&
                !(data.get_planSource() instanceof ProjectTemplate) &&
                (data.get_taskAlignType() === "from" || !data.get_taskAlignType());
        },
        'saveProject': function (withValidate) {
            var data = this.get_data();
            var controller = this;

            if (this.canInheritStartDateFromBaseProject()) {
                Application.showConfirm('Warning', 'Do you want to inherit start date from the base project?', function (result) {
                    if (result) {
                        data.set_alignDate(data.get_planSource().get_from());
                        controller.saveProjectData(withValidate);
                    }
                });
            }
            else {
                controller.saveProjectData(withValidate);
            }
        },
        'createBranch': function (globalProject) {
            Services.ProjectService.CreateAsBranch(this.getProjectData(), globalProject, function (result) {
                Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
            });
        },
        'promote': function (localProjects) {
            Services.ProjectService.Promote(this.getProjectData(), localProjects, function (result) {
                Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
            });
        },
        'joinToProject': function (globalProject) {
            Services.ProjectService.Join(this.getProjectData(), globalProject, function (result) {
                Application.loadPage('planning/projectDetails|projectId=' + result.get_id());
            });
        },
        'isCopyTeamAvailable': function() {
            var data = this.get_data(),
                parentProject = data.get_planSource();

            return parentProject == null || data.get_inheritType() == 'Project';
        },
        'copyTeam': function () {
            var data = this.get_data(),
                parentProject = data.get_planSource(),
                handsetId = data.get_handset() ? data.get_handset().get_id() : null,
                countriesParam = data.get_selectedCountries().select('id'),
                operatorsParam = data.get_selectedOperators().select(
                    function (op) {
                        return op instanceof SubsidiaryOperator ? op.get_operatorId() : op.get_id();
                    }
                );

            if (!this.isCopyTeamAvailable()) {
                throw new Error("The \"Copy team\" function is not available");
            }

            Services.UserRoleService.GetApplicableUserRolesFromProject(parentProject.get_id(), countriesParam, operatorsParam, handsetId, function (result) {
                data.get_team().clear();

                for (var i = result.length - 1; i >= 0; i--) {
                    var ur = new UserRole(result[i]);
                    ur.set_projectId(-1);

                    data.get_team().add(ur);
                }
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
        }
        //#endregion
    },
    cssClass: 'create_project_page',

    onLoad: function () {
        this.get_data().get_selectedCountries().add_changed(this._countriesChanged, this);
    },

    onFree: function () {
        this.get_data().get_selectedCountries().remove_changed(this._countriesChanged, this);
    },

    controls:
    [
    //#region Project Details Popup
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
    //#endregion
    //#region Validation Error Popup
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
                            },
                                {
                                    id: 'globalProjectJoinSuggestion',
                                    type: 'marketConflictList',
                                    showJoinButton: true,
                                    height: '?',
                                    onJoinClick: function (sender, args) {
                                        this.get_window().get_window().joinToProject(args.project);
                                    }
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

            if (args.button == 'Promote All') {
                this.get_window().promote(multiView.marketsConflict.get_dataSource());
            }
        },
        onLoad: function () {
            this.get_data().add_validationErrorChanged(function (sender, args) {                
                var errors = args.newValue;
                var multiView = this.container.scrollBox.multiView;

                if (errors) {
                    if (errors instanceof Array && errors.length > 0) {
                        var marketsConflict = errors.ofType(MarketsConflictFaultContract).first();
                        var duplicateErrors = errors.ofType(DuplicateProjectFaultContract);
                        var globalProjectJoinSuggestion = errors.ofType(GlobalProjectJoinSuggestionContract).first();

                        if (marketsConflict != null) {
                            this.container.lblError.set_text("The following project targets for selected market already exists.");
                            multiView.set_activeView('marketsConflict');

                            if (this.get_data().get_scope() == ProjectScope.open) {
                                this.set_buttons(["Close"]);
                            }

                            if (this.get_data().get_scope() == ProjectScope.global) {
                                this.set_buttons(["Promote All", "Close"]);
                            }

                            if (this.get_data().get_scope() == ProjectScope.local) {
                                this.set_buttons(["Create branch", "Close"]);
                            }

                            multiView.marketsConflict.set_dataSource(marketsConflict.get_conflictProjects());
                        } else if (duplicateErrors.length > 0) {
                            multiView.set_activeView('marketsConflict');

                            if (this.get_data().get_scope() == ProjectScope.open) {
                                this.container.lblError.set_text("Similar project targets already exists. Would you like to continue?");
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

                            multiView.marketsConflict.set_dataSource(duplicateErrors.select(function (err) { return err.get_existingProject(); }).makeObservable());
                        } else if (globalProjectJoinSuggestion != null) {
                            multiView.set_activeView('globalProjectJoinSuggestion');

                            if (this.get_data().get_scope() == ProjectScope.local) {
                                this.container.lblError.set_text("Similar project target already exists. New local project target should be joined to one of existing global project targets.");
                                this.set_buttons(["Continue saving", "Close"]);
                            }

                            multiView.globalProjectJoinSuggestion.set_dataSource(globalProjectJoinSuggestion.get_conflictProjects());
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
    //#endregion
    //#region Page Title
        {
        id: 'pageHeader',
        type: "pageHeader",
        title: "Create Project Target",
        description: "Create Project Target page"
    },
    //#endregion
        {
        type: 'panel',
        orientation: 'horizontal',
        controls: [
        //#region Left Panel
                {
                type: 'panel',
                width: '330',
                margin: '0 0 3 0',
                cssClass: 'project_page_left_column',
                controls: [
                            {
                                type: 'collapsablePanel',
                                title: 'Basic Properties',
                                orientation: 'horizontal',
                                cssClass: 'project_scope_panel',
                                cookieKey: 'project_scope',
                                padding: '5',
                                height: '?',
                                margin: '0 0 0 3',
                                controls: [
                                    {
                                        type: 'form',
                                        height: '?',
                                        controls: [
                                            {
                                                type: 'dropDownList',
                                                height: '20',
                                                'form.label': 'Scope:',
                                                textProperty: 'text',
                                                items: [
                                                    { text: ProjectScope.open.toString(), id: ProjectScope.open },
                                                    { text: ProjectScope.local.toString(), id: ProjectScope.local },
                                                    { text: ProjectScope.global.toString(), id: ProjectScope.global }
                                                ],
                                                customFunctions: {
                                                    '__checkChanged': function (sender, args) {
                                                        var ds = this.get_items();

                                                        for (var i = 0; i < ds.length; i++) {
                                                            if (ds[i].id === args.newValue) {
                                                                this.set_selectedValue(ds[i]);
                                                            }
                                                        }
                                                    }
                                                },
                                                onLoad: function () {
                                                    this.get_data().add_scopeChanged(this.__checkChanged, this);
                                                    this.__checkChanged.bind(this)(this, { newValue: this.get_data().get_scope() });
                                                },
                                                onFree: function () {
                                                    this.get_data().remove_scopeChanged(this.__checkChanged, this);
                                                },
                                                onChanged: function (sender, args) {
                                                    var selectedCountries = this.get_data().get_selectedCountries();
                                                    var selectedOperators = this.get_data().get_selectedOperators();
                                                    var team = this.get_data().get_team();

                                                    var scopeId = args.newValue.id;

                                                    var checkScope = function () {
                                                        var scope = this.get_data().get_scope();
                                                        var pageView = this.get_data().get_pageView();

                                                        if (scope == ProjectScope.local && (pageView == 'countries' || pageView == 'operators' || pageView == 'globalMarket')) {
                                                            this.get_data().set_pageView('localMarket');
                                                        }

                                                        if (scope == ProjectScope.global && (pageView == 'countries' || pageView == 'operators' || pageView == 'localMarket')) {
                                                            this.get_data().set_pageView('globalMarket');
                                                        }

                                                        if (scope == ProjectScope.open && (pageView == 'globalMarket' || pageView == 'localMarket')) {
                                                            this.get_data().set_pageView('countries');
                                                        }

                                                        if (pageView == 'team') {
                                                            this.get_data().set_pageView('handsets');
                                                        }
                                                    } .bind(this);

                                                    if (selectedCountries.length >= 1 || selectedOperators.length >= 1 || team.length >= 1) {
                                                        Application.showConfirm('Warning', 'Changing of the scope will reset selected countries, selected operators and team. Do you want to continue?', function (result) {
                                                            if (result) {
                                                                selectedCountries.clear();
                                                                selectedOperators.clear();
                                                                team.clear();

                                                                this.get_data().set_scope(scopeId);
                                                                checkScope();
                                                            } else {
                                                                this.set_selectedValue(args.oldValue);
                                                            }
                                                        } .bind(this));
                                                    } else {
                                                        this.get_data().set_scope(scopeId);
                                                        checkScope();
                                                    }
                                                }
                                            },
                                            {
                                                id: 'projectType',
                                                type: 'dropDownList',
                                                height: '20',
                                                'form.label': 'Type:',
                                                items: [
                                                    HandsetModelLine.base, HandsetModelLine.countryAdaptation
                                                ],
                                                onChanged: function (sender, args) {
                                                    this.get_data().set_projectType(args.newValue);
                                                }
                                            },
                                            {
                                                id: 'departmentId',
                                                type: 'dropDownList',
                                                textProperty: 'name',
                                                'form.label': 'R&D:',
                                                valueProperty: 'id',
                                                notSelectedItem: {
                                                    name: '-'
                                                },
                                                bindings: {
                                                    'data:departmentId': 'selectedValue'
                                                },
                                                onLoad: function (sender, args) {
                                                    var ds = [].makeDataSource('Department').filterBy('isArchived = false');
                                                    ds.load();
                                                    this.set_items(ds);
                                                }
                                            },
                                            {
                                                type: 'textBox',
                                                'form.label': 'Description:',
                                                mode: 'multiline',
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
                                        id: 'handsetNotSelected',
                                        type: 'label',
                                        text: 'Handset is not selected',
                                        width: '*',
                                        height: '20',
                                        valign: 'middle'
                                    },
                                    {
                                        id: 'handsetInfo',
                                        type: 'panel',
                                        height: '65',
                                        visible: false,
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
                                ],
                                blockName: 'handsets',
                                headerAsCollapseButton: false,
                                bindings: {
                                    'data:pageView': function (sender, args) {
                                        if (args.newValue == this.options.blockName) {
                                            this.addCssClass('block_selected');
                                        } else if (args.oldValue == this.options.blockName) {
                                            this.removeCssClass('block_selected');
                                        }
                                    },
                                    'data:handset': function (sender, args) {
                                        var inner = this.innerPanel;
                                        var handsetInfo = inner.handsetInfo;
                                        var handsetNotSelected = inner.handsetNotSelected;
                                        var handset = args.newValue;

                                        if (handset) {
                                            handsetNotSelected.hide();
                                            handsetInfo.show(true);

                                            var details = handsetInfo.details;
                                            var title = handset.get_code() + '/' + handset.get_name();
                                            this.set_title('Handset Model: ' + title);

                                            inner.image.set_imageId(handset.get_imageId());
                                            handsetInfo.name.set_text(title);

                                            details["releaseDate"].set_text(handset.get_releaseDate());
                                            details["class"].set_text(handset.get_class());
                                        } else {
                                            handsetInfo.hide();
                                            handsetNotSelected.show(true);
                                            inner.image.set_imageId(-1);
                                        }
                                    }
                                },
                                domHandlers: {
                                    'click': function (sender, args) {
                                        this.parent.get_data().set_pageView(this.options.blockName);
                                    },
                                    'mouseover': function () {
                                        this.addCssClass('block_hovered');
                                    },
                                    'mouseout': function () {
                                        this.removeCssClass('block_hovered');
                                    }
                                }
                            },
                            {
                                type: 'collapsablePanel',
                                title: 'Planning',
                                layout: 'wrap',
                                orientation: 'horizontal',
                                cssClass: 'project_team_panel',
                                height: '?',
                                margin: '0 0 0 3',
                                padding: '5',
                                cookieKey: 'planningPanel',
                                bindings: {
                                    'alignDate': function (sender, args) {
                                        var value = args.newValue;
                                        this.alignDate.set_text(value ? value.format('d') : 'not selected');
                                    },
                                    'planSource': function (sender, args) {
                                        var value = args.newValue;
                                        this.sourceName.set_text(value ? value.get_name() : 'not selected');
                                        this.inheritType.set_text(value ? (value instanceof ProjectTemplate ? 'Template' : 'Project') : '-');
                                    },
                                    'taskAlignType': function (sender, args) {
                                        var value = args.newValue;
                                        this.taskAlignType.set_text(value == 'to' ? 'Target ends at:' : 'Target starts at:');
                                    },
                                    'data:pageView': function (sender, args) {
                                        if (args.newValue == this.options.blockName) {
                                            this.addCssClass('block_selected');
                                        } else if (args.oldValue == this.options.blockName) {
                                            this.removeCssClass('block_selected');
                                        }
                                    }
                                },
                                controls: [
                                    {
                                        id: 'taskAlignType',
                                        type: 'label',
                                        width: '100',
                                        height: '18',
                                        text: 'Project starts at:',
                                        cssClass: 'form_label'
                                    },
                                    {
                                        type: 'label',
                                        id: 'alignDate',
                                        height: '18',
                                        width: '100%-100'
                                    },
                                    {
                                        type: 'label',
                                        width: '100',
                                        height: '18',
                                        text: 'Inherit from:',
                                        cssClass: 'form_label'
                                    },
                                    {
                                        type: 'label',
                                        id: 'inheritType',
                                        text: '',
                                        height: '18',
                                        width: '60'
                                    },
                                    {
                                        type: 'label',
                                        id: 'sourceName',
                                        text: '',
                                        height: '18',
                                        width: '100%-160'
                                    }
                                ],
                                onLoad: function () {
                                    this.set_dataSource(this.get_data());
                                },
                                blockName: 'templates',
                                headerAsCollapseButton: false,
                                domHandlers: {
                                    'click': function (sender, args) {
                                        this.parent.get_data().set_pageView(this.options.blockName);
                                    },
                                    'mouseover': function () {
                                        this.addCssClass('block_hovered');
                                    },
                                    'mouseout': function () {
                                        this.removeCssClass('block_hovered');
                                    }
                                }
                            },
                            {
                                type: 'scrollablePanel',
                                margin: '0 0 0 3',
                                bindings: {
                                    'data:scope': function (sender, args) {
                                        this.countriesTarget.hide();
                                        this.operatorsTarget.hide();
                                        this.localTarget.hide();
                                        this.globalTarget.hide();

                                        if (args.newValue === ProjectScope.open) {
                                            this.countriesTarget.show();
                                            this.operatorsTarget.show();
                                        }

                                        if (args.newValue === ProjectScope.local) {
                                            this.localTarget.show();
                                        }

                                        if (args.newValue === ProjectScope.global) {
                                            this.globalTarget.show();
                                        }
                                    }
                                },
                                controls: [
                                    {
                                        id: 'countriesTarget',
                                        type: 'collapsablePanel',
                                        title: 'Countries',
                                        cookieKey: 'countriesPanel',
                                        cssClass: 'countries_list',
                                        margin: '0 0 0 3',
                                        minHeight: 100,
                                        visible: false,
                                        controls: [
                                            {
                                                type: 'scrollablePanel',
                                                controls: [
                                                    {
                                                        id: 'countries',
                                                        type: 'repeater',
                                                        layout: 'wrap',
                                                        padding: '5',
                                                        orientation: 'horizontal',
                                                        width: '100%',
                                                        height: '?',
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
                                                                    width: 25,
                                                                    height: 15,
                                                                    margin: '0 0 3 0'
                                                                },
                                                                {
                                                                    id: 'countryName',
                                                                    type: 'label',
                                                                    width: '*',
                                                                    height: '*',
                                                                    cssClass: 'countryName',
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
                                        bindings: {
                                            'data:pageView': function (sender, args) {
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
                                            'mouseover': function () {
                                                this.addCssClass('block_hovered');
                                            },
                                            'mouseout': function () {
                                                this.removeCssClass('block_hovered');
                                            }
                                        }
                                    },
                                    {
                                        id: 'operatorsTarget',
                                        type: 'collapsablePanel',
                                        title: 'Operators',
                                        minHeight: 100,
                                        cookieKey: 'operatorsPanel',
                                        margin: '0 0 0 3',
                                        visible: false,
                                        scrolling: true,
                                        controls: [
                                            {
                                                id: 'operatorsTree',
                                                type: 'operatorsTree',
                                                width: '*',
                                                height: '?',
                                                padding: '5'
                                            }
                                        ],
                                        blockName: 'operators',
                                        headerAsCollapseButton: false,
                                        bindings: {
                                            'data:selectedOperators': 'operatorsTree',
                                            'data:pageView': function (sender, args) {
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
                                            'mouseover': function () {
                                                this.addCssClass('block_hovered');
                                            },
                                            'mouseout': function () {
                                                this.removeCssClass('block_hovered');
                                            }
                                        }
                                    },
                                    {
                                        id: 'localTarget',
                                        type: 'collapsablePanel',
                                        title: 'Local Market',
                                        padding: '5',
                                        visible: false,
                                        margin: '0 0 0 3',
                                        height: '?',
                                        cookieKey: 'localMarketPanel',
                                        cssClass: 'local_target',
                                        controls: [
                                            {
                                                type: 'form',
                                                height: '?',
                                                controls: [
                                                    {
                                                        type: 'label',
                                                        width: '*',
                                                        height: '18',
                                                        'form.label': 'Country:',
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
                                                        width: '*',
                                                        text: 'not selected',
                                                        'form.label': 'Operator:',
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
                                            'data:pageView': function (sender, args) {
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
                                            'mouseover': function () {
                                                this.addCssClass('block_hovered');
                                            },
                                            'mouseout': function () {
                                                this.removeCssClass('block_hovered');
                                            }
                                        }
                                    },
                                    {
                                        id: 'globalTarget',
                                        type: 'collapsablePanel',
                                        title: 'Global Market',
                                        cookieKey: 'globalMarketPanel',
                                        margin: '0 0 0 3',
                                        minHeight: 150,
                                        height: '2*',
                                        visible: false,
                                        cssClass: 'global_target',
                                        padding: '5',
                                        controls: [
                                            {
                                                type: 'panel',
                                                cssClass: 'operator_name',
                                                height: 22,
                                                orientation: 'horizontal',
                                                border: '0 0 0 1',
                                                controls: [
                                                    {
                                                        id: 'image',
                                                        type: 'entityImage',
                                                        margin: '0 0 5 0',
                                                        width: 20,
                                                        height: 15
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
                                                        padding: '5',
                                                        type: 'repeater',
                                                        layout: 'wrap',
                                                        orientation: 'horizontal',
                                                        height: '?',
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
                                        blockName: 'globalMarket',
                                        headerAsCollapseButton: false,
                                        bindings: {
                                            'data:pageView': function (sender, args) {
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
                                            'mouseover': function () {
                                                this.addCssClass('block_hovered');
                                            },
                                            'mouseout': function () {
                                                this.removeCssClass('block_hovered');
                                            }
                                        }
                                    },
                                    {
                                        type: 'collapsablePanel',
                                        title: 'Team',
                                        orientation: 'horizontal',
                                        minHeight: 100,
                                        height: '2*',
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
                                                        height: '?',
                                                        padding: '5',
                                                        showCheckBoxes: false,
                                                        onLoad: function () {
                                                            this.set_dataSource(this.parent.get_data().get_team());
                                                        }
                                                    }
                                                ]
                                            }
                                        ],
                                        blockName: 'team',
                                        headerAsCollapseButton: false,
                                        bindings: {
                                            'data:pageView': function (sender, args) {
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
                                            'mouseover': function () {
                                                this.addCssClass('block_hovered');
                                            },
                                            'mouseout': function () {
                                                this.removeCssClass('block_hovered');
                                            }
                                        }
                                    }
                                ]
                            },
                // #region Action Bar
                            {
                            type: 'panel',
                            height: '25',
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
                                        border: '1',
                                        padding: '0',
                                        text: 'Reset',
                                        onClick: function () {
                                            Application.showConfirm('Warning', 'Do you really want to reset all project data?', function (result) {
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
                                        border: '1',
                                        padding: '0',
                                        onClick: function () {
                                            Application.loadPage('planning/managePlans');
                                        }
                                    }
                                ]
                        }
                //#endregion
                        ]
            },
        //#endregion
        //#region Right Panel
                {
                type: 'collapsablePanel',
                title: 'Handset Model',
                height: '100%',
                showCollapseButton: false,
                innerPanelCssClass: 'section_no_padding',
                controls: [
                        {
                            type: 'multiView',
                            id: 'multiView',
                            width: '100%',
                            height: '100%',
                            activeView: 'handsets',
                            views: [
                            //#region Team
                                {
                                id: 'team',
                                type: 'panel',
                                controls: [
                                        {
                                            id: 'tabPanel',
                                            type: 'panel',
                                            orientation: 'horizontal',
                                            height: '28',
                                            padding: '2',
                                            controls: [
                                                {
                                                    type: 'button',
                                                    text: 'Copy team',
                                                    width: '100',
                                                    height: '*',
                                                    margin: '0 0 5 0',
                                                    padding: '0',
                                                    border: '1',
                                                    tooltip: 'Copy team from parent target',
                                                    enabled: false,
                                                    bindings: {
                                                        'planSource': function (sender, args) {
                                                            var value = args.newValue;
                                                            this.set_enabled(value != null && !(value instanceof ProjectTemplate));
                                                        }
                                                    },
                                                    onClick: function () {
                                                        this.get_window().copyTeam();
                                                    },
                                                    onLoad: function () {
                                                        this.set_dataSource(this.get_data());
                                                    }
                                                },
                                                {
                                                    type: 'button',
                                                    text: 'Prefill users',
                                                    width: '120',
                                                    height: '*',
                                                    border: '1',
                                                    padding: '0',
                                                    margin: '0 0 5 0',
                                                    onClick: function () {
                                                        this.parent.parent.teamSelector.prefillUsers();
                                                    }
                                                },
                                                {
                                                    id: 'cbAllUsers',
                                                    type: 'checkbox',
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
                                            handlers: {
                                                selectedRoleChanged: function (sender, args) {
                                                    var role = args.newValue;
                                                    var cb = this.parent.tabPanel.cbAllUsers;
                                                    if (role.get_hasOperatorConstraint() || role.get_hasCountryConstraint()) {
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
                                            },
                                            onFree: function() {
                                                this._handsetToSelectedLink.dispose();
                                                this.set_selectedCountries([].makeObservable());
                                                this.set_selectedOperators([].makeObservable());
                                            }
                                        }
                                    ]
                            },
                            //#endregion

                            //#region Handsets
                                {
                                id: 'handsets',
                                type: 'panel',
                                bindings: {
                                    '*': 'scrollable'
                                },
                                controls: [
                                        {
                                            id: 'scrollable',
                                            type: 'scrollablePanel',
                                            padding: '5',
                                            bindings: {
                                                '*': 'handsetList'
                                            },
                                            controls: [
                                                {
                                                    id: 'handsetList',
                                                    type: 'handsetList',
                                                    handlers: {
                                                        onHandsetClick: function (sender, args) {
                                                            var pageData = this.get_data();
                                                            //pageData.set_handsetsPopupState(false);
                                                            pageData.set_handset(args.item);
                                                        }
                                                    },
                                                    customFunctions: {
                                                        _handsetChanged: function (sender, args) {
                                                            this.set_selectedHandset(args.newValue);
                                                        }
                                                    },
                                                    onLoad: function () {
                                                        this.get_data().add_handsetChanged(this._handsetChanged, this);
                                                    },
                                                    onFree: function () {
                                                        this.get_data().remove_handsetChanged(this._handsetChanged, this);
                                                    }
                                                }
                                            ]
                                        },
                                        {
                                            type: 'searchPane',
                                            onSearch: function (sender, args) {
                                                if (this.parent.get_dataSource()) {
                                                    this.parent.get_dataSource().set_searchQuery(args.query, true);
                                                }
                                            }
                                        }
                                    ]
                            },
                            //#endregion

                            //#region Countries
                                {
                                id: 'countries',
                                type: 'countriesSelector',
                                onLoad: function () {
                                    var ds = [].makeDataSource('Country').filterBy('').orderBy('Name');
                                    this.set_dataSource(ds);
                                    ds.load();
                                }
                            },
                            //#endregion

                            //#region Operators
                                {
                                id: 'operators',
                                type: 'operatorsSelector'
                            },
                            //#endregion

                            //#region Local Market
                                {
                                id: 'localMarket',
                                type: 'localMarket',
                                onDataInit: function () {
                                    this.get_data().set_selectedCountries(this.parent.get_data().get_selectedCountries());
                                    this.get_data().set_selectedOperators(this.parent.get_data().get_selectedOperators());
                                }
                            },
                            //#endregion

                            //#region Global Market
                                {
                                id: 'globalMarket',
                                type: 'globalMarket',
                                onLoad: function () {
                                    this.set_selectedCountries(this.parent.get_data().get_selectedCountries());                                                                                

                                    this.add_selectedOperatorChanged(function(sender, args) {                                                              
                                        this.get_data().get_selectedOperators().clear();
                                        this.get_data().get_selectedOperators().add(args.newValue);
                                    }.bind(this));
                                }
                            },
                            //#endregion

                            //#region Planning
                                {
                                id: 'templates',
                                type: 'projectPlanningSelector',
                                onLoad: function () {
                                    this.set_dataSource(this.get_data());
                                }
                            }
                            //#endregion
                            ],
                            onActiveViewChanged: function (sender, args) {
                                if (args.newValue == 'handsets') {
                                    if (!args.view.get_dataSource()) {
                                        var ds = [].makeDataSource('Handset').filterBy('').orderBy('name');
                                        args.view.set_dataSource(ds);
                                        ds.load();
                                    }
                                }

                                if (args.newValue == 'countries') {
                                    var countriesSelector = args.view;
                                    countriesSelector.set_selectedCountries(this.parent.get_data().get_selectedCountries());
                                }

                                if (args.newValue == 'operators') {
                                    var operatorsSelector = args.view;

                                    if (!operatorsSelector.get_dataSource()) {
                                        operatorsSelector.set_selectedOperators(this.parent.get_data().get_selectedOperators());
                                        operatorsSelector.set_dataSource(this.parent.get_data().get_selectedCountries());
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

                                this.set_activeView(this.get_data().get_pageView());
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

                            if (args.newValue == 'handsets') {
                                this.set_title('Handset Model');
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

                            if (args.newValue == 'templates') {
                                this.set_title('Planning');
                            }
                        };
                    }

                    this.get_data().add_pageViewChanged(this.__changed, this);
                },
                onFree: function () {
                    this.get_data().remove_pageViewChanged(this.__changed);
                }
            }
        //#endregion
            ]
    }
    ]
})