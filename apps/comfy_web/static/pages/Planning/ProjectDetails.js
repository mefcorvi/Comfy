({
    title: "Project Details",
    // #region Model
    data: [
        { name: 'projectId' },
        'project', 'projectInfo', 'scope', 'handset', 'userPopupData',
        { name: 'team', value: [].makeObservable() },
        { name: 'countries', value: [].makeSorted(function (a, b) { var n1 = a.country.get_name(); n2 = b.country.get_name(); return (n1 > n2) ? 1 : (n1 === n2 ? 0 : -1); }) },
        { name: 'operators', value: [].makeSorted('name') },
        { name: 'buttonsList', value: [].makeObservable() }
    ],
    // #endregion
    cssClass: 'project_details_page',

    // #region Bindings
    bindings: {
        'history:projectId': function (sender, args) {
            if (args.newValue) {
                this.loadProject(args.newValue);
            }
        },
        'data:buttonsList': 'buttonsList'
    },
    // #endregion

    // #region Controller
    customFunctions: {
        // #region Load Project Inner Functions
        '_loadOpenProject': function (openProject) {
            var data = this.get_data(),
                i;

            openProject.get_countries(function (countries) {
                if (countries.length === 1) {
                    data.get_countries().add({ country: countries[0], project: '' });
                } else {
                    openProject.get_relatedProjects(function (relatedProjects) {
                        for (i = 0; i < relatedProjects.length; i++) {
                            var project = relatedProjects[i];
                            var countryId = project.get_countryId();

                            Repository.Get('Country', countryId, function (country) {
                                data.get_countries().add({ country: country, project: project });
                            });
                        }
                    });
                }
            });

            openProject.get_operators(function (res) {
                data.get_operators().synchronize(res);
            });
        },

        '_loadGlobalProject': function (globalProject) {
            var data = this.get_data();

            globalProject.get_relatedProjects(function (relatedProjects) {
                var i;
                for (i = 0; i < relatedProjects.length; i++) {
                    var project = relatedProjects[i];
                    var opId = project.get_operatorId();

                    Repository.Get('LocalOperator', opId, (function (project) {
                        return function (operator) {
                            var countryId = operator.get_countryId();
                            Repository.Get('Country', countryId, function (country) {
                                data.get_countries().add({ country: country, project: project });
                            });
                        }
                    })(relatedProjects[i]));
                }
            });

            globalProject.get_operators(function (dataOperators) {
                if (dataOperators.length > 0) {
                    Repository.Get('GlobalOperator', dataOperators[0].get_operatorId(), function (operator) {
                        data.get_operators().add(operator);
                    });
                }
            });
        },

        '_loadLocalProject': function (localProject) {
            var data = this.get_data();
            Repository.Get('LocalOperator', localProject.get_operatorId(), function (operator) {
                data.get_operators().add(operator);

                Repository.Get('Country', operator.get_countryId(), function (country) {
                    data.get_countries().add({ country: country });
                });
            });
        },

        '_loadOpenBranchProject': function (openBranchProject) {
            var data = this.get_data();
            data.set_scope(ProjectScope.open);

            Repository.Get('Country', openBranchProject.get_countryId(), function (country) {
                data.get_countries().add({ country: country, project: openBranchProject });
            });
            openBranchProject.get_operators(function (res) {
                data.get_operators().synchronize(res)
            });
        },
        // #endregion

        // #region Actions for the project
        'loadProject': function (projectId) {
            var data = this.get_data();
            data.set_projectId(projectId);

            Repository.Filter('HandsetProjectInfo', 'Project.Id = ' + projectId, function (result) {
                if (result.length == 1) {
                    data.set_projectInfo(result[0]);
                }
            });

            Repository.Load('HandsetProject', projectId, function (result) {
                if (!result) {
                    Application.load404();
                    return;
                }

                data.get_countries().clear();
                data.get_operators().clear();
                data.get_team().clear();
                data.set_project(result);

                Repository.Get('Handset', result.get_handsetId(), function (handset) {
                    data.set_handset(handset);
                });

                var scope = ProjectScope.getByProject(result);
                data.set_scope(scope);

                if (result instanceof OpenProject) {
                    this._loadOpenProject(result);
                }

                if (result instanceof GlobalProject) {
                    this._loadGlobalProject(result);
                }

                if (result instanceof LocalProject) {
                    this._loadLocalProject(result);
                }

                if (result instanceof OpenBranchProject) {
                    this._loadOpenBranchProject(result);
                }

                result.get_userRoles(function (res) {
                    data.get_team().synchronize(res);
                });
            } .bind(this));
        },

        'editProject': function () {
            var project = this.get_data().get_project();
            var projectId = this.get_data().get_projectId();

            if (project.get_isLocalWithParentAligned() || project.get_isOpenBranch()) {
                this.editLocalProjectPopup.set_uri('planning/project/editLocalProject');
                this.editLocalProjectPopup.open();
            } else {
                Application.loadPage('planning/projectEdit|projectId=' + projectId);
            }
        },

        'editProjectInfo': function () {
            this.editSummaryPopup.open();
        },

        'dqmsApproval': function () {
            var project = this.get_data().get_project();

            Application.showConfirm('Warning', 'Do you really want to approve the project?', function (result) {
                if (result) {
                    Services.ProjectService.ApproveProject(project);
                }
            });
        },

        'dropProject': function () {
            var project = this.get_data().get_project();

            Application.showConfirm('Warning', 'Do you really want to drop the project?', function (result) {
                if (result) {
                    Services.ProjectService.DropProject(project);
                }
            });
        },

        'onHold': function () {
            var project = this.get_data().get_project();

            Application.showConfirm('Warning', 'Do you really want to set status as \'On Hold\'?', function (result) {
                if (result) {
                    Services.ProjectService.HoldProject(project);
                }
            });
        },

        'unHold': function () {
            var project = this.get_data().get_project();
            Services.ProjectService.UnHoldProject(project);
        },

        'createRunningChanges': function () {
            var projectId = this.get_data().get_projectId();
            this.runningChangesPopup.set_uri('planning/project/runningChanges|projectId=' + projectId);
            this.runningChangesPopup.open();
        },

        'splitByCountries': function () {
            this.splitByCountriesPopup.set_uri('planning/project/splitByCountries|projectId=' + this.get_data().get_projectId());
            this.splitByCountriesPopup.open();
        },

        'reOpen': function () {
            var project = this.get_data().get_project();
            Services.ProjectService.ReOpenProject(project);
        },

        'approvalRequest': function () {
            var project = this.get_data().get_project();
            Services.ProjectService.EnableApprovalRequest(project);
        },

        'stopApprovalRequest': function () {
            var project = this.get_data().get_project();
            Services.ProjectService.DisableApprovalRequest(project);
        },

        'detachFromGlobal': function () {
            var data = this.get_data(),
                project = data.get_project();

            this.detachFromGlobalPopup.set_uri('planning/project/detachFromGlobal|projectId=' + project.get_id());
            this.detachFromGlobalPopup.open();
        },

        'joinWithParent': function () {
            var data = this.get_data(),
                project = data.get_project();

            Application.showConfirm('Warning', 'By joining local project with Parent, its current plan/information will be lost. Are you sure you would like to proceed?', function (result) {
                if (result) {
                    Services.ProjectService.JoinWithParent(project, function (res) {
                        data.set_project(res);
                    });
                }
            });
        },

        'detachFromOpen': function () {
            this.detachFromOpenPopup.set_uri('planning/project/detachFromOpen|projectId=' + this.get_data().get_projectId());
            this.detachFromOpenPopup.open();
        },

        'editApprovers': function () {
            var project = this.get_data().get_project();

            project.get_roles(function (result) {
                this.approversEditPopup.set_dataSource(project);
                this.approversEditPopup.open();
            } .bind(this));
        },

        // #endregion

        // #region Update Buttons List

        updateButtonsList: function () {
            var data = this.get_data(),
                buttons = data.get_buttonsList(),
                projectId = data.get_projectId(),
                project = data.get_project(),
                status = project.get_status(),
                user = Application.get_currentUser();
            
            buttons.clear();

            // Show buttons depending on rights

            // Edit
            if (project.hasStatus(['draft', 'onProgress'])) {
                if(user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                    buttons.add(['Edit']);
                }
            }

            // DQMS Approval Received
            if (project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if(project.hasStatus(['onProgress'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['DQMS Approval Received']);
                    }
                }
            }

            // Drop
            if (project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if(project.hasStatus(['draft', 'onProgress', 'onHold'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['Drop']);
                    }
                }
            }

            // On Hold
            if (project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if(project.hasStatus(['draft', 'onProgress'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['On Hold']);
                    }
                }
            }            

            // Un-Hold
            if (project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if (project.hasStatus(['onHold'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['Un-Hold']);
                    }
                }
            }

            // Create running changes
            if (project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if (project.hasStatus(['approved'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['Create running changes']);
                    }
                }
            }

            // Split by Countries
            if (project.get_isOpen()) {
                if (project.hasStatus(['draft', 'onProgress'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        project.get_countries(function (res) {
                            if (res.length > 0) {
                                buttons.add('Split by Countries');
                            }
                        });
                    }
                }
            }

            // Re-Open
            if(project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if (project.hasStatus(['approved','dropped'])) {
                    if (user.get_isSuperUser()) {
                        buttons.add(['Re-Open']);
                    }
                }
            }

            // Detach from Global
            if(project.get_isLocalWithParentAligned()) {
                if (project.hasStatus(['draft', 'onProgress'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['Detach from Global']);
                    }
                }
            }

            // Detach from Open
            if(project.get_isOpenBranch()) {
                if (project.hasStatus(['draft', 'onProgress'])) {
                    if (user.get_isSuperUser() || (user.canEdit(project) && project.get_isCurrentUserInTeam())) {
                        buttons.add(['Detach from Open']);
                    }
                }
            }

            // Request Confirmation
            if(project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if (project.hasStatus(['draft', 'onProgress'])) {
                    if (project.get_isCurrentUserPlanAdministrator()) {
                        buttons.add(['Request Confirmation']);
                    }
                }
            }

            // Stop Confirmation Request
            if(project.get_isOpen() || project.get_isGlobal() || project.get_isLocalWithParentNotAligned() || project.get_isLocalWithoutParent()) {
                if (project.hasStatus(['approvalRequest'])) {
                    if (project.get_isCurrentUserPlanAdministrator()) {
                        buttons.add(['Stop Confirmation Request']);
                    }
                }
            }

            // Join with Parent
            if (project.get_isLocalWithParentNotAligned()) {
                this.childProject = project;                
                Repository.Get('HandsetProject', project.get_globalProjectId() , function (parentProject) {
                    if(
                        (project.hasStatus(['draft']) && parentProject.hasStatus(['draft'])) ||
                        (project.hasStatus(['draft']) && parentProject.hasStatus(['onProgress'])) ||
                        (project.hasStatus(['onProgress']) && parentProject.hasStatus(['draft'])) ||
                        (project.hasStatus(['onProgress']) && parentProject.hasStatus(['onProgress']))) {
                        if (user.get_isSuperUser() || 
                            (user.canEdit(project) && project.get_isCurrentUserInTeam() && 
                                user.canEdit(parentProject) && parentProject.get_isCurrentUserInTeam())) {
                                buttons.add(['Join with Parent']);
                        }
                    }
                }.bind(this));
            }
        }
        // #endregion
    },
    // #endregion

    controls: [
    // #region Project Panel
        {
        id: 'projectPanel',
        type: 'panel',
        orientation: 'horizontal',
        cssClass: 'top_block',
        height: '250',
        bindings: {
            'data:project': 'team'
        },
        controls: [
        // #region Project Info
                {
                id: 'project',
                margin: '0 0 3 0',
                type: 'collapsablePanel',
                title: 'Project',
                minWidth: 350,
                showCollapseButton: false,
                orientation: 'horizontal',
                cssClass: 'project_target_panel',
                customFunctions: {
                    '__updateButtonsList': function (sender, args) {
                        this.get_window().updateButtonsList();
                    }
                },
                onFree: function () {
                    var project = this.innerPanel.leftPanel.get_dataSource();

                    // detach handlers on free
                    if (project) {
                        project.remove_statusChanged(this.__updateButtonsList, this);

                        if (project instanceof LocalProject) {
                            project.remove_isAlignedChanged(this.__updateButtonsList, this);
                        }
                    }
                },
                bindings: {
                    'data:handset': function (sender, args) {
                        var value = args.newValue;
                        this.innerPanel.leftPanel.image.set_dataSource(value);
                    },
                    'data:project': function (sender, args) {
                        var newValue = args.newValue;
                        var oldValue = args.oldValue;

                        // detach handlers from old project
                        if (oldValue) {
                            oldValue.remove_statusChanged(this.__updateButtonsList, this);

                            if (oldValue instanceof LocalProject) {
                                oldValue.remove_isAlignedChanged(this.__updateButtonsList, this);
                            }
                        }

                        // create list of buttons for a new project
                        if (newValue) {
                            // attach handlers to a new project
                            newValue.add_statusChanged(this.__updateButtonsList, this);

                            if (newValue instanceof LocalProject) {
                                newValue.add_isAlignedChanged(this.__updateButtonsList, this);
                            }

                            this.__updateButtonsList();
                            this.innerPanel.leftPanel.set_dataSource(newValue);
                        }
                    }
                },
                controls: [
                        {
                            id: 'leftPanel',
                            type: 'panel',
                            width: '120',
                            height: '150',
                            margin: '0 9 0 0',
                            valign: 'top',
                            customFunctions: {
                                '_labelsChanged': function (sender) {
                                    if (!sender) {
                                        return;
                                    }

                                    var currentUser = Application.get_currentUser();
                                    var labels = currentUser.get_labels();

                                    var ds = this.tagsSelector.get_items();

                                    if (ds != null) {
                                        ds.dispose();
                                    }

                                    ds = [].makeObservable();

                                    for (var i = 0; i < labels.length; i++) {
                                        ds.add({ label: labels[i], value: 0 });
                                    }

                                    var items = sender;

                                    for (var j = 0; j < ds.length; j++) {
                                        if (items.contains(ds[j].label)) {
                                            ds[j].value = 1;
                                        }
                                    }

                                    this.tagsSelector.set_items(ds);
                                },
                                '_userLabelChanged': function () {
                                    this._labelsChanged(this.get_dataSource().get_labels());
                                }
                            },
                            onLoad: function () {
                                Application.get_currentUser().get_labels().add_changed(this._userLabelChanged, this);
                            },
                            onFree: function () {
                                Application.get_currentUser().get_labels().remove_changed(this._userLabelChanged, this);
                            },
                            bindings: {
                                'labels': function (sender, args) {
                                    this._labelsChanged(args.newValue);
                                },
                                'labels.changed': function (sender, args) {
                                    this._labelsChanged(sender);
                                }
                            },
                            controls: [
                                {
                                    id: 'image',
                                    type: 'entityImage',
                                    width: '90',
                                    height: '120',
                                    mode: "bounded",
                                    cssClass: 'project_image',
                                    bindings: {
                                        'imageId': 'imageId'
                                    }
                                },
                                {
                                    id: 'tagsSelector',
                                    type: 'tagsSelector',
                                    halign: 'center',
                                    assignAvailable: true,
                                    onApply: function (sender, args) {
                                        var ds = this.get_items();
                                        var project = this.parent.get_dataSource();
                                        var parentDs = this.parent.get_dataSource().get_labels();
                                        var addedLabels = [];
                                        var removedLabels = [];

                                        for (var i = 0; i < ds.length; i++) {
                                            if (ds[i].value === 1 && !parentDs.contains(ds[i].label)) {
                                                addedLabels.add(ds[i].label);
                                            }

                                            if (ds[i].value === 0 && parentDs.contains(ds[i].label)) {
                                                removedLabels.add(ds[i].label);
                                            }
                                        }

                                        if (addedLabels.length > 0) {
                                            Services.ProjectService.AddLabels([project], addedLabels);
                                        }

                                        if (removedLabels.length > 0) {
                                            Services.ProjectService.RemoveLabels([project], removedLabels);
                                        }
                                    }
                                }
                            ]
                        },
                        {
                            id: 'infoContainer',
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    id: 'info',
                                    type: 'panel',
                                    padding: '5',
                                    height: '?',
                                    valign: 'middle',
                                    bindings: {
                                        'data:handset': function (sender, args) {
                                            var value = args.newValue;
                                            this.namePanel.name.set_dataSource(value);
                                        },
                                        'data:project': function (sender, args) {
                                            var value = args.newValue;
                                            this.projectInfo.set_dataSource(value);
                                            this.namePanel.favoriteStar.set_dataSource(value);

                                            if (value.get_globalProjectId && value.get_globalProjectId() > 0) {
                                                this.namePanel.parentLink.show();
                                                this.namePanel.parentLink.set_url('page:planning/projectDetails|projectId=' + value.get_globalProjectId());
                                            } else if (value.get_openProjectId && value.get_openProjectId() > 0) {
                                                this.namePanel.parentLink.show();
                                                this.namePanel.parentLink.set_url('page:planning/projectDetails|projectId=' + value.get_openProjectId());
                                            } else {
                                                this.namePanel.parentLink.hide();
                                            }

                                            if (value) {
                                                this.labelsList.set_dataSource(value.get_labels());
                                            }
                                        }
                                    },
                                    controls: [
                                        {
                                            type: 'panel',
                                            id: 'namePanel',
                                            cssClass: 'name_panel',
                                            orientation: 'horizontal',
                                            height: '20',
                                            margin: '0 0 0 7',
                                            controls: [
                                                {
                                                    id: 'name',
                                                    type: 'label',
                                                    text: '',
                                                    cssClass: 'handset_name',
                                                    height: '*', width: '?',
                                                    bindings: {
                                                        '*': function (sender, args) {
                                                            var value = args.newValue;
                                                            this.set_text(value.get_code() + '/' + value.get_name());
                                                        }
                                                    }
                                                },
                                                {
                                                    id: 'favoriteStar',
                                                    type: 'favoriteCheckbox',
                                                    valign: 'middle',
                                                    margin: '5 0 5 0'
                                                },
                                                { type: 'panel' },
                                                {
                                                    id: 'parentLink',
                                                    type: 'link',
                                                    text: 'Parent',
                                                    width: '40',
                                                    height: '16',
                                                    valign: 'middle'
                                                }
                                            ]
                                        },
                                        {
                                            id: 'labelsList',
                                            type: 'repeater',
                                            margin: '0 0 0 3',
                                            orientation: 'horizontal',
                                            layout: 'wrap',
                                            width: '*',
                                            height: '?',
                                            cssClass: 'labels_block',
                                            template: { type: 'userTag', margin: '0 0 1 0' }
                                        },
                                        {
                                            id: 'projectInfo',
                                            type: 'form',
                                            width: '100%',
                                            height: '?',
                                            labelWidth: '100',
                                            cssClass: 'project_details',
                                            bindings: {
                                                '*': function (sender, args) {
                                                    var value = args.newValue;
                                                    var key = value.get_tDDomain() + '/' + value.get_tDProject();
                                                    this.tdInfo.set_text(key == '/' ? '' : key);

                                                    var inheritanceId = value.get_inheritanceId();
                                                    var inheritanceType = value.get_inheritanceType();

                                                    this.template.set_activeView('noInheritance');
                                                    this.label_template.set_text('Template:');

                                                    if (inheritanceType == InheritanceType.project) {
                                                        this.label_template.set_text('Inherited from:');
                                                        Repository.GetOrDefault('HandsetProject', inheritanceId, function (project) {
                                                            if (project) {
                                                                this.template.set_activeView('link');
                                                                this.template.link.set_text(project.get_name());
                                                                this.template.link.set_url('page:planning/projectDetails|projectId=' + project.get_id());
                                                            }
                                                        } .bind(this));
                                                    }

                                                    if (inheritanceType == InheritanceType.template) {
                                                        Repository.GetOrDefault('ProjectTemplate', inheritanceId, function (template) {
                                                            if (template) {
                                                                this.template.set_activeView('label');
                                                                this.template.label.set_text(template.get_name());
                                                            }
                                                        } .bind(this));
                                                    }
                                                },
                                                'tDHandsetName': 'tdkey',
                                                'tDCommonBugsKey': 'tdCommonBugskey',
                                                'modelLine': 'projectType',
                                                'description': 'description',
                                                'status': 'status'
                                            },
                                            controls: [
                                                {
                                                    id: 'status',
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'Status:',
                                                    emptyText: 'Draft'
                                                },
                                                {
                                                    id: 'projectType',
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'Type:',
                                                    emptyText: 'unknown'
                                                },
                                                {                                                    
                                                    type: 'label',
                                                    id: 'CALevel',
                                                    height: '?',
                                                    'form.label': 'CA Level:',
                                                    emptyText: '-',                                                   
                                                    bindings: {
                                                        'data:project': function(sender, args) {
                                                            if (args.newValue.get_modelLine() != HandsetModelLine.countryAdaptation) {
                                                                this.parent.hideRow('CALevel');
                                                            }
                                                        },
                                                        'data:projectInfo': 'dataSource',
                                                        'cALevel': 'text'
                                                    }
                                                },
                                                {
                                                    id: 'description',
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'Description:',
                                                    emptyText: '-'
                                                },
                                                {
                                                    id: 'template',
                                                    type: 'multiView',
                                                    height: '16',
                                                    'form.label': 'Template',
                                                    views: [
                                                        { id: 'noInheritance', type: 'label', height: '16', text: 'unknown' },
                                                        {
                                                            id: 'label',
                                                            type: 'label',
                                                            height: '16'
                                                        },
                                                        {
                                                            id: 'link',
                                                            type: 'link',
                                                            height: '16'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'PR Number:',
                                                    emptyText: '-',
                                                    bindings: {
                                                        'data:projectInfo': 'dataSource',
                                                        'requestNumber': 'text'
                                                    }
                                                },
                                                {
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'Model Suffix:',
                                                    emptyText: '-',
                                                    bindings: {
                                                        'data:projectInfo': 'dataSource',
                                                        'modelSuffix': 'text'
                                                    }
                                                },
                                                {
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'Amount:',
                                                    emptyText: '-',
                                                    bindings: {
                                                        'data:projectInfo': 'dataSource',
                                                        'amount': 'text'
                                                    }
                                                },
                                                {
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'R&D:',
                                                    emptyText: '-',
                                                    bindings: {
                                                        'data:projectInfo': 'dataSource',
                                                        'departmentId': function(sender, args) {
                                                            if (args.newValue) {
                                                                Repository.Get('Department', args.newValue, function(department) {
                                                                    this.set_text(department.get_name());
                                                                }.bind(this));
                                                            } else {
                                                                this.set_text('-');
                                                            }
                                                        }
                                                    }
                                                },
                                                {
                                                    id: 'tdInfo',
                                                    type: 'label',
                                                    'form.label': 'TD Domain/Project:',
                                                    emptyText: '-'
                                                },
                                                {
                                                    id: 'tdCommonBugskey',
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'TD Common Bugs:',
                                                    emptyText: '-'
                                                },
                                                {
                                                    id: 'tdkey',
                                                    type: 'label',
                                                    height: '16',
                                                    'form.label': 'TD Local Bugs:',
                                                    emptyText: '-'
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
            },
        // #endregion
        // #region Market
                {
                type: 'multiView',
                margin: '0 0 3 0',
                views: [
                // #region Open Target
                        {
                        id: 'openTarget',
                        type: 'panel',
                        cssClass: 'open_target',
                        controls: [
                                {
                                    type: 'collapsablePanel',
                                    title: 'Countries',
                                    showCollapseButton: false,
                                    cookieKey: 'countriesPanel',
                                    margin: '0 0 0 3',
                                    width: '100%',
                                    scrolling: true,
                                    cssClass: 'countries_list',
                                    controls: [
                                        {
                                            id: 'countries',
                                            type: 'repeater',
                                            layout: 'wrap',
                                            padding: '5',
                                            orientation: 'vertical',
                                            width: '*',
                                            height: '100%-3',
                                            emptyDataText: 'No countries selected',
                                            template: {
                                                type: 'panel',
                                                width: '49%',
                                                height: '20',
                                                orientation: 'horizontal',
                                                cssClass: 'country_row',
                                                controls: [
                                                    {
                                                        id: 'image',
                                                        type: 'entityImage',
                                                        width: 25,
                                                        height: 15,
                                                        mode: 'exactly',
                                                        margin: '0 0 3 0'
                                                    },
                                                    {
                                                        id: 'countryName',
                                                        type: 'literal',
                                                        cssClass: 'countryName',
                                                        width: '*',
                                                        height: '*',
                                                        text: '',
                                                        dataType: 'html'
                                                    }
                                                ],
                                                bindings: {
                                                    '*': function (sender, args) {
                                                        var dataItem = args.newValue;

                                                        if (dataItem.project != '') {
                                                            var projectLink = '#planning/projectDetails|projectId=' + dataItem.project.get_id();

                                                            var setProjectName = function (name) {
                                                                this.countryName.set_text('<a href="' + projectLink + '">' + name + '</a>');
                                                            } .bind(this);

                                                            setProjectName(dataItem.country.get_name());
                                                            this.image.set_imageId(dataItem.country.get_imageId());
                                                        } else {
                                                            this.countryName.set_text('<span>' + dataItem.country.get_name() + '</span>');
                                                        }

                                                        this.image.set_imageId(dataItem.country.get_imageId());
                                                    }
                                                }
                                            },
                                            customFunctions: {
                                                '_alignCountries': function () {
                                                    var ds = this.get_dataSource();

                                                    if (ds && ds.length) {
                                                        var height = Math.round(ds.length / 2) * 20;
                                                        this.set_height(height + 10);
                                                        this.update();
                                                    }
                                                }
                                            },
                                            bindings: {
                                                '*': '_alignCountries',
                                                '*.changed': '_alignCountries'
                                            },
                                            onLoad: function () {
                                                this.set_dataSource(this.get_data().get_countries());
                                            }
                                        }
                                    ],
                                    onLoad: function () {
                                        this._changed = function () {
                                            var count = this.get_data().get_countries().length;

                                            if (count == 0) {
                                                this.hide(true);
                                            }
                                            else {
                                                this.show(true);
                                            }
                                        };

                                        this.get_data().get_countries().add_changed(this._changed, this);
                                    },
                                    onFree: function () {
                                        this.get_data().get_countries().remove_changed(this._changed, this);
                                    },
                                    onCommand: function (sender, args) {
                                        this.get_data().set_pageView('countries');
                                    }
                                },
                                {
                                    type: 'collapsablePanel',
                                    title: 'Operators',
                                    cookieKey: 'operatorsPanel',
                                    cssClass: 'operators_list',
                                    width: '100%',
                                    showCollapseButton: false,
                                    controls: [
                                        {
                                            type: 'scrollablePanel',
                                            controls: [
                                                {
                                                    type: 'operatorsTree',
                                                    width: '100%',
                                                    height: '?',
                                                    padding: '5',
                                                    onLoad: function () {
                                                        this.set_dataSource(this.get_data().get_operators());
                                                    }
                                                }
                                            ]
                                        }
                                    ],
                                    onLoad: function () {
                                        this._changed = function () {
                                            var count = this.get_data().get_operators().length;

                                            if (count == 0) {
                                                this.hide(true);
                                            }
                                            else {
                                                this.show(true);
                                            }
                                        };

                                        this.get_data().get_operators().add_changed(this._changed, this);
                                    },
                                    onFree: function () {
                                        this.get_data().get_operators().remove_changed(this._changed, this);
                                    },
                                    onCommand: function (sender, args) {
                                        this.parent.get_data().set_pageView('operators');
                                    }
                                }
                            ]
                    },
                // #endregion
                // #region Local Target
                        {
                        id: 'localTarget',
                        width: '100%',
                        height: '100%',
                        showStatusBar: true,
                        type: 'collapsablePanel',
                        title: 'Local Market',
                        cookieKey: 'localMarketPanel',
                        cssClass: 'local_target',
                        padding: '5',
                        showCollapseButton: false,
                        controls: [
                                {
                                    type: 'panel',
                                    width: '100%',
                                    height: '100%',
                                    layout: 'stack',
                                    orientation: 'horizontal',
                                    controls: [
                                        {
                                            type: 'label',
                                            text: 'Country:',
                                            width: '80'
                                        },
                                        {
                                            type: 'panel',
                                            cssClass: 'country_name',
                                            height: 22,
                                            width: '100%-80',
                                            orientation: 'horizontal',
                                            controls: [
                                                {
                                                    id: 'image',
                                                    type: 'entityImage',
                                                    mode: 'exactly',
                                                    width: 25,
                                                    height: 15,
                                                    margin: '0 0 5 0'
                                                },
                                                {
                                                    type: 'label',
                                                    text: 'Country is not selected',
                                                    width: '*',
                                                    height: '*',
                                                    valign: 'middle',
                                                    onLoad: function () {
                                                        if (!this.__changed) {
                                                            this.__changed = function (sender, args) {
                                                                if (sender.length == 1) {
                                                                    var country = sender[0].country;
                                                                    this.parent.image.set_imageId(country.get_imageId());
                                                                    this.set_text(country.get_name());
                                                                } else {
                                                                    this.parent.image.set_imageId(0);
                                                                    this.set_text('Country is not selected');
                                                                }
                                                            }
                                                        }

                                                        this.get_data().get_countries().add_changed(this.__changed, this);
                                                    },
                                                    onFree: function () {
                                                        if (this.__changed) {
                                                            this.get_data().get_countries().remove_changed(this.__changed);
                                                        }
                                                    }
                                                }
                                            ]
                                        },
                                        {
                                            type: 'label',
                                            text: 'Operator:',
                                            width: '80'
                                        },
                                        {
                                            type: 'panel',
                                            cssClass: 'country_name',
                                            height: 22,
                                            width: '100%-80',
                                            orientation: 'horizontal',
                                            controls: [
                                                {
                                                    id: 'image',
                                                    type: 'entityImage',
                                                    width: 25,
                                                    height: 15,
                                                    margin: '0 0 5 0'
                                                },
                                                {
                                                    type: 'label',
                                                    text: 'Operator is not selected',
                                                    width: '*',
                                                    height: '*',
                                                    valign: 'middle',
                                                    onLoad: function () {
                                                        if (!this.__changed) {
                                                            this.__changed = function (sender, args) {
                                                                if (sender.length == 1) {
                                                                    var operator = sender[0];

                                                                    if (operator instanceof SubsidiaryOperator) {
                                                                        Repository.Get('GlobalOperator', operator.get_operatorId(), function (result) {
                                                                            this.set_text(operator.get_name() + ' (' + result.get_name() + ')');
                                                                            this.parent.image.set_imageId(operator.get_imageId() || result.get_imageId());
                                                                        } .bind(this));
                                                                    } else {
                                                                        this.set_text(operator.get_name());
                                                                        this.parent.image.set_imageId(operator.get_imageId());
                                                                    }
                                                                } else {
                                                                    this.parent.image.set_imageId(0);
                                                                    this.set_text('Operator is not selected');
                                                                }
                                                            }
                                                        }

                                                        this.get_data().get_operators().add_changed(this.__changed, this);
                                                    },
                                                    onFree: function () {
                                                        if (this.__changed) {
                                                            this.get_data().get_operators().remove_changed(this.__changed);
                                                        }
                                                    }
                                                }
                                            ]
                                        }
                                    ]
                                }
                            ],
                        onCommand: function (sender, args) {
                            this.parent.get_data().set_pageView('localMarket');
                        }
                    },
                // #endregion
                // #region Global Target
                        {
                        id: 'globalTarget',
                        width: '100%',
                        height: '100%',
                        type: 'collapsablePanel',
                        title: 'Global Market',
                        padding: '5',
                        cookieKey: 'globalMarketPanel',
                        showStatusBar: true,
                        showCollapseButton: false,
                        statusText: '* Aligned Projects',
                        cssClass: 'global_target',
                        controls: [
                                {
                                    type: 'panel',
                                    cssClass: 'operator_name',
                                    border: '0 0 0 1',
                                    margin: '0 0 0 5',
                                    height: 22,
                                    orientation: 'horizontal',
                                    controls: [
                                        {
                                            id: 'image',
                                            type: 'entityImage',
                                            mode: 'bounded',
                                            width: 25,
                                            height: 18,
                                            margin: '0 0 3 0'
                                        },
                                        {
                                            type: 'label',
                                            text: 'Operator is not selected',
                                            width: '*',
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

                                                this.get_data().get_operators().add_changed(this.__changed, this);
                                            },
                                            onFree: function () {
                                                if (this.__changed) {
                                                    this.get_data().get_operators().remove_changed(this.__changed);
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
                                            width: '100%',
                                            emptyDataText: 'No countries selected',
                                            template: {
                                                type: 'panel',
                                                width: '49%',
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
                                                        type: 'literal',
                                                        cssClass: 'countryName',
                                                        text: '',
                                                        dataType: 'html'
                                                    },
                                                    {
                                                        id: 'isAligned',
                                                        cssClass: 'projectAlignment',
                                                        type: 'label',
                                                        text: '*',
                                                        margin: '5 0 0 0',
                                                        visible: false
                                                    }
                                                ],
                                                bindings: {
                                                    '*': function (sender, args) {
                                                        var dataItem = args.newValue;

                                                        var projectLink = '#planning/projectDetails|projectId=' + dataItem.project.get_id();

                                                        var setProjectName = function (name) {
                                                            this.countryName.set_text('<a href="' + projectLink + '">' + name + '</a>');
                                                        } .bind(this);

                                                        setProjectName(dataItem.country.get_name());
                                                        this.image.set_imageId(dataItem.country.get_imageId());

                                                        if (dataItem.project.get_isAligned()) {
                                                            this.isAligned.show();
                                                        }
                                                    }
                                                }
                                            },
                                            onLoad: function () {
                                                this.set_dataSource(this.get_data().get_countries());
                                            }
                                        }
                                    ]
                                }
                            ],
                        onCommand: function (sender, args) {
                            this.parent.get_data().set_pageView('globalMarket');
                        }
                    }
                // #endregion
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
        // #endregion
        // #region Team
                {
                id: 'team',
                type: 'collapsablePanel',
                title: 'Team',
                showCollapseButton: false,
                cssClass: 'project_team_block',
                margin: '0 0 3 0',
                padding: '0',
                statusLabelTemplate: {
                    type: 'label',
                    width: '*',
                    padding: '5 0'
                },
                showStatusBar: true,
                customFunctions: {
                    '_reloadButtonsList': function () {
                        var buttons = [],
                                project = this.get_data().get_project(),
                                currentUser = Application.get_currentUser(),
                                status = project.get_status();

                        if ((status == ProjectStatus.draft || status == ProjectStatus.onProgress) && currentUser.owns(project)) {
                            buttons.add('Edit approvers');
                        }

                        this.set_buttons(buttons);
                    }
                },
                bindings: {
                    'status': '_reloadButtonsList',
                    'deadlineDate, deadline': function () {
                        var currentUser = Application.get_currentUser();

                        var project = this.get_data().get_project(),
                                deadline = project.get_deadline(),
                                deadlineDate = project.get_deadlineDate(),
                                status = '';

                        currentUser.canApprove(project, function (result) {
                            if (result) {
                                if (deadline) {
                                    status = 'Approval Deadline: ' + deadline;
                                }

                                if (deadlineDate) {
                                    status += ' (' + deadlineDate.format('d') + ')';
                                }

                                this.set_statusText(status);
                            }
                        } .bind(this));
                    }
                },
                onCommand: function (sender, args) {
                    if (args.button == 'Edit approvers') {
                        this.get_window().editApprovers();
                    }
                },
                controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    id: 'teamTree',
                                    type: 'teamTree',
                                    padding: '5',
                                    height: '?',
                                    showCheckBoxes: false,
                                    showLinks: true,
                                    onClick: function (sender, args) {
                                        Repository.Get('User', args.item.get_userId(), function (result) {
                                            this.get_data().set_userPopupData(result);
                                        } .bind(this));
                                    },
                                    onLoad: function () {
                                        this.set_dataSource(this.get_window().get_data().get_team());
                                    }
                                }
                            ]
                        }
                    ]
            },
        // #endregion
        // #region Summary Status
                {
                type: 'collapsablePanel',
                title: 'Summary Status',
                showCollapseButton: false,
                cssClass: 'summary_status_block',
                scrolling: true,
                padding: '5',
                horizontal: true,
                showStatusBar: true,
                customFunctions: {
                    '_showEditButton': function () {
                        this.set_buttons(['Edit summary']);
                        this.showStatusBar();
                    },
                    '_hideEditButton': function () {
                        this.set_buttons([]);
                        this.hideStatusBar();
                    }
                },
                onCommand: function (sender, args) {
                    if (args.button == 'Edit summary') {
                        this.get_window().editProjectInfo();
                    }
                },
                bindings: {
                    'data:project': function (sender, args) {
                        var user = Application.get_currentUser(),
                                project = args.newValue;

                        if (user.canEdit(project)) {
                            if (user.get_isSuperUser()) {
                                this._showEditButton();
                            } else {
                                Services.UsersService.GetProjectsWithCurrentUser(function (result) {
                                    if (result.contains(project)) {
                                        this._showEditButton();
                                    } else {
                                        this._hideEditButton();
                                    }
                                } .bind(this));
                            }
                        }
                    }
                },
                controls: [
                        {
                            height: '?',
                            width: '100%-12',
                            type: 'form',
                            cssClass: 'summary_details',
                            labelWidth: 100,
                            minWidth: 175,
                            bindings: {
                                'sWQAStep': 'sWQAStep',
                                'sWVersion': 'sWVersion',
                                'tAReferencePlan': 'tAReferencePlan',
                                'tAReplan': 'tAReplan',
                                'kPITarget': 'kPITarget',
                                'gate2ProjectQualityIndex': 'pQIG2',
                                'tAProjectQualityIndex': 'pQITA',
                                'currentProjectQualityIndex': 'currentPQI',
                                'tADateDQMS': 'tADateDQMS',
                                'tADateDVPMS': 'tADateDVPMS',
                                'defects': 'defects',
                                'comments': 'comments',
                                'data:projectInfo': 'dataSource'
                            },
                            controls: [
                                { id: 'sWVersion', type: 'label', height: '16', 'form.label': 'SW Version:', emptyText: '-' },
                                { id: 'sWQAStep', type: 'label', height: '16', 'form.label': 'SW Q/A Step:', emptyText: '-' },
                                { id: 'tAReferencePlan', type: 'label', height: '16', 'form.label': 'TA Reference Plan:', emptyText: '-', format: '{0:d}' },
                                { id: 'tAReplan', type: 'label', height: '16', 'form.label': 'TA Replan:', emptyText: '-', format: '{0:d}' },
                                { id: 'kPITarget', type: 'label', height: '16', 'form.label': 'KPI Target:', emptyText: '-', format: '{0:d}' },
                                { id: 'pQIG2', type: 'label', height: '16', 'form.label': 'PQI-G2:', emptyText: '-' },
                                { id: 'pQITA', type: 'label', height: '16', 'form.label': 'PQI-TA:', emptyText: '-' },
                                { id: 'currentPQI', type: 'label', height: '16', 'form.label': 'Current PQI:', emptyText: '-' },
                                { id: 'tADateDQMS', type: 'label', height: '16', 'form.label': 'TA Date (DQMS):', emptyText: '-', format: '{0:d}' },
                                { id: 'tADateDVPMS', type: 'label', height: '16', 'form.label': 'TA Date (DVPMS):', emptyText: '-', format: '{0:d}' },
                                { id: 'defects', type: 'label', height: '16', 'form.label': 'Defects:', emptyText: '-' },
                                { id: 'comments', type: 'label', height: '?', 'form.label': 'Comments:', emptyText: '-' }
                            ]
                        }
                    ]
            }
        // #endregion
            ]
    },
    // #endregion

    // #region Buttons list
    {
        id: 'buttonsList',
        type: 'repeater',
        orientation: 'horizontal',
        width: '*',
        height: '?',
        padding: '0 2 0 0',
        cssClass: 'toolbar_panel',
        border: '1',
        margin: '0 3',
        bindings: {
            '*': function (sender, args) {
                if (args.newValue.length == 0) {
                    this.hide();
                } else {
                    this.show();
                }
            },
            '*.changed': function (sender, args) {
                if (this.get_dataSource().length == 0) {
                    this.hide();
                } else {
                    this.show();
                }
            }
        },
        customFunctions: {
            'buttonClicked': function (text) {
                var controller = this.get_window();

                var map = {
                    'Edit': controller.editProject,
                    'DQMS Approval Received': controller.dqmsApproval,
                    'Drop': controller.dropProject,
                    'On Hold': controller.onHold,
                    'Un-Hold': controller.unHold,
                    'Create running changes': controller.createRunningChanges,
                    'Split by Countries': controller.splitByCountries,
                    'Re-Open': controller.reOpen,
                    'Detach from Global': controller.detachFromGlobal,
                    'Detach from Open': controller.detachFromOpen,
                    'Request Confirmation': controller.approvalRequest,
                    'Stop Confirmation Request': controller.stopApprovalRequest,
                    'Join with Parent': controller.joinWithParent
                };

                if (!map[text]) {
                    throw new Error('Unknown action');
                }

                map[text]();
            }
        },
        template: {
            type: 'button',
            onClick: function () {
                this.parent.buttonClicked(this.get_text());
            }
        }
    },
    // #endregion

    // #region Collapser
        {
        id: 'collapser',
        type: 'link',
        cssClass: 'page_top_collapser',
        text: 'collapse',
        width: '*',
        height: '5',
        onClick: function () {
            if (!this.__collapsed) {
                this.parent.projectPanel.hide(true);
                this.addCssClass('page_top_collapser_collapsed');
                this.__collapsed = true;
            } else {
                this.parent.projectPanel.show(true);
                this.removeCssClass('page_top_collapser_collapsed');
                this.__collapsed = false;
            }
        }
    },
    // #endregion

    // #region Bottom side bar
        {
        type: 'tabPanel',
        tabs: [
        // #region Planning
                {
                title: "Planning",
                bindings: {
                    'data:project': function (sender, args) {
                        this.silverlight.set_dataSource(args.newValue);
                        this.silverlight.reload();
                    }
                },
                controls: [
                        {
                            id: 'silverlight',
                            type: 'silverlight',
                            url: 'silverlight:SilverlightAdvGrid.xap',
                            width: '100%',
                            height: '100%',
                            bindings: {
                                'status': function (sender, args) {
                                    if (args.oldValue !== undefined)
                                        this.reload();
                                }
                            },
                            onSlPreLoad: function (sender, args) {
                                var userId = Application.get_context().get('currentUser').get_id();
                                var projectId = this.parent.parent.parent.get_param('projectId') * 1;

                                args.params = 'Mode=4,UserId=' + userId + ',SLControlID=sl,ProjectsIds=' + projectId;
                            }
                        }
                    ]
            },
        // #endregion
        // #region Defects statistics
                {
                title: 'Defects statistics',
                onActivate: function () {
                    var projectId = this.get_window().get_param('projectId') * 1;

                    Services.GoogleChartService.GetStatistics(projectId,
                            function (result) {
                                if (result) {
                                    this.statisticsPanel.show();
                                    this.statisticsPanel.set_dataSource(result);
                                } else {
                                    this.statisticsPanel.hide();
                                    this.notAvailableLabel.show();
                                }
                            } .bind(this)
                        );
                },
                controls: [
                        {
                            id: 'statisticsPanel',
                            type: 'panel',
                            visible: false,
                            padding: '10',
                            orientation: 'horizontal',
                            bindings: {
                                'GoogleChartUrl': 'chartImage',
                                '*': function (sender, args) {
                                    var stat = args.newValue;
                                    var groups = stat.Groups;
                                    this.info.set_dataSource(groups.makeObservable());
                                    this.info.total.set_dataSource(stat);
                                }
                            },
                            controls: [
                                {
                                    type: 'panel',
                                    id: 'info',
                                    width: '400',
                                    height: '?',
                                    orientation: 'vertical',
                                    bindings: {
                                        '*': 'info'
                                    },
                                    controls: [
                                        {
                                            type: 'panel',
                                            orientation: 'horizontal',
                                            height: '?',
                                            width: '?',
                                            margin: '0 10 0 10',
                                            controls: [
                                                {
                                                    type: 'panel',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            width: '?',
                                                            text: 'Group',
                                                            border: '0 0 0 1'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'panel',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            width: '?',
                                                            text: 'High',
                                                            border: '0 0 0 1',
                                                            halign: 'center'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'panel',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            width: '?',
                                                            text: 'Medium',
                                                            border: '0 0 0 1',
                                                            halign: 'center'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'panel',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            width: '?',
                                                            text: 'Low',
                                                            border: '0 0 0 1',
                                                            halign: 'center'
                                                        }
                                                    ]
                                                }
                                            ]
                                        },
                                        {
                                            type: 'scrollablePanel',
                                            id: 'info',
                                            height: '?',
                                            maxHeight: '200',
                                            width: '380',
                                            border: '0 0 0 1',
                                            bindings: { '*': 'info' },
                                            controls: [
                                                {
                                                    id: 'info',
                                                    width: '?',
                                                    type: 'repeater',
                                                    layout: 'stack',
                                                    height: '?',
                                                    orientation: 'vertical',
                                                    template: {
                                                        type: 'panel',
                                                        orientation: 'horizontal',
                                                        height: '?',
                                                        width: '?',
                                                        bindings: {
                                                            '*': function (sender, args) {
                                                                var data = args.newValue;
                                                                var openSum = data.OpenDefects[0].Value + data.OpenDefects[1].Value + data.OpenDefects[2].Value;
                                                                var totalSum = data.TotalDefects[0].Value + data.TotalDefects[1].Value + data.TotalDefects[2].Value;

                                                                this.groupName.set_text(data.GroupName);

                                                                this.highDefects.label.set_text(data.OpenDefects[0].Value + '/' + data.TotalDefects[0].Value);
                                                                this.mediumDefects.label.set_text(data.OpenDefects[1].Value + '/' + data.TotalDefects[1].Value);
                                                                this.lowDefects.label.set_text(data.OpenDefects[2].Value + '/' + data.TotalDefects[2].Value);
                                                                //this.totalDefects.set_text(openSum + '/' + totalSum);
                                                            }
                                                        },
                                                        controls: [
                                                            {
                                                                type: 'label',
                                                                id: 'groupName',
                                                                width: '90'
                                                            },
                                                            {
                                                                id: 'highDefects',
                                                                type: 'panel',
                                                                width: '90',
                                                                height: '?',
                                                                controls: [
                                                                    {
                                                                        id: 'label',
                                                                        type: 'label',
                                                                        width: '?',
                                                                        halign: 'center'
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                id: 'mediumDefects',
                                                                type: 'panel',
                                                                width: '90',
                                                                height: '?',
                                                                controls: [
                                                                    {
                                                                        id: 'label',
                                                                        type: 'label',
                                                                        width: '?',
                                                                        halign: 'center'
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                id: 'lowDefects',
                                                                type: 'panel',
                                                                width: '90',
                                                                height: '?',
                                                                controls: [
                                                                    {
                                                                        id: 'label',
                                                                        type: 'label',
                                                                        width: '?',
                                                                        halign: 'center'
                                                                    }
                                                                ]
                                                            }
                                                        ]
                                                    }
                                                }
                                            ]
                                        },
                                        {
                                            id: 'total',
                                            bindings: {
                                                '*': function (sender, args) {
                                                    var data = args.newValue;
                                                    var groups = data.Groups.makeObservable();

                                                    var openHigh = groups.sum(function (g) { return g.OpenDefects[0].Value; });
                                                    var totalHigh = groups.sum(function (g) { return g.TotalDefects[0].Value; })

                                                    var openMedium = groups.sum(function (g) { return g.OpenDefects[1].Value; });
                                                    var totalMedium = groups.sum(function (g) { return g.TotalDefects[1].Value; })

                                                    var openLow = groups.sum(function (g) { return g.OpenDefects[2].Value; });
                                                    var totalLow = groups.sum(function (g) { return g.TotalDefects[2].Value; })

                                                    this.highDefects.label.set_text(openHigh + '/' + totalHigh);
                                                    this.mediumDefects.label.set_text(openMedium + '/' + totalMedium);
                                                    this.lowDefects.label.set_text(openLow + '/' + totalLow);
                                                }
                                            },
                                            type: 'panel',
                                            orientation: 'horizontal',
                                            height: '30',
                                            width: '380',
                                            margin: '0 10 0 10',
                                            controls: [
                                                {
                                                    type: 'panel',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            width: '?',
                                                            text: 'Total'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'panel',
                                                    id: 'highDefects',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            id: 'label',
                                                            width: '?',
                                                            halign: 'center'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'panel',
                                                    id: 'mediumDefects',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            id: 'label',
                                                            width: '?',
                                                            halign: 'center'
                                                        }
                                                    ]
                                                },
                                                {
                                                    type: 'panel',
                                                    id: 'lowDefects',
                                                    width: '90',
                                                    height: '?',
                                                    controls: [
                                                        {
                                                            type: 'label',
                                                            id: 'label',
                                                            width: '?',
                                                            halign: 'center'
                                                        }
                                                    ]
                                                }
                                            ]
                                        }
                                    ]
                                }
                                ,
                                {
                                    id: 'chartImage',
                                    type: 'image',
                                    width: '800',
                                    height: '200',
                                    border: '1',
                                    margin: '5 0 0 0'
                                }
                            ]
                        },
                        {
                            id: 'notAvailableLabel',
                            type: 'label',
                            padding: '5',
                            text: 'Statistics are not available for this project',
                            visible: false
                        }
                    ]
            }
        // #endregion
            ]
    },
    // #endregion

    // #region Running Changes Popup
        {
        id: 'runningChangesPopup',
        type: 'popup',
        title: 'Running changes',
        width: '90%',
        height: '90%',
        onOpen: function (sender, args) {
            this.page.add_onMessage(function (sender, args) {
                if (args.message == 'RunningChangeCreated') {
                    this.close();
                }
            }, this);
        }
    },
    // #endregion

    // #region Detach From Global Popup
    {
        id: 'detachFromGlobalPopup',
        type: 'popup',
        title: 'Detach from global',
        width: 640,
        height: 480,
        buttons: ['Yes', 'No'],
        onCommand: function (sender, args) {
            if (args.button == 'Yes') {
                this.page.processDetach(function(res) {
                    Application.reloadPage();
                }.bind(this));
            }

            this.close();
        }
    },
    // #endregion

    // #region Split By Countries Popup
        {
        id: 'splitByCountriesPopup',
        type: 'popup',
        title: 'Split by Countries',
        width: '600px',
        height: '90%',
        buttons: [
                'Split',
                'Close'
            ],
        onCommand: function (sender, args) {
            if (args.button == 'Close') {
                this.close();
            }

            if (args.button == 'Split') {
                this.page.save();
                this.close();
            }
        }
    },
    // #endregion

    // #region Detach From Open Popup
        {
        id: 'detachFromOpenPopup',
        type: 'popup',
        title: 'Detach from open',
        width: '350px',
        height: '155px',
        buttons: [
                'OK',
                'Cancel'
            ],
        onCommand: function (sender, args) {
            if (args.button == 'OK') {
                this.page.save();
            }

            this.close();
        }
    },
    // #endregion

    // #region User Details Popup
        {
        type: 'popup',
        width: '90%',
        height: '90%',
        title: '',
        buttons: ["OK"],
        cssClass: 'popup team_selector_user_popup',
        controls: [
                {
                    type: 'userBusinessCard',
                    id: 'profileDetails',
                    readOnly: true
                }
            ],
        bindings: {
            'data:userPopupData': function (sender, args) {
                if (args.newValue) {
                    this.set_title(args.newValue.get_name());
                    this.container.profileDetails.set_dataSource(args.newValue);
                    this.open();
                } else {
                    this.close();
                }
            }
        },
        onCommand: function () {
            this.parent.get_data().set_userPopupData(null);
        }
    },
    // #endregion

    // #region Edit Project Summary Popup
        {
        id: 'editSummaryPopup',
        type: 'popup',
        width: '400',
        height: '?',
        title: 'Edit Project Summary',
        buttons: ['Save', 'Cancel'],
        controls: [
                {
                    id: 'projectSummaryEdit',
                    height: '?',
                    padding: '5',
                    type: 'projectSummaryEdit'
                }
            ],
        onOpen: function () {
            this.container.projectSummaryEdit.set_dataSource(this.get_data().get_projectInfo());
        },
        onCommand: function (sender, args) {
            var entity = this.get_data().get_projectInfo();

            if (args.button == 'Close' || args.button == 'Cancel' && !entity.isNew())
                Repository.Reload(entity);

            if (args.button == 'Save') {
                Repository.Save(entity);
            }

            this.close();
        }
    },
    // #endregion

        // #region Approvers Edit Popup
        {
            type: 'popup',
            id: 'approversEditPopup',
            title: 'Edit approvers',
            width: '300',
            height: '300',
            bindings: {
                '*': function (sender, args) {
                    this.innerBox.innerPanel.popupContainer.edit.set_dataSource(args.newValue);
                }
            },
            buttons: ['OK', 'Cancel'],
            onCommand: function (sender, args) {
                if (args.button == 'OK') {
                    var roles = this.get_data().get_project().get_roles();
                    for (var i = 0; i < roles.length; i++) {
                        Repository.Save(roles[i]);
                    }
                }

                this.close();
            },
            controls: [
                {
                    type: 'projectApproversEdit',
                    id: 'edit',
                    margin: '10'
                }
            ]
        },
        // #endregion

        // #region Edit local project popup
        {
            type: 'popup',
            id: 'editLocalProjectPopup',
            title: 'Edit local project',
            width: '300',
            height: '?',
            buttons: ['Save', 'Cancel'],
            onCommand: function (sender, args) {
                var project = this.get_data().get_project(),
                    projectInfo = this.get_data().get_projectInfo();

                if (args.button == 'Save') {
                    var ds = this.page.get_data();
                    projectInfo.set_departmentId(ds.get_departmentId());
                    project.set_description(ds.get_description());

                    var editLocalProjectCommand = new EditLocalProjectCommandContract(
                        project.get_id(),
                        projectInfo.get_departmentId(),
                        project.get_description()
                    );

                    Services.ProjectService.EditLocalProject(editLocalProjectCommand, null, function(err) {
                        Application.throwError(err);
                        Repository.Reload(project);
                    });
                }

                this.close();
            },
            onUriLoaded: function() {
                var project = this.get_data().get_project();
                var projectInfo = this.get_data().get_projectInfo();

                var pageData = this.page.get_data();
                pageData.set_description(project.get_description());
                pageData.set_departmentId(projectInfo.get_departmentId());
            }
        }
        // #endregion
    ]
})