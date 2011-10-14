({
    title: "Manage Plans",
    layout: 'stack',

    data: [
        { name: 'selectedProjects', value: [].makeObservable() },
        { name: 'filteredByFavorites', value: false, saveHistory: true },
        { name: 'projects', value: [].makeDataSource('HandsetProjectViewModel')
              .orderBy('name asc').filterBy('').paginate(15).simple()
        },
        { name: "tags", value: [].makeObservable(), saveHistory: true },
        { name: "selectedFilterId" },
        { name: 'currentComplexFilter', value: '' },
        { name: 'displayLocals', value: false, saveHistory: true }
    ],

    bindings: {
        '*': function (sender, args) {
            var value = args.newValue;
            this.gridPanel.set_dataSource(value);
            this.pager.set_dataSource(value);
        },
        'data:tags, data:tags.changed': function(sender, args) {
            var tags = this.get_data().get_tags();
            var projects = this.get_data().get_projects(),
                customFilter = projects.get_filter().get_customFilters();
            
            if (tags.length > 0) {
                customFilter.set('filterByTags', tags.join());
            } else {
                customFilter.remove('filterByTags');
            }

            this.updateProjects();
        },
        'data:filteredByFavorites': function (sender, args) {
            var filter = this.get_data().get_projects().get_filter();

            if (args.newValue === true || args.newValue === 'true') {
                var currentUser = Application.get_currentUser();
                var favorites = currentUser.get_favoriteProjects().select('id').join();

                filter.set_query(favorites ? ('Project.Id IN (' + favorites + ')') : 'Id = 0');
            } else {
                filter.set_query('');
            }
            
            this.updateProjects();
        },
        'data:selectedFilterId': function (sender, args) {
            var data = this.get_data();
            
            if (!args.newValue || args.newValue == '') {
                if (args.oldValue) {
                    data.get_projects().set_filterString('');
                    data.set_currentComplexFilter('');            
                }
            }
            else {
                data.get_projects().set_filterString('complexFilter(' + args.newValue + ');');
                data.set_currentComplexFilter('complexFilter(' + args.newValue + ');');            
            }

            this._updateLocalsFilter();            
            
            data.get_projects().load(function () {
                this.get_data().get_selectedProjects().clear();                        
            } .bind(this));
        },
        'data:displayLocals': function (sender, args) {    
            this._updateLocalsFilter();
            this.get_data().get_projects().load();
            this.toolbar.container.displayLocalsCheckbox.set_state(Boolean.parse(args.newValue));
        }
    },    

    customFunctions: {
        'updateProjects': function() {
            this.get_data().get_projects().load(function () {
                this.get_data().get_selectedProjects().clear();                        
            } .bind(this));
        },

        '_updateLocalsFilter': function() {
            var data = this.get_data();
            var projects = data.get_projects();
                        
            var displayLocals = Boolean.parse(data.get_displayLocals());
            var customFilters = projects.get_filter().get_customFilters();

            if (displayLocals) {
                customFilters.remove("hideRelatedLocals");
            } else {   
                customFilters.set("hideRelatedLocals", []);
            }
        },

        '_applyLabelsForProjects': function (labels) {
            var projects = this.get_data().get_selectedProjects();
            var removedLabels = [];
            var addedLabels = [];

            for (var i = 0; i < projects.length; i++) {
                var project = projects[i];

                for (var j = 0; j < labels.length; j++) {
                    var item = labels[j];

                    if (item.value == 0) {
                        removedLabels.add(item.label);
                    }

                    if (item.value == 1) {
                        addedLabels.add(item.label);
                    }
                }
            }

            if (addedLabels.length > 0) {
                Services.ProjectService.AddLabels(projects, addedLabels);
            }

            if (removedLabels.length > 0) {
                Services.ProjectService.RemoveLabels(projects, removedLabels);
            }
        }
    },

    onLoad: function () {
        var ds = this.get_data().get_projects();
        this._updateLocalsFilter();

        this.set_dataSource(ds);
        ds.load();
    },

    onFree: function () {
        this.get_data().get_projects().dispose();
    },

    controls: [
        {
            type: "pageHeader",
            title: "Manage Plans",
            description: "Manage Plans page"
        },
        {
            type: 'gridToolbar',
            id: 'toolbar',
            controls: [
                "delete", "view", "edit", "favorites",
                {
                    type: 'advancedFilter',
                    bindings: {
                        'history:filter': 'selectedFilterId'
                    },
                    handlers: {
                        'selectedFilterIdChanged': function (sender, args) {
                            var projects = this.get_data().get_projects();
                            var data = this.get_data();

                            if (args.newValue === 'none') {
                                data.set_selectedFilterId('');
                            } else {
                                data.set_selectedFilterId(args.newValue);
                            }
                        }
                    }
                },           
                {
                    id: 'projectDeletingConflictPopup',
                    type: 'popup',
                    title: 'Can\'t delete all projects',
                    width: '400',
                    height: '300',
                    buttons: ['Yes', 'Cancel'],
                    onCommand: function (sender, args) {
                        if (args.button == 'Yes') {
                            this.parent.parent.performDeleting();
                            this.close();
                        }

                        if (args.button == 'Cancel') {
                            this.close();
                        }
                    },
                    bindings: {
                        '*': function (sender, args) {
                            var value = args.newValue;
                            this.container.projectDeletingConflict.set_dataSource(value);
                        }
                    },
                    controls: [
                        {
                            id: 'projectDeletingConflict',
                            type: 'projectDeletingConflict',
                            width: '100%',
                            height: '100%'
                        }
                    ]
                },
                {
                    type: 'tagsSelector',
                    width: '100px',
                    displayAvailable: true,
                    customFunctions: {
                        '_selectedProjectsChanged': function (sender) {
                            var currentUser = Application.get_currentUser();
                            var labels = currentUser.get_labels();

                            if (this.get_items() != null) {
                                this.get_items().dispose();
                            }

                            var ds = [].makeObservable();
                            ds.clear();

                            for (var i = 0; i < labels.length; i++) {
                                ds.add({ label: labels[i], value: 0 });
                            }

                            if (sender.length > 0) {
                                for (var i = 0; i < sender.length; i++) {
                                    var items = sender[i].get_labels();

                                    for (var j = 0; j < ds.length; j++) {
                                        if (items.contains(ds[j].label)) {
                                            ds[j].value++;
                                        }
                                    }
                                }

                                for (var i = 0; i < ds.length; i++) {
                                    if (ds[i].value == sender.length) {
                                        ds[i].value = 1;
                                    } else if (ds[i].value > 0) {
                                        ds[i].value = 2;
                                    }
                                }
                            }

                            this.set_items(ds);
                        }
                    },
                    bindings: {
                        'data:tags, data:tags.changed': function() {
                            var tags = this.get_data().get_tags(),
                                actionsBar = this.itemsList.get_actionsBar();
                            
                            if (actionsBar.contains('Show All') && tags.length == 0) {
                                actionsBar.remove('Show All');
                            } else if (!actionsBar.contains('Show All') && tags.length > 0) {
                                actionsBar.add('Show All');
                            }
                        },
                        'data:selectedProjects': function (newValue, oldValue) {
                            this._selectedProjectsChanged(newValue);
                        },
                        'data:selectedProjects.changed': function (sender, args) {
                            this._selectedProjectsChanged(sender);
                        }
                    },
                    onTagClick: function (sender, args) {
                        this.get_window()._applyLabelsForProjects([args.item]);
                    },
                    onDisplay: function(sender, args) {
                        var projects = this.get_data().get_projects(),
                            customFilter = projects.get_filter().get_customFilters();

                        var labelIds = [];

                        for (var i = 0; i < args.selected.length; i++) {
                            labelIds.add(args.selected[i].label.get_id());
                        }

                        if (labelIds.length > 0) {
                            this.get_data().get_tags().synchronize(labelIds);
                        } else {
                            this.get_data().get_tags().clear();
                        }
                    },
                    onCommand: function(sender, args) {
                        var projects = this.get_data().get_projects(),
                            customFilter = projects.get_filter().get_customFilters();

                        if (args.command == 'Show All') {
                            this.get_data().get_tags().clear();
                        }
                    },
                    onOpen: function() {
                        var projects = this.get_data().get_selectedProjects();
                        this._selectedProjectsChanged(projects);
                        this.set_isAssignAvailable(projects.length > 0);
                    },
                    onApply: function () {
                        this.get_window()._applyLabelsForProjects(this.get_items());
                        this.get_data().get_selectedProjects().clear();                        
                    }
                },
                {
                    type: 'checkbox',
                    height: '?',
                    valign: 'middle',
                    id: 'displayLocalsCheckbox',
                    text: 'Display child projects',

                    onChanged: function() {
                        var data = this.get_data();
                        data.set_displayLocals(this.get_state() == CheckBoxStates.checked);                   
                    }
                }
            ],
            customFunctions: {
                '_selectedProjectsChanged': function (sender, args) {
                    var currentUser = Application.get_currentUser();
                    var btnEnabled = sender.length > 0;

                    var buttons = [
                        this.container['delete'],
                        this.container['view'],
                        this.container['edit']
                    ];

                    for (var i = 0; i < buttons.length; i++) {
                        buttons[i].set_enabled(btnEnabled);
                        buttons[i].set_tooltip(btnEnabled ? '' : 'Please select one or more projects');
                    }

                    for (var i = 0; i < sender.length; i++) {
                        if (!currentUser.canEdit(sender[i])) {
                            this.container['edit'].set_enabled(false);
                        }
                    }
                },

                'performDeleting': function () {
                    var projects = this.get_data().get_selectedProjects();
                    var deleteTimeout;

                    var reload = function () {
                        this.get_data().get_selectedProjects().clear();
                        this.get_data().get_projects().load();
                    } .bind(this);

                    var currentUser = Application.get_currentUser();

                    for (var i = 0; i < projects.length; i++) {
                        var project = projects[i];

                        if (currentUser.canDelete(project)) {
                            var contract = { projectId : project.get_id() };
                            Services.ProjectService.DeleteProject(contract, function () {
                                if (deleteTimeout) {
                                    clearTimeout(deleteTimeout);
                                }
                                deleteTimeout = setTimeout(reload, 0);
                            });
                        }
                    }
                }
            },
            onCommand: function (sender, args) {
                var mode = null;

                if (args.button == 'delete') {

                    var currentUser = Application.get_currentUser();
                    var projectsToDelete = this.get_data().get_selectedProjects();
                    var projectsCantDelete = [].makeObservable();

                    for (var i = 0; i < projectsToDelete.length; i++) {
                        if (!currentUser.canDelete(projectsToDelete[i])) {
                            projectsCantDelete.add(projectsToDelete[i]);
                        }
                    }

                    if (projectsToDelete.length == projectsCantDelete.length) {
                        Application.showError('Error', 'You don\'t have permission to delete ' +
                            (projectsToDelete.length > 1 ? 'any of these projects' : 'this project'));
                    } else if (projectsCantDelete.length > 0) {
                        this.container.projectDeletingConflictPopup.set_dataSource(projectsCantDelete);
                        this.container.projectDeletingConflictPopup.open();
                    }
                    else {
                        Application.showConfirm('Warning', 'Do you really want to delete these projects?', function (result) {
                            if (result) {
                                this.performDeleting();
                            }
                        } .bind(this));
                    }
                    return;
                }                

                if (args.button == 'favorites') {
                    var data = this.get_data();

                    data.set_filteredByFavorites(!data.get_filteredByFavorites());
                    return;
                }

                if (args.button == 'edit') {
                    mode = 'edit';
                }

                if (args.button == 'view') {
                    mode = 'view';
                }

                if (mode) {
                    var projects = this.get_data().get_selectedProjects().select('id').join(';');
                    Application.loadPage('planning/viewEditProjects|mode=' + mode + '|projects=' + projects);
                }
            },
            onLoad: function () {

                var buttons = [
                    this.container['delete'],
                    this.container['view'],
                    this.container['edit']
                ];

                for (var i = 0; i < buttons.length; i++) {
                    buttons[i].set_enabled(false);
                    buttons[i].set_tooltip('Please select one or more projects');
                }

                if (!Application.get_currentUser().can("Project", PermissionLevel.create)) {
                    this.container['delete'].hide();
                }

                if (!Application.get_currentUser().can("Project", PermissionLevel.update)) {
                    this.container['edit'].hide();
                }

                this.get_data().get_selectedProjects().add_changed(this._selectedProjectsChanged, this)
            }
        },
        {
            type: 'scrollablePanel',
            id: 'gridPanel',
            vertical: true,
            horizontal: true,
            bindings: {
                '*': 'gridPanel'
            },
            controls: [
                {
                    id: 'gridPanel',
                    type: 'projectGrid',
                    width: '*',
                    height: '?',
                    onLoad: function () {
                        this.set_selectedProjects(this.get_data().get_selectedProjects());
                    }
                }
            ]
        },
        {
            id: 'pager',
            type: 'pager',
            bindings: {
                'history:page': 'page'
            }
        },
        {
            type: 'searchPane',
            bindings: {
                'history:searchQuery': 'query'
            },
            onSearch: function (sender, args) {
                var filterQuery = args.query || '';
                this.get_data().get_projects().set_searchQuery(filterQuery, true);
            }
        }
    ]
})