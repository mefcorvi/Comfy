({
    title: "Projects Summary",
    layout: 'stack',
    orientation: 'vertical',
    valign: 'stretch',

    cssClass: 'projects_status_page',

    data: [
        'selectedProject',
        { name: 'projects', value: [].makeDataSource('HandsetProjectInfo').filterBy('').paginate(13).simple() },
        'userProjects',
        { name: 'G4FilterEnabled', value: true, saveHistory: true, type: 'Boolean' },
        { name: 'G4SelectedDate', value: (function () { var d = new Date(); d.setDate(d.getDate() + 14 - d.getDay()); return d; })(), saveHistory: true },
        { name: 'hiddenColumns', value: [].makeObservable() },
        { name: 'displayLocals', value: false, saveHistory: true }
    ],

    onLoad: function () {
        var hiddenColumns = this.get_data().get_hiddenColumns();

        hiddenColumns.add_changed(function (sender, args) {
            $.cookie('projectSummary_grid_hidden_columns', hiddenColumns.join(','), { expires: 365 });
        }, this);

        var cookieState = $.cookie('projectSummary_grid_hidden_columns');

        if (cookieState) {
            hiddenColumns.add(cookieState.split(','));
        } else {
            hiddenColumns.add(['requestNumber', 'modelSuffix', 'amount',
                'sWQAStep', 'tADateDVPMS',
                'gate2ProjectQualityIndex', 'tAProjectQualityIndex', 'modelLeader']);
        }

        var data = this.get_data();
        var ds = data.get_projects();
        this.set_dataSource(ds);

        Services.UsersService.GetProjectsWithCurrentUser(function (result) {
            this.get_data().set_userProjects(result);
            this.assembleAndApplyFilter();
        } .bind(this));
    },

    bindings: {
        '*': function (sender, args) {
            var value = args.newValue;
            this.scrollPanel.set_dataSource(value);
            this.pager.set_dataSource(value);
        },
        'data:displayLocals': function (sender, args) {
            var data = this.get_data();
            var filter = data.get_projects().get_filter().get_customFilters();

            if (!args.newValue) {
                filter.set('hideRelatedLocals', []);
            } else {
                filter.remove('hideRelatedLocals');
            }
            this.reloadProjects();
        }
    },

    customFunctions: {
        'assembleAndApplyFilter': function () {
            var data = this.get_data();
            var filter = data.get_projects().get_filter().get_customFilters();

            var selectedAdvancedFilterItem = this.topPanel.filtersPanel.advancedFilter.get_selectedFilter();
            if (selectedAdvancedFilterItem && selectedAdvancedFilterItem.id != 'none') {
                filter.set('complexFilter', selectedAdvancedFilterItem.get_id());
            } else {
                filter.remove('complexFilter');
            }

            if (data.get_G4FilterEnabled()) {
                filter.set('G4', data.get_G4SelectedDate().format("u"));
            } else {
                filter.remove('G4');
            }

            this.reloadProjects();
        },
        'reloadProjects': function () {
            this.get_data().get_projects().load();
        }
    },

    controls: [
    // #region Edit Project Summary Popup
        {
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
        onLoad: function () {
            this.get_data().add_selectedProjectChanged(function (sender, args) {
                if (args.newValue) {
                    this.container.projectSummaryEdit.set_dataSource(args.newValue);
                    this.open();
                } else {
                    this.close();
                }
            }, this);
        },
        onCommand: function (sender, args) {
            var entity = this.get_data().get_selectedProject();

            if (args.button == 'Close' || args.button == 'Cancel' && !entity.isNew())
                Repository.Reload(entity);

            if (args.button == 'Save') {
                Repository.Save(entity);
            }

            this.get_data().set_selectedProject(null);
        }
    },
    // #endregion
    // #region Columns Visibility Popup
        {
        id: 'columnsVisibilityPopup',
        type: 'popup',
        width: '200px',
        height: '?',
        maxHeight: 530,
        scrolling: true,
        title: 'Show/Hide Columns',
        buttons: ['Save', 'Cancel'],

        customFunctions: {
            reassembleHiddenColumns: function () {
                if (!this._columnMappings) {
                    var cmprototype = {};
                    Auto.Property(cmprototype, { name: 'checked', autoEvent: true, defaultValue: true });

                    this._columnMappings = [
                            { key: 'requestNumber', name: 'PR Number' },
                            { key: 'modelCode', name: 'Model Code' },
                            { key: 'modelName', name: 'Model Name' },
                            { key: 'country', name: 'Country' },
                            { key: 'operator', name: 'Operator/Open' },
                            { key: 'modelSuffix', name: 'Model Suffix' },
                            { key: 'amount', name: 'Amount' },
                            { key: 'sWVersion', name: 'SW Version' },
                            { key: 'modelLine', name: 'Type' },
                            { key: 'departmentName', name: 'R&D' },
                            { key: 'sWQAStep', name: 'SW Q/A Step' },
                            { key: 'qMPlan', name: 'QM PLan' },
                            { key: 'tAReferencePlan', name: 'TA Reference Plan' },
                            { key: 'tAReplan', name: 'TA Replan' },
                            { key: 'KPITarget', name: 'KPI Target' },
                            { key: 'gate2ProjectQualityIndex', name: 'PQI-G2' },
                            { key: 'tAProjectQualityIndex', name: 'PQI-TA' },
                            { key: 'currentProjectQualityIndex', name: 'Current PQI' },
                            { key: 'tADateDQMS', name: 'TA Date (DQMS)' },
                            { key: 'tADateDVPMS', name: 'TA Date (DVPMS)' },
                            { key: 'status', name: 'Status' },
                            { key: 'modelLeader', name: 'Model Leader' },
                            { key: 'defects', name: 'Defects' },
                            { key: 'cALevel', name: 'CA Level' },
                            { key: 'comments', name: 'Comments' }
                        ].makeObservable();

                    this._columnMappings.forEach(function (cm) { Object.extend(cm, cmprototype); });
                }

                var hiddenColumns = this.get_data().get_hiddenColumns();

                this._columnMappings.forEach(function (column) {
                    column.set_checked(!hiddenColumns.contains(column.key));
                });

                return this._columnMappings;
            }
        },

        onLoad: function () {
            this.container.columnsRepeater.set_dataSource(this.reassembleHiddenColumns());

            this.get_data().get_hiddenColumns().add_changed(function (sender, args) {
                this.reassembleHiddenColumns();
            }, this);
        },

        controls: [
                {
                    type: 'repeater',
                    id: 'columnsRepeater',
                    padding: '5',
                    height: '?',
                    template: {
                        type: 'panel',
                        height: '20',
                        orientation: 'horizontal',
                        bindings: { '*': 'checkbox' },
                        controls: [
                            {
                                type: 'checkbox',
                                id: 'checkbox',
                                height: '16',
                                padding: '0 2',
                                bindings: { 'name': 'text', 'checked': 'state' },
                                handlers: {
                                    'onChanged': function () {
                                        this.get_dataSource().set_checked(this.get_state() == CheckBoxStates.checked);
                                    }
                                }
                            }
                        ]
                    }
                }
            ],
        onCommand: function (sender, args) {
            if (args.button === 'Save') {
                this.get_data().get_hiddenColumns().synchronize(
                        this._columnMappings.where(function (cm) { return !cm.get_checked(); }).select(function (cm) { return cm.key; })
                    );
            }
            else {
                this.reassembleHiddenColumns();
            }

            this.close();
        }
    },
    // #endregion
        {
            type: "pageHeader",
            title: "Planning Summary"
        },
        {
            id: 'topPanel',
            type: 'panel',
            height: '?',
            width: '*',
            layout: 'wrap',
            orientation: 'horizontal',
            controls: [
                {
                    id: 'filtersPanel',
                    type: 'panel',
                    height: '33px',
                    width: '?',
                    orientation: 'horizontal',
                    padding: '5 0',
                    bindings: {
                        'data:G4SelectedDate': 'datePicker'
                    },
                    controls: [
                        {
                            id: 'advancedFilter',
                            type: 'advancedFilter',
                            valign: 'middle',
                            handlers: {
                                'selectedFilterIdChanged': function (sender, args) {
                                    this.get_window().assembleAndApplyFilter();
                                }
                            }
                        },
                        {
                            type: 'checkbox',
                            width: '140px',
                            height: '18',
                            valign: 'middle',
                            id: 'displayLocalsCheckbox',
                            text: 'Display child projects',
                            bindings: {
                                'data:displayLocals': 'state'
                            }
                        },
                        {
                            type: 'checkbox',
                            height: '18px',
                            valign: 'middle',
                            width: '270px',
                            text: 'Show projects with G4 between today and date:',
                            bindings: {
                                'data:G4FilterEnabled': 'state'
                            }
                        },
                        {
                            id: 'datePicker', type: 'datePicker', valign: 'middle', height: '22px', width: '100px',
                            handlers: {
                                'dateChanged': function (sender, args) {
                                    this.get_data().set_G4SelectedDate(args.newValue);
                                }
                            }
                        },
                        {
                            type: 'button', text: 'Apply', valign: 'middle', width: '70px',
                            onClick: function () {
                                this.get_window().assembleAndApplyFilter();
                            }
                        }
                    ]
                },
                { type: 'panel' },
                {
                    id: 'exportPanel',
                    type: 'panel',
                    height: '33px',
                    width: '495px',
                    orientation: 'horizontal',
                    controls: [
                        {
                            type: 'button', width: '150px', text: 'Show/Hide Columns',
                            onClick: function () {
                                this.get_window().columnsVisibilityPopup.open();
                            }
                        },
                        {
                            type: 'button', width: '115px', text: 'Excel Report',
                            domHandlers: {
                                'mousedown': function () {
                                    var data = this.get_window().get_data();
                                    var filter = data.get_projects().get_filterString();
                                    var searchQuery = data.get_projects().get_searchQuery() || "";
                                    this.domElement.href = Services.ExportService.GetUrl('summary', filter, searchQuery, null);
                                }
                            }
                        },
                        {
                            type: 'button', width: '110px', text: 'MM Report',
                            onClick: function () {
                                var data = this.get_window().get_data();
                                var filter = data.get_projects().get_filterString();
                                var searchQuery = data.get_projects().get_searchQuery() || "";

                                this.reportPopup.set_uri('planning/summary/mmReportDialog|filter=' + filter + '|searchQuery=' + searchQuery);
                                this.reportPopup.open();
                            },
                            controls: [
                                {
                                    type: 'popup',
                                    id: 'reportPopup',
                                    title: 'Select Handsets',
                                    width: '500',
                                    height: '400'
                                }
                            ]
                        },
                        {
                            type: 'button', width: '110px', text: 'TAM Report',
                            onClick: function () {
                                var data = this.get_window().get_data();
                                var filter = data.get_projects().get_filterString();
                                var searchQuery = data.get_projects().get_searchQuery() || "";

                                this.reportPopup.set_uri('planning/summary/tamReportDialog');
                                this.reportPopup.open();
                            },
                            controls: [
                                {
                                    type: 'popup',
                                    id: 'reportPopup',
                                    title: 'TAM Report',
                                    width: '800',
                                    height: '600'
                                }
                            ]
                        }
                    ]
                }
            ]
        },
        {
            id: 'scrollPanel',
            type: 'scrollablePanel',
            horizontal: true,

            bindings: {
                '*': 'grid'
            },

            controls: [
                {
                    id: 'grid',
                    type: 'dataGrid',
                    width: '?',

                    selectable: false,
                    resizable: true,

                    onLoad: function () {
                        var user = Application.get_context().get("currentUser");
                        if (!user.can('Project', PermissionLevel.update)) {
                            this.get_data().get_hiddenColumns().add('actions');
                        };

                        this.set_hiddenColumns(this.get_data().get_hiddenColumns());
                    },

                    handlers: {
                        columnResized: function (sender, args) {
                            $.cookie('projectSummary_grid_' + args.columnName + '_width', args.width.pixels, { expires: 365 });
                        }
                    },

                    rowHeight: '40px',

                    rowProperties: {
                        onAttached: function () {
                            var height = this.qMPlanCell.controls.first().domElement.offsetHeight;

                            if (height > 40) {
                                this.set_height(height + 5);
                                this.update();
                            } else if (this.get_height().pixels != 40) {
                                this.set_height(40);
                                this.update();
                            }
                        }
                    },

                    //TODO: 'implement more "smart" way of saving/loading resize states'

                    columnProperties: {
                        actions: {
                            headerText: 'Actions',
                            template: {
                                bindings: {
                                    '*': function (sender, args) {
                                        var ds = args.newValue;

                                        if (ds) {
                                            if (ds.get_isReadOnly()) {
                                                this.hide();
                                            }
                                        }

                                        if (!Application.get_currentUser().get_isSuperUser() &&
                                            !this.get_data().get_userProjects().contains(ds, function (projectInfo, project) {
                                                return projectInfo.get_projectId() == project.get_id();
                                            })) {

                                            this.hide();
                                        }
                                    }
                                },
                                type: 'link',
                                text: 'Edit',
                                halign: 'center',
                                onClick: function () {
                                    this.get_data().set_selectedProject(this.get_dataSource());
                                },
                                height: '18',
                                width: '60'
                            },
                            minWidth: 60,
                            maxWidth: 60,
                            width: 60,
                            custom: true
                        },
                        requestNumber: {
                            headerText: 'PR Number',
                            width: $.cookie('projectSummary_grid_requestNumber_width') ? $.cookie('projectSummary_grid_requestNumber_width') : '80'
                        },
                        modelCode: {
                            headerText: 'Model Code',
                            template: {
                                type: 'link',
                                width: '*',
                                bindings: {
                                    '*': function (sender, args) {
                                        if (args.newValue) {
                                            this.set_text(args.newValue.get_modelCode());
                                            this.set_url('page:planning/projectDetails|projectId=' + args.newValue.get_projectId());
                                        }
                                    }
                                },
                                height: '18'
                            },
                            width: $.cookie('projectSummary_grid_modelCode_width') ? $.cookie('projectSummary_grid_modelCode_width') : '90',
                            custom: true
                        },
                        modelName: {
                            headerText: 'Model Name',
                            width: $.cookie('projectSummary_grid_modelName_width') ? $.cookie('projectSummary_grid_modelName_width') : '90'
                        },
                        country: {
                            headerText: 'Country',
                            width: $.cookie('projectSummary_grid_country_width') ? $.cookie('projectSummary_grid_country_width') : '100',
                            showTooltip: true
                        },
                        operator: {
                            headerText: 'Operator/Open',
                            width: $.cookie('projectSummary_grid_operator_width') ? $.cookie('projectSummary_grid_operator_width') : '100',
                            showTooltip: true
                        },
                        modelSuffix: {
                            headerText: 'Model Suffix',
                            width: $.cookie('projectSummary_grid_modelSuffix_width') ? $.cookie('projectSummary_grid_modelSuffix_width') : '90'
                        },
                        amount: {
                            headerText: 'Amount',
                            width: $.cookie('projectSummary_grid_amount_width') ? $.cookie('projectSummary_grid_amount_width') : '60'
                        },
                        sWVersion: {
                            headerText: 'SW Version',
                            width: $.cookie('projectSummary_grid_sWVersion_width') ? $.cookie('projectSummary_grid_sWVersion_width') : '85'
                        },
                        modelLine: {
                            headerText: 'Type',
                            width: $.cookie('projectSummary_grid_modelLine_width') ? $.cookie('projectSummary_grid_modelLine_width') : '50'
                        },
                        departmentName: {
                            headerText: 'R&D',
                            width: $.cookie('projectSummary_grid_departmentName_width') ? $.cookie('projectSummary_grid_departmentName_width') : '60'
                        },
                        sWQAStep: {
                            headerText: 'SW Q/A Step',
                            width: $.cookie('projectSummary_grid_sWQAStep_width') ? $.cookie('projectSummary_grid_sWQAStep_width') : '90'
                        },
                        qMPlan: {
                            headerText: 'QM Plan',
                            minWidth: 170,
                            width: $.cookie('projectSummary_grid_qMPlan_width') ? $.cookie('projectSummary_grid_qMPlan_width') : '180',
                            multiline: true
                        },
                        tAReferencePlan: {
                            headerText: 'TA Reference Plan',
                            format: '{0:d}',
                            width: $.cookie('projectSummary_grid_tAReferencePlan_width') ? $.cookie('projectSummary_grid_tAReferencePlan_width') : '120'
                        },
                        kPITarget: {
                            headerText: 'KPI Target',
                            format: '{0:d}',
                            width: $.cookie('projectSummary_grid_kPITarget_width') ? $.cookie('projectSummary_grid_kPITarget_width') : '120'
                        },
                        tAReplan: {
                            headerText: 'TA Replan',
                            format: '{0:d}',
                            width: $.cookie('projectSummary_grid_tAReplan_width') ? $.cookie('projectSummary_grid_tAReplan_width') : '80'
                        },
                        gate2ProjectQualityIndex: {
                            headerText: 'PQI-G2',
                            width: $.cookie('projectSummary_grid_gate2ProjectQualityIndex_width') ? $.cookie('projectSummary_grid_gate2ProjectQualityIndex_width') : '70'
                        },
                        tAProjectQualityIndex: {
                            headerText: 'PQI-TA',
                            width: $.cookie('projectSummary_grid_tAProjectQualityIndex_width') ? $.cookie('projectSummary_grid_tAProjectQualityIndex_width') : '70'
                        },
                        currentProjectQualityIndex: {
                            headerText: 'Current PQI',
                            width: $.cookie('projectSummary_grid_currentProjectQualityIndex_width') ? $.cookie('projectSummary_grid_currentProjectQualityIndex_width') : '90'
                        },
                        tADateDQMS: {
                            headerText: 'TA Date (DQMS)',
                            format: '{0:d}',
                            width: $.cookie('projectSummary_grid_tADateDQMS_width') ? $.cookie('projectSummary_grid_tADateDQMS_width') : '120'
                        },
                        tADateDVPMS: {
                            headerText: 'TA Date (DVPMS)',
                            format: '{0:d}',
                            width: $.cookie('projectSummary_grid_tADateDVPMS_width') ? $.cookie('projectSummary_grid_tADateDVPMS_width') : '120'
                        },
                        status: {
                            headerText: 'Status',
                            custom: true,
                            cssClass: 'project_status_cell',
                            template: {
                                type: 'label',
                                width: '*',
                                height: '16',
                                valign: 'middle',
                                bindings: {
                                    'status': function (sender, args) {
                                        var value = args.newValue;
                                        if (value == ProjectStatus.approved) {
                                            this.set_text('');
                                            this.addCssClass('item_approved');
                                        }
                                        else {
                                            this.set_text(value);
                                            this.removeCssClass('item_approved');
                                        }
                                    },
                                    'indicator': function (sender, args) {
                                        var project = this.get_dataSource();
                                        this.removeCssClass('red_light_icon green_light_icon yellow_light_icon');

                                        if (project.get_status() == ProjectStatus.draft || project.get_status() == ProjectStatus.onProgress) {
                                            if (args.newValue) {
                                                this.addCssClass(args.newValue.get_name() + '_light_icon');
                                            }
                                        }
                                    }
                                }
                            },
                            width: $.cookie('projectSummary_grid_projectStatus_width') ? $.cookie('projectSummary_grid_projectStatus_width') : '100'
                        },
                        modelLeader: {
                            headerText: 'Model Leader',
                            width: $.cookie('projectSummary_grid_modelLeader_width') ? $.cookie('projectSummary_grid_modelLeader_width') : '100',
                            showTooltip: true
                        },
                        defects: {
                            headerText: 'Defects',
                            width: $.cookie('projectSummary_grid_defects_width') ? $.cookie('projectSummary_grid_defects_width') : '150'
                        },
                        cALevel: {
                            headerText: 'CA Level',
                            width: $.cookie('projectSummary_grid_CALevel_width') ? $.cookie('projectSummary_grid_CALevel_width') : '200'
                        },
                        comments: {
                            headerText: 'Comments',
                            width: $.cookie('projectSummary_grid_comments_width') ? $.cookie('projectSummary_grid_comments_width') : '200'
                        }
                }
            }
            ]
        },
        {
            id: 'pager',
            type: 'pager'
        },
        {
            id: 'searchPane',
            type: 'searchPane',
            onSearch: function (sender, args) {
                var ds = this.get_data().get_projects();
                ds.set_searchQuery(args.query, true);
            }
        }
    ]
})