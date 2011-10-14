({
    title: 'Create running changes',
    cssClass: 'create_running_changes',
    data:
    [
        'projectId',
        'project',
        'planSource',
        { name: 'taskAlignType', value: 'from' },
        { name: 'inheritType', value: PlanOwnerType.template },
        { name: 'alignDate', value: null },
        { name: 'team', value: [].makeObservable() },
        { name: 'countries', value: [].makeSorted('name') },
        { name: 'otherCountries', value: [].makeSorted('name') },
        { name: 'selectedCountries', value: [].makeObservable() },
        { name: 'selectedOtherCountries', value: [].makeObservable() },
        { name: 'operators', value: [].makeObservable() }
    ],

    customFunctions: {
        'save': function () {
            var data = this.get_data();

            var countryIds = [];

            var convertCountriesWithProjectsToContract = function(countries) { 
                return countries.select(function(c) { return { m_Item1: c.get_id(), m_Item2: c.get_projectId() }; });
            };

            countryIds.add(convertCountriesWithProjectsToContract(data.get_selectedCountries()));
            countryIds.add(convertCountriesWithProjectsToContract(data.get_selectedOtherCountries()));

            Services.ProjectService.CreateRunningChange
            (
                {
                    _parentProjectId: data.get_projectId(),
                    _from: (data.get_taskAlignType() == "from" && data.get_alignDate()) ? data.get_alignDate().toWcfString() : null,
                    _to: (data.get_taskAlignType() == "to" && data.get_alignDate()) ? data.get_alignDate().toWcfString() : null,
                    _countryIds: countryIds,
                    _planSourceId: data.get_planSource() ? data.get_planSource().get_id() : null,
                    _inheritType: data.get_inheritType()._value
                },
                data.get_project() instanceof LocalProject,
                data.get_taskAlignType(),
                data.get_alignDate(),
                data.get_project().get_from(),
                function (result) {
                    Application.loadPage('planning/projectDetails|projectId=' + result._id);
                    this.sendMessage('RunningChangeCreated');
                }.bind(this),
                function (error) {
                    Application.showError("Error", error);
                }
            );
        }
    },

    onLoad: function () {
        var data = this.get_data();
        var projectId = this.get_param('projectId') * 1;

        data.set_projectId(projectId);

        Services.ProjectService.GetRunningChangeAvailableCountries
        (
            projectId,
            function (result) {
                if (result.length == 0) {
                    Application.showError('Error', 'Running changes for all countries are already created');
                    this.get_window().close();
                    return;
                }

                Repository.Get('HandsetProject', projectId, function (result) {
                    data.set_project(result);
            
                    if (result instanceof LocalProject) {
                        this.container.countries.hide(true);
                        data.get_selectedCountries().attach(data.get_countries());
                    }
                } .bind(this));
                                
                data.get_countries().synchronize(result.where(function(p) { return p.get_projectId() == projectId; }));
                data.get_otherCountries().synchronize(result.where(function(p) { return p.get_projectId() != projectId; }));
            } .bind(this),
            function (error) {
                Application.showError('Error', error);
            }
        );


    },
    controls:
    [
        {
            id: 'container',
            type: 'panel',
            orientation: 'horizontal',
            width: '*',
            cssClass: 'running_changes_content',
            controls:
            [
                {
                    type: 'collapsablePanel',
                    margin: '0 0 3 0',
                    title: 'Select a project or a template to inherit from',
                    showCollapseButton: false,
                    cssClass: 'planning_block',
                    showStatusBar: true,
                    buttons: [
                        'Create Running Change'
                    ],
                    onCommand: function () {
                        this.get_window().save();
                    },
                    controls: [
                        {
                            id: 'templates',
                            type: 'projectPlanningSelector',
                            width: '100%',
                            height: '100%',
                            onLoad: function () {
                                this.set_dataSource(this.parent.get_data());
                            }
                        }
                    ]
                },
                {
                    type: 'panel',
                    id: 'countries',
                    width: '300px',
                    controls: [
                        {
                            id: 'ownCountries',
                            type: 'collapsablePanel',
                            title: 'Select a subset of countries (project own)',
                            showCollapseButton: false,
                            cssClass: 'planning_block',
                            width: '*',
                            controls: [
                                {
                                    type: 'countriesSelector',
                                    width: '100%',
                                    height: '100%',
                                    countryItemWidth: 200,
                                    onLoad: function () {                                
                                        this.set_dataSource(this.parent.get_data().get_countries());
                                        this.set_selectedCountries(this.parent.get_data().get_selectedCountries());
                                    }
                                }
                            ]
                        },
                        {
                            id: 'otherCountries',
                            type: 'collapsablePanel',
                            title: 'Select a subset of countries (other countries)',
                            showCollapseButton: false,
                            cssClass: 'planning_block',
                            width: '*',
                            controls: [
                                {
                                    type: 'countriesSelector',
                                    width: '100%',
                                    height: '100%',
                                    countryItemWidth: 200,
                                    onLoad: function () {                                
                                        this.set_dataSource(this.parent.get_data().get_otherCountries());
                                        this.set_selectedCountries(this.parent.get_data().get_selectedOtherCountries());
                                    },
                                    addInfo:
                                    {
                                        type: 'panel',                                
                                        width: '?',
                                        height: '?',
                                        valign: 'middle',
                                        controls: [
                                            {
                                                id: 'pop',
                                                type: 'popup',
                                                width: '75%',
                                                height: '75%'
                                            },
                                            {
                                                type: 'link',
                                                cssClass: 'infoBtn',
                                                tooltip: 'Open corresponding project',
                                                width: '16',
                                                height: '16',
                                                onClick: function() {                                                    
                                                    this.parent.pop.set_uri('planning/projectDetails|projectId=' + this.parent.parent.get_dataSource().get_projectId());
                                                    this.parent.pop.open();
                                                }
                                            }
                                        ]
                                    }
                                }
                            ]
                        }
                    ]
                }                
            ]
        }
    ]
})