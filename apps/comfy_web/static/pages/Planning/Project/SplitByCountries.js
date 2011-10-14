({
    title: 'Split by Countries',
    layout: 'stack',
    orientation: 'vertical',
    cssClass: 'split_by_countries_page',
    data:
    [
        'projectId',
        'project',
        'scope',
        'handset',
        'planSource',
        { name: 'taskAlignType', value: 'from' },
        { name: 'inheritType', value: 'Template' },
        { name: 'alignDate', value: null },
        { name: 'team', value: [].makeObservable() },
        { name: 'countries', value: [].makeObservable() },
        { name: 'selectedCountries', value: [].makeObservable() },
        { name: 'operators', value: [].makeObservable() },
        { name: 'countryGroups', value: [].makeObservable() },
        'selectedGroup',
        'selectedCountryInTree',
        'parentOfSelectedNode'
    ],

    customFunctions: {

        'addSelectedCountriesToTree': function () {
            var data = this.get_data();

            if (data.get_selectedGroup()) {
                data.get_selectedGroup().countries.add(data.get_selectedCountries());
                data.get_countries().remove(data.get_selectedCountries());
            }

            data.get_selectedCountries().clear();
        },

        'removeSelectedCountryFromTree': function () {
            var data = this.get_data();
            if (data.get_selectedCountryInTree()) {
                data.get_parentOfSelectedNode().get_dataSource().countries.remove(data.get_selectedCountryInTree());
                data.get_countries().add(data.get_selectedCountryInTree());
                data.set_selectedCountryInTree(null);
            }
        },

        'selectGroup': function (group) {
            var data = this.get_data();
            data.set_selectedGroup(group);
            data.set_selectedCountryInTree(null);
        },

        'selectCountryInTree': function (countryNode) {
            var data = this.get_data();
            data.set_parentOfSelectedNode(countryNode.parentNode);
            data.set_selectedCountryInTree(countryNode.get_dataSource());
        },

        'createAndSelectNewGroup': function (groupName) {
            var data = this.get_data();
            if (data.get_countryGroups().length > 0 && data.get_countryGroups().where(function (g) { return g.name == groupName; }).length > 0) {
                Application.showError('Error', 'You must choose unique group name');
                return;
            }

            var newGroup = { name: groupName, countries: [].makeObservable() };
            data.get_countryGroups().add(newGroup);
            data.set_selectedGroup(newGroup);
        },

        'removeSelectedGroup': function (groupName) {
            var data = this.get_data();
            if (data.get_selectedGroup()) {
                data.get_countries().add(data.get_selectedGroup().countries);
                data.get_selectedGroup().countries.clear();
                data.get_countryGroups().remove(data.get_selectedGroup());
                data.set_selectedGroup(null);
                data.set_selectedCountryInTree(null);
            }
        },

        'validate': function () {
            var data = this.get_data();

            if (data.get_countryGroups().length == 0) {
                Application.showError('Error', 'You must create at least one group');
                return false;
            }

            var treeCountainsCountries = false;
            for (var i = 0; i < data.get_countryGroups().length; i++) {
                if (data.get_countryGroups()[i].countries.length > 0) {
                    treeCountainsCountries = true;
                }
            }

            if (!treeCountainsCountries) {
                Application.showError('Error', 'You must add at least one country to the tree');
                return false;
            }

            return true;
        },

        'save': function () {
            var data = this.get_data();

            var countryGroups = [];

            data.get_countryGroups().forEach(function (group) {
                countryGroups.push(
                    {
                        Key: group.name,
                        Value: group.countries.select(function (country) { return country.get_id(); })
                    }
                );
            });

            if (this.validate()) {
                Services.ProjectService.SplitByCountries(
                    data.get_projectId(),
                    countryGroups,
                    function (result) {
                        Application.loadPage('planning/managePlans');
                    },
                    function (error) {
                        Application.showError('Error', error);
                    }
                );
            }
        }
    },

    onLoad: function () {
        var data = this.get_data();
        var projectId = this.get_param('projectId') * 1;

        data.set_projectId(projectId);

        Repository.Get('HandsetProject', projectId, function (result) {
            data.set_project(result);

            Repository.Get('Handset', result.get_handsetId(), function (handset) {
                data.set_handset(handset);
            });

            var scope = ProjectScope.getByProject(result);
            data.set_scope(scope);

            result.get_countries(function (res) { data.get_countries().synchronize(res) });

        } .bind(this));
    },
    controls:
    [
    //#region Add new group popup
        {
            id: 'addNewGroupPopup',
            type: 'popup',
            title: 'Add new group',
            width: '300',
            height: '126',
            customFunctions: {
                'addGroup': function () {
                    var groupNameControl = this.container.innerPanel.groupName;
                    var name = groupNameControl.get_text();

                    if (name) {
                        this.get_window().createAndSelectNewGroup(name);
                        groupNameControl.set_text("");
                        this.close();
                    }
                }
            },
            controls: [
                {
                    type: 'panel',
                    cssClass: 'add_new_group_popup',
                    padding: '5',
                    id: 'innerPanel',
                    controls: [
                        {
                            type: 'textBox',
                            text: '',
                            width: '100%',
                            watermark: 'Enter the group name...',
                            id: 'groupName',
                            onEnterPressed: function () {
                                this.get_window().addGroup();
                            }
                        }
                    ]
                }
            ],
            buttons: ['Add', 'Cancel'],
            onOpen: function () {
                var groupNameControl = this.container.innerPanel.groupName;
                groupNameControl.focus();
            },
            onCommand: function (sender, args) {
                if (args.button == 'Add') {
                    this.addGroup();
                }

                if (args.button == 'Cancel') {
                    this.close();
                }
            }
        },
        //#endregion
        {
        id: 'container',
        type: 'panel',
        orientation: 'horizontal',
        cssClass: 'running_changes_content',
        controls:
            [
                {
                    type: 'collapsablePanel',
                    title: 'Groups of Countries',
                    layout: 'stack',
                    orientation: 'vertical',
                    showCollapseButton: false,
                    cssClass: 'planning_block',
                    padding: '5',
                    showStatusBar: true,
                    scrolling: true,
                    domHandlers: {
                        'dblclick': function() {
                            this.get_window().addNewGroupPopup.open();
                        }
                    },
                    buttons: [
                        'Add group',
                        'Remove'
                    ],
                    onCommand: function (sender, args) {
                        if (args.button == 'Remove') {
                            this.get_window().removeSelectedGroup();
                        }

                        if (args.button == 'Add group') {
                            this.get_window().addNewGroupPopup.open();
                        }
                    },
                    controls: [
                        {
                            type: 'tree',
                            width: '100%',
                            childProperty: 'countries',
                            emptyDataTemplate: [
                                {
                                    type: 'label',
                                    padding: '5',
                                    width: '*',
                                    text: 'You should create a group. You can add new group by double-click on this panel.',
                                    cssClass: 'data_not_found_label'
                                },
                                {
                                    type: 'label',
                                    padding: '5 0',
                                    text: "Group doesn't contain any country",
                                    cssClass: 'group_countries_not_found'
                                }
                            ],
                            nodeTemplate: [
                                {
                                    cssClass: 'countries_group_node',
                                    controls: [
                                        {
                                            type: 'link',
                                            id: 'groupName',
                                            padding: '5 0',
                                            margin: '0 0 0 3',
                                            cssClass: 'countries_group',
                                            customFunctions: {
                                                '_checkChanged': function (sender, args) {
                                                    var ds = this.parent.get_dataSource();

                                                    if (args.newValue === ds) {
                                                        this.parent.set_isSelected(true);
                                                    } else if (args.oldValue === ds) {
                                                        this.parent.set_isSelected(false);
                                                    }
                                                }
                                            },
                                            onLoad: function () {
                                                this.get_window().get_data().add_selectedGroupChanged(this._checkChanged, this);
                                            },
                                            onClick: function () {
                                                this.get_window().selectGroup(this.parent.get_dataSource());
                                            }
                                        }
                                    ],
                                    bindings: {
                                        '*': function (sender, args) {
                                            var dataItem = args.newValue;
                                            this.groupName.set_text(dataItem.name);
                                        }
                                    }
                                },
                                {
                                    cssClass: 'country_node',
                                    margin: '0 0 0 2',
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
                                            type: 'link',
                                            padding: '1 1',
                                            width: '?',
                                            height: '*',
                                            id: 'countryName',
                                            onClick: function () {
                                                this.get_window().selectCountryInTree(this.parent);
                                                this.get_window().removeSelectedCountryFromTree();
                                            }
                                        }
                                    ],
                                    bindings: {
                                        'name': 'countryName',
                                        'imageId': 'image'
                                    }
                                }
                            ],

                            onLoad: function () {
                                this.set_dataSource(this.get_data().get_countryGroups());
                            }
                        }
                    ]
                },
                {
                    id: 'countries',
                    type: 'collapsablePanel',
                    margin: '3 0 0 0',
                    title: 'Select a subset of countries',
                    showCollapseButton: false,
                    cssClass: 'planning_block',
                    controls: [
                        {
                            type: 'countriesSelector',
                            selectMode: 'single',
                            countryItemWidth: 200,
                            onSelect: function (sender, args) {
                                this.get_window().addSelectedCountriesToTree();
                            },
                            onLoad: function () {
                                this.set_dataSource(this.parent.get_data().get_countries());
                                this.set_selectedCountries(this.parent.get_data().get_selectedCountries());
                            }
                        }
                    ]
                }
            ]
        }
    ]
})