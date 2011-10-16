({
    title: "Team Templates",
    orientation: 'vertical',
    data: [
        { name: 'templateDS', value: [].makeDataSource('ProjectTeamTemplate') },
        { name: 'openTemplate' },
        { name: 'localTemplate' },
        { name: 'globalTemplate' }
    ],

    customFunctions: {
        _saveTemplate: function (template) {
            Repository.Save(template, function () {
                for (var i = 0; i < template.get_roles().length; i++) {
                    Repository.Save(template.get_roles()[i]);
                }
            });
        }
    },

    onLoad: function () {
        this.get_data().get_templateDS().load(function (templates) {
            var data = this.get_data();
            data.set_openTemplate(templates.single(function (t) { return t.get_scope() == ProjectScope.open; }));
            data.set_localTemplate(templates.single(function (t) { return t.get_scope() == ProjectScope.local; }));
            data.set_globalTemplate(templates.single(function (t) { return t.get_scope() == ProjectScope.global; }));
            this.set_dataSource(data);
        } .bind(this));
    },

    bindings: {
        '*': 'templatesPanel'
    },

    controls:
    [
        {
            type: "pageHeader",
            title: "Team Templates"
        },
        {
            type: 'panel',
            id: 'templatesPanel',
            orientation: 'horizontal',
            bindings: {
                'openTemplate': 'openTemplatePanel',
                'localTemplate': 'localTemplatePanel',
                'globalTemplate': 'globalTemplatePanel'
            },
            controls: [
                {
                    type: 'collapsablePanel',
                    id: 'openTemplatePanel',
                    margin: '0 0 2 0',
                    showCollapseButton: false,
                    title: 'Open',
                    bindings: {
                        '*': function (sender, args) {
                            this.editor.set_template(args.newValue);
                        }
                    },
                    controls: [
                        {
                            type: 'teamTemplateRolesEdit',
                            id: 'editor'
                        }
                    ]
                },
                {
                    type: 'collapsablePanel',
                    id: 'globalTemplatePanel',
                    margin: '0 0 2 0',
                    showCollapseButton: false,
                    title: 'Global',
                    bindings: {
                        '*': function (sender, args) {
                            this.editor.set_template(args.newValue);
                        }
                    },
                    controls: [
                        {
                            type: 'teamTemplateRolesEdit',
                            id: 'editor'
                        }
                    ]
                },
                {
                    type: 'collapsablePanel',
                    id: 'localTemplatePanel',
                    showCollapseButton: false,
                    title: 'Local',
                    bindings: {
                        '*': function (sender, args) {
                            this.editor.set_template(args.newValue);
                        }
                    },
                    controls: [
                        {
                            type: 'teamTemplateRolesEdit',
                            id: 'editor'
                        }
                    ]
                }
            ]
        },
        {
            type: 'button',
            text: 'Save',
            onClick: function () {
                this.get_window()._saveTemplate(this.get_data().get_openTemplate());
                this.get_window()._saveTemplate(this.get_data().get_localTemplate());
                this.get_window()._saveTemplate(this.get_data().get_globalTemplate());
            }
        }
    ]
})