({
    title: 'Detach from open',
    layout: 'stack',
    orientation: 'vertical',

    data: [
        'project'
    ],

    onLoad: function() {
        Repository.Load('HandsetProject', this.get_param('projectId'), function(result) {            
            this.get_data().set_project(result);
        }.bind(this));               
    },

    customFunctions: {

        'save': function () {        
            var data = this.get_data();

            var countryGroups = [{
                Key: this.projectDescriptionPanel.projectDescription.get_text(),
                Value: [ data.get_project().get_countryId() ]
            }];                               

            Services.ProjectService.SplitByCountries(
                data.get_project().get_openProjectId(),
                countryGroups,
                function (result) {                    
                    Application.loadPage('planning/projectDetails|projectId=' + result[0].get_id());
                },
                function (error) {
                    Application.showError('Error', error);
                }
            );
        }

    },

    controls: [
        {
            type: 'panel',
            id: 'projectDescriptionPanel',
            layout: 'stack',
            orientation: 'horizontal',
            margin: '10 20 10 20',
            controls: [
                {
                    type: 'label',                    
                    text: 'New project description:'
                },
                {                    
                    type: 'textBox',
                    id: 'projectDescription',
                    margin: '5 0 0 0'
                }
            ]
        }        
    ]
})