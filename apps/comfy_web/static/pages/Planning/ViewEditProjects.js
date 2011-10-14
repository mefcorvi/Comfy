({
    title: "View and Edit Projects",
    controls: [
        {
            id: 'projects',
            type: 'silverlight',
            url: 'silverlight:SilverlightAdvGrid.xap',
            width: '100%',
            height: '100%',
            onSlPreLoad: function(sender, args) {                
                var currentUser = Application.get_context().get('currentUser');
                var userId = currentUser.get_id();
                
                var mode = this.parent.get_param('mode');
                var projects = this.parent.get_param('projects');

                var modeId = 0;
                
                if (mode == 'view') {
                    modeId = 2;
                }
                        
                if (mode == 'edit') {
                    modeId = 1;
                }
                
                var dataSource = {
                    'userId': userId,
                    'mode': modeId,
                    'projects': projects || ''
                };
        
                args.params = 'Mode=' + dataSource.mode + ',UserId=' + dataSource.userId  + ',SLControlID=sl,ProjectsIds=' + dataSource.projects;
            }
        }
    ]
})