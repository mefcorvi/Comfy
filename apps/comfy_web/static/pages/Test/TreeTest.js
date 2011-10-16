({
    title: 'TreeTest',
    onLoad: function() {
        var test = {
            role: { anchor: true },
            workgroup: { by: 'role.workgroupId' },
            
            users: {
                item: {
                    user: { by: 'userRole.userId' },
                    userRole: { by: '.role.id', property: 'role', anchor: true }
                }
            }
        };

        result.add_loaded(function() {
        });
        
        result.load();
    }
})