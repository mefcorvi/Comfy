({
    title: "My Favorites",
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
        
                args.params = 'Mode=0,'+ ',UserId=' + userId  + ',SLControlID=sl';
            }
        }
    ]
})