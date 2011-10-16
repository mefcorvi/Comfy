({
    title: "Templates",
    controls: [
        {
            id: 'Templates',
            type: 'silverlight',
            url: 'silverlight:SilverlightAdvGrid.xap',
            width: '100%',
            height: '100%',
            onSlPreLoad: function(sender, args) {
                var userId = Application.get_context().get('currentUser').get_id();
                args.params = 'Mode=3,UserId=' + userId  + ',SLControlID=sl';
            }
        }
    ]
})