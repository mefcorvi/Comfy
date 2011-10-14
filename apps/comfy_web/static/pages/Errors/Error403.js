({
    title: "403",
    cssClass: 'error_403_page',
    
    controls:
    [
        {
            type: "pageHeader",
            title: "Error: Access denied",
            description: "Error 403"
        },
        {
            type: 'label',
            text: 'You have attempted to access a page for which you are not authorized.',
            padding: '5',
            cssClass: 'not_authorized_error'
        }
    ]
})