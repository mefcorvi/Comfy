({
    type: 'page',
    cssClass: 'feedbacks_page',
    controls: [
        {
            type: 'pageHeader',
            title: 'Feedbacks'
        },
        {
            id: 'feedbacks',
            type: 'feedbackViewer',
            adminMode: true,
            width: '*',
            height: '*'
        }
    ]
})