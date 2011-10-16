({
    title: "Master Page",
    layout: "stack",
    orientation: 'vertical',
    cssClass: 'master_page',
    valign: 'stretch',
    padding: '0 5px 0 0',

    customFunctions: {},

    controls:
    [
        {
            type: 'popup',
            id: 'confirmPopup',
            minWidth: '100',
            width: '30%',
            height: '?',
            scrolling: true,
            buttons: ['Yes', 'No'],
            controls: [
                {
                    type: 'label',
                    padding: '5',
                    width: '*',
                    height: '?',
                    id: 'popupText'
                }
            ]
        },
        {
            type: 'popup',
            id: 'errorPopup',
            minWidth: 100,
            width: '30%',
            height: '?',
            scrolling: true,
            buttons: ['OK'],
            controls: [
                {
                    type: 'label',
                    padding: '5',
                    width: '*',
                    height: '?',
                    id: 'popupText'
                }
            ]
        },

	// top panel
        {
            id: "page_container",
            type: "box",
            width: '*',
            height: '*'
        }
    ]
})