({
    title: "Welcome Master",
    cssClass: 'welcome_master_page',
    layout: 'grid',

    controls:
    [
        {
            type: 'popup',
            id: 'errorPopup',
            width: '30%',
            height: '150',
            scrolling: true,
            buttons: [ 'OK' ],
            controls: [
                {
                    type: 'label',
                    id: 'popupText'
                }
            ]
        },
        {
            id: "page_container",
            cssClass: 'welcome_page_container',
            type: "box",
            padding: '0 39 0 0',
            valign: 'middle',
            halign: 'center',
            width: '762',
            height: '400',
            onAttached: function() {
                setTimeout(function() { // TODO: Fix me
                    this.update();
                }.bind(this), 1000);
            }
        }
    ]
})