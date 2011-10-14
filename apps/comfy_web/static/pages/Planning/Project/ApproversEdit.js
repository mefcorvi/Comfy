({
    title: "Edit project approvers",
    layout: "stack",
    orientation: 'vertical',
    cssClass: 'notification_settings',
    padding: '5',
    height: '?',

    data: [        
        'domainEvents', 'mailEnabled'
    ],

    onLoad: function() {       
            
    },

    controls: [
        {
            type: 'panel',
            orientation: 'horizontal',
            height: '?',
            cssClass: 'settings_header',
            padding: '3 0 3 3',
            controls: [
                {
                    type: 'panel',
                    width: '150',
                    height: '?',
                    margin: '0 0 3 0',
                    border: '0 0 0 1',
                    controls: [   
                        {
                            type: 'label',
                            width: '?',
                            text: 'Role'
                        }
                    ]
                },   
                {
                    type: 'panel',
                    width: '100',
                    height: '?',
                    margin: '0 0 3 0',
                    border: '0 0 0 1',
                    controls: [             
                        {
                            type: 'label',
                            width: '?',
                            text: 'Active',
                            halign: 'center'
                        }
                    ]
                }
            ]
        },
        {
            id: 'eventsScroll',
            type: 'scrollablePanel',
            height: '?',
            maxHeight: 500,
            bindings: {
                '*': 'rolesRepeater'
            },
            controls: [
                {
                    id: 'rolesRepeater',
                    type: 'repeater',                                    
                    layout: 'stack',
                    height: '?',
                    orientation: 'vertical',
                    halign: 'center',
                    template: {
                        type: 'panel',
                        orientation: 'horizontal',
                        height: '?',
                        padding: '3 2',
                        cssClass: 'event_row',                                        
                        bindings: {
                            'roleId': function(sender, args) {
                                
                            },
                            'isActive': 'activeCheckbox'
                        },
                        controls: [
                            {
                                id: 'roleName',
                                type: 'label',
                                width: '150',
                                margin: '0 0 3 0',
                                valign: 'center'
                            },
                            {
                                id: 'activeCheckbox',
                                type: 'panel',
                                width: '100',
                                height: '?',
                                margin: '0 0 3 0',
                                bindings: {
                                    '*': 'checkbox'
                                },
                                controls: [ 
                                    {
                                        id: 'checkbox',
                                        type: 'checkbox',
                                        width: '?',
                                        halign: 'center'                                        
                                    }
                                ]
                            }                            
                        ]
                    }
                }
            ]
        }
    ]
})