({
    title: "User notification settings",
    layout: "stack",
    orientation: 'vertical',
    cssClass: 'notification_settings',
    padding: '5',
    height: '?',

    data: [        
        'domainEvents', 'mailEnabled'
    ],

    onLoad: function() {       
        Services.UserNotificationSettingsService.GetDomainEvents(function(result) {            
            this.get_data().set_domainEvents(result);

            var ds = [].makeDataSource('UserNotificationSettings').filterBy('User.Id = ' + Application.get_currentUser().get_id());

            this.eventsScroll.set_dataSource(ds);            
            ds.load();

        }.bind(this));

        var user = Application.get_currentUser();
        this.get_data().set_mailEnabled(!!user.get_email());
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
                            text: 'Event'
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
                            text: 'Browser',
                            halign: 'center'
                        }
                    ]
                },
                {
                    type: 'panel',
                    width: '40',
                    height: '?',
                    border: '0 0 0 1',
                    controls: [             
                        {
                            type: 'label',
                            width: '?',
                            text: 'Mail',
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
                '*': 'eventsRepeater'
            },
            controls: [
                {
                    id: 'eventsRepeater',
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
                        onLoad: function() {
                            var eventType = this.get_dataSource().get_eventType();
                            var name = this.get_data().get_domainEvents().single(function (e) { return e._type == eventType; })._title;
                            this.eventName.set_text(name);
                        },
                        bindings: {
                            'isBrowserEnabled': 'browserCheckbox',
                            'isMailEnabled': 'mailCheckbox'
                        },
                        controls: [
                            {
                                id: 'eventName',
                                type: 'label',
                                width: '150',
                                margin: '0 0 3 0',
                                valign: 'center'
                            },
                            {
                                id: 'browserCheckbox',
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
                                        halign: 'center',
                                        onChanged: function (sender, args) {
                                            var isChecked = this.get_state() == CheckBoxStates.checked;
                                            this.parent.parent.get_dataSource().set_isBrowserEnabled(isChecked);
                                            this.parent.parent.mailCheckbox.checkbox.set_enabled(isChecked);                                    
                                        }
                                    }
                                ]
                            },
                            {
                                id: 'mailCheckbox',
                                type: 'panel',
                                width: '40',
                                height: '?',
                                bindings: {
                                    '*': 'checkbox'
                                },
                                controls: [ 
                                    {
                                        id: 'checkbox',
                                        type: 'checkbox',
                                        width: '?',
                                        halign: 'center',
                                        onChanged: function (sender, args) {
                                            var isChecked = this.get_state() == CheckBoxStates.checked;
                                            this.parent.parent.get_dataSource().set_isMailEnabled(isChecked);                                    
                                        },
                                        bindings: {
                                            '*': 'state',
                                            'data:mailEnabled': 'enabled'
                                        }
                                    }
                                ]
                            }
                        ]
                    }
                }
            ]
        },
        {
            type: 'label',
            height: 40,
            width: '*',
            padding: '0 3',
            cssClass: 'specify_mail_first',
            text: 'In order to enable email notifications you need to specify primary email address in your configuration first',
            bindings: {
                'data:mailEnabled': function(sender, args) {
                    if (args.newValue) {
                        this.hide();
                    } else {
                        this.show();
                    }
                }
            }
        }
    ]
})