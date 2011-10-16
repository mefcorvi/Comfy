({
    title: 'GridTest',
    controls: [
        {
            type: 'dataGrid',
            id: 'testGrid',
            width: '100%',
            selectable: true,
            editable: true,
            columnProperties: {
                id: { headerText: 'Id', width: '50px', readOnly: true },
                name: { 
                    headerText: 'Name',
                    validation: {
                        notEmpty: true
                    }
                },
                phone: { 
                    headerText: 'Phone',
                    validation: {
                        expression: /^\+?[0-9\(\)-]+$/
                    }
                },
                domain: { headerText: 'Domain',
                    validation: {
                        notEmpty: true
                    }
                },
                email: { headerText: 'Email',
                    validation: {
                        notEmpty: true,
                        expression: /\w+([-+.'']\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*/
                    }
                },
                title: {                     
                    headerText: 'Title',
                    editTemplate: {
                        type: 'panel',
                        width: '98%',
                        height: '100%',
                        controls: [
                            {
                                type: 'dropDownList',
                                id: 'selector',
                                width: '100%',
                                onLoad: function() {
                                    var ds = ['Junior Engineer', 'Engineer', 'Senior Engineer'].makeObservable();
                                    
                                    this.set_items(ds);
                                }
                            }
                        ],
                        
                        bindings: {
                            '*' : function(sender, args) {
                                var value = args.newValue;
                                if(value)
                                    this.selector.set_selectedValue(value);
                                else
                                    this.selector.set_selectedValue(this.selector.get_dataSource().first());
                            }
                        },
                        
                        getter: function() {
                            return this.selector.get_selectedValue();
                        }
                    }
                },
                roles: {
                    headerText: 'Roles',
                    readOnly: true,
                    
                    template: {
                        type: 'panel',
                        width: '100%',
                        height: '100%',
                        
                        controls: [
                            {
                                type: 'label',
                                id: 'rolesLabel',
                                
                                width: '100%'
                            }
                        ],
                        
                        customFunctions: {
                            showRoles: function(roles) {
                                var ids = roles.select('roleId');
                                
                                Repository.Get('Role', ids, function(result) {
                                    this.rolesLabel.set_text(result.select('name').join(' ,'));
                                }.bind(this));
                            }
                        },
                        
                        bindings: {
                            '*' : function(sender, args) {
                                var value = args.newValue;
                                this.showRoles(value);
                            },
                            
                            '*.changed': function(sender, args) {
                                this.showRoles(sender);
                            }
                        }
                    }
                },
                custom1: {
                    headerText: 'Custom',
                    readOnly: true,
                    
                    custom: true,
                    
                    template: {
                        type: 'panel',
                        width: '100%',
                        height: '100%',
                        
                        controls: [
                            {
                                type: 'label',
                                id: 'textLabel',
                                
                                width: '100%'
                            }
                        ],
                        
                        bindings: {
                            '*' : function(sender, args) {
                                var value = args.newValue;
                                this.textLabel.set_text('id: ' + value._id + ' name: ' + value._name);
                            }
                        }
                    }
                }
            },
            newItemResolver: function() {
                return new User();
            },
            handlers: {
                newItemCreated: function(sender, item) {
                    sender.get_dataSource().add(item);
                }
            }
        }
    ],
    onLoad: function() {
        var ds = [].makeDataSource('User');
        
        this.testGrid.set_dataSource(ds);
        ds.load();
    }
})