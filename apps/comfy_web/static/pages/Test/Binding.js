({
    title: 'Binding',
    controls: [
        {
            type: 'panel',
            width: '600',
            height: '600',
            
            id: 'controlsPanel',
            layout: 'stack',
            orientation: 'vertical',
            
            controls: [
                {
                    type: 'label',
                    width: '100',
                    height: '20',
                    id: 'numberLabel',
                    bindings: { '*': 'text' }
                },
                {
                    type: 'label',
                    width: '100',
                    height: '20',
                    text: '- Complex:'
                },
                {
                    type: 'panel',
                    id: 'complexPanel',
                    width: '80%',
                    height: '300',
            
                    layout: 'stack',
                    orientation: 'vertical',
                    
                    controls: [
                        {
                            type: 'label',
                            id: 'innerNumber',
                            
                            width: '80%',
                            height: '20',
                            text: 'empty',
                            
                            bindings: { '*': 'text' }
                        },
                        {
                            type: 'repeater',
                            id: 'repeater',
                            
                            width: '80%',
                            height: '200',
                            layout: 'stack',
                            orientation: 'vertical',                            
                            
                            template: {
                                type: 'label',
                                width: '60',
                                bindings: {
                                    '*': 'text'
                                }
                            }
                        }
                    ],
                    bindings: { 
                        'number': 'innerNumber',
                        'items': 'repeater',
                        'items.changed' : function(sender, args)  {
                            alert('added: ' + args.added.join(',') + '\nremoved: ' + args.removed.join(','));
                        }
                    }
                }
            ],
            
            bindings: {
                'number' : 'numberLabel',
                'complex' : 'complexPanel'
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'number',
            
            onClick: function() {
                this.get_window().get_dataSource().set_number(Math.random() * 100);
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'complex',
            
            onClick: function() {
                var complex = {};
                Auto.Property(complex, { name: 'number', autoEvent: true });
                Auto.Property(complex, { name: 'items', autoEvent: true });
                
                complex.set_number(0);
                complex.set_items(['a', 'b', 'c'].makeObservable());   
            
                this.get_window().get_dataSource().set_complex(complex);
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'complex.number',
            
            onClick: function() {         
                this.get_window().get_dataSource().get_complex().set_number(Math.random() * 100);
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'complex.items',
            
            onClick: function() {
                var items = [];
                for(var i = 0; i < 3; i ++)
                    items.add(Math.round((Math.random() * 100)));                                    
                
                this.get_window().get_dataSource().get_complex().set_items(items.makeObservable());
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'complex.items.add',
            
            onClick: function() {
                this.get_window().get_dataSource().get_complex().get_items().add(Math.round((Math.random() * 100)));
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'complex.items.remove',
            
            onClick: function() {
                var items = this.get_window().get_dataSource().get_complex().get_items();
                if(items.length > 0)
                    items.remove(items.last());
            }
        },
        {
            type: 'button',
            width: '120',
            
            text: 'Set new item',
            
            onClick: function() {
                var data = {};
                
                Auto.Property(data, { name: 'number', autoEvent: true });
                Auto.Property(data, { name: 'complex', autoEvent: true });
                
                var complex = {};
                Auto.Property(complex, { name: 'number', autoEvent: true });
                Auto.Property(complex, { name: 'items', autoEvent: true });
                
                complex.set_number(0);
                complex.set_items(['a', 'b', 'c'].makeObservable());   
                
                data.set_number(1);
                data.set_complex(complex);

                this.get_window().set_dataSource(data);
            }
        }
    ],
    bindings: {
        '*': 'controlsPanel',
        'number': function(sender, args) {
            var value = args.newValue;
            alert(value);
        }
    },
    
    onLoad: function(sender, args) {
        var data = {};
        
        Auto.Property(data, { name: 'number', autoEvent: true });
        Auto.Property(data, { name: 'complex', autoEvent: true });
        
        var complex = {};
        Auto.Property(complex, { name: 'number', autoEvent: true });
        Auto.Property(complex, { name: 'items', autoEvent: true });
        
        complex.set_number(0);
        complex.set_items(['a', 'b', 'c'].makeObservable());        
        
        data.set_number(1);
        data.set_complex(complex);

        this.set_dataSource(data);
    }
})