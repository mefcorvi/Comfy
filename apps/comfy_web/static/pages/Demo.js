({
    resources: {
        'tree': {
            childProperty: 'childs',
            textProperty: 'title',
            nodeTemplate: {
                layout: 'grid',
                width: '*',
                columns: [ '*', '*' ],
                rows: ['?'],
                orientation: 'horizontal',
                bindings: {
                    'title': 'lbl1',
                    'blabla': 'lbl2'
                },
                controls: [
                    { id: 'lbl1', type: 'label', 'grid.column': 1, 'grid.row': 1 },
                    { id: 'lbl2', type: 'label', 'grid.column': 2, 'grid.row': 1 }
                ]
            },
            onLoad: function() {
                var ds = [
                    {
                        title: 'Node 1',
                        blabla: 'Bla',
                        childs: [
                            { title: 'Node 1.1', blabla: 'Bla 2' },
                            {
                                title: 'Node 1.2',
                                childs: [
                                    { title: 'Node 1.2.1' },
                                    { title: 'Node 1.2.2' }
                                ].makeObservable()
                            },
                            { title: 'Node 1.3' }
                        ].makeObservable()
                    },
                    {
                        title: 'Node 2',
                        blabla: 'Bla',
                        childs: [
                            { title: 'Node 2.1' },
                            { title: 'Node 2.2' }
                        ].makeObservable()
                    }
                ].makeObservable();
 
                this.set_dataSource(ds);
            }
        }
    },
    controls: [
        {
            type: 'tree',
            resource: 'page:tree'
        },
        {
            type: 'tree',
            resource: 'page:tree'
        }
    ]
})

