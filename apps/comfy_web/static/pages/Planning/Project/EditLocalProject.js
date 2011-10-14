({
    title: 'Edit local project',
    height: '?',
    data: ['departmentId', 'description'],
    controls: [
        {
            type: 'form',
            width: '*',
            height: '?',
            padding: '5',
            controls: [
                {
                    id: 'department',
                    type: 'dropDownList',
                    textProperty: 'name',
                    'form.label': 'R&D:',
                    valueProperty: 'id',
                    bindings: {
                        'data:departmentId': 'selectedValue'
                    },
                    notSelectedItem: {
                        name: '-'
                    },
                    onLoad: function (sender, args) {
                        var ds = [].makeDataSource('Department').filterBy('isArchived = false');
                        ds.load();
                        this.set_items(ds);
                    }
                },
                {
                    id: 'description',
                    type: 'textbox',
                    mode: 'multiline',
                    height: '43',
                    bindings: {
                        'data:description': 'text'
                    },
                    'form.label': 'Description'
                }
            ]
        }
    ]
})