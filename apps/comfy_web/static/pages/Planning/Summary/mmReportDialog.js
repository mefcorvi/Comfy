({

    cssClass: 'report_handset_selection',

    data: [
        { name: 'handsets', value: [].makeSorted('fullName') },
        { name: 'selectedHandsets', value: [].makeSorted('fullName') },
        { name: 'currentFilter' },
        { name: 'currentSearchQuery' },
        { name: 'handsetsDS' },
        { name: 'reportName' }
    ],

    onLoad: function () {
        var ds = [].makeDataSource('Handset').filterBy('').orderBy('Name');
        ds.load(function (res) { this.get_data().get_handsets().synchronize(res); } .bind(this));

        this.get_data().set_handsetsDS(ds);
        this.get_data().set_currentFilter(this.get_param('filter'));
        this.get_data().set_currentSearchQuery(this.get_param('searchQuery'));
        this.get_data().set_reportName('summaryWeek');
    },

    controls: [
        {
            id: 'container',
            type: 'panel',
            orientation: 'horizontal',

            controls: [                
                {
                    type: 'collapsablePanel',
                    title: 'Selected handsets',
                    layout: 'stack',
                    orientation: 'vertical',
                    margin: '0 0 3 0',
                    showCollapseButton: false,

                    controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    type: 'repeater',
                                    cssClass: 'handset_repeater',
                                    layout: 'stack',
                                    width: '*',
                                    height: '?',
                                    orientation: 'vertical',
                                    emptyDataText: 'No handsets selected',
                                    template: {
                                        type: 'panel',
                                        height: '40',
                                        padding: '5',
                                        bindings: {
                                            'fullName': 'name',
                                            'imageId': 'image'
                                        },

                                        cssClass: 'handset_row',
                                        border: '0 0 0 1',

                                        domHandlers: {
                                            'click': function () {
                                                var data = this.get_data();

                                                var clickedHandset = this.get_dataSource();
                                                data.get_selectedHandsets().remove(clickedHandset);                                                        
                                                data.get_handsets().add(clickedHandset);
                                            }
                                        },

                                        orientation: 'horizontal',

                                        controls: [
                                            {
                                                id: 'image',
                                                type: 'entityImage',
                                                mode: 'exactly',
                                                valign: 'middle',
                                                margin: '0 0 5 0',
                                                width: 20,
                                                height: 30
                                            },
                                            {
                                                type: 'label',
                                                id: 'name',
                                                width: '*',
                                                height: '18',
                                                valign: 'middle'
                                            }
                                        ]
                                    },

                                    onLoad: function () {
                                        this.set_dataSource(this.get_window().get_data().get_selectedHandsets());
                                    }
                                }
                            ]
                        }
                    ]
                },
                {
                    type: 'collapsablePanel',
                    id: 'allHandsets',
                    title: 'All handsets',
                    layout: 'stack',
                    orientation: 'vertical',
                    margin: '0 0 0 0',
                    showCollapseButton: false,

                    controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    type: 'repeater',
                                    cssClass: 'handset_repeater',
                                    layout: 'stack',
                                    width: '*',
                                    height: '?',
                                    orientation: 'vertical',
                                    emptyDataText: 'No handsets found',
                                    template: {
                                        type: 'panel',
                                        height: '40',
                                        padding: '5',
                                        bindings: {
                                            'fullName': 'name',
                                            'imageId': 'image'
                                        },

                                        cssClass: 'handset_row',
                                        border: '0 0 0 1',

                                        domHandlers: {
                                            'click': function () {
                                                var clickedHandset = this.get_dataSource();
                                                if (!this.get_data().get_selectedHandsets().contains(clickedHandset)) {
                                                    this.get_data().get_selectedHandsets().add(clickedHandset);
                                                    this.get_data().get_handsets().remove(clickedHandset);
                                                }
                                            }
                                        },

                                        orientation: 'horizontal',

                                        controls: [
                                            {
                                                id: 'image',
                                                type: 'entityImage',
                                                mode: 'exactly',
                                                valign: 'middle',
                                                margin: '0 0 5 0',
                                                width: 20,
                                                height: 30
                                            },
                                            {
                                                type: 'label',
                                                id: 'name',
                                                width: '*',
                                                height: '18',
                                                valign: 'middle'
                                            }
                                        ]
                                    },

                                    onLoad: function () {
                                        this.set_dataSource(this.get_window().get_data().get_handsets());
                                    }
                                }
                            ]
                        },
                        {
                            type: 'searchPane',
                            onSearch: function (sender, args) {
                                var filterQuery = args.query || '';

                                var ds = this.get_data().get_handsetsDS();
                                ds.set_searchQuery(filterQuery);
                                ds.load(function (res) {
                                    this.get_data().get_handsets().synchronize(res);
                                    this.get_data().get_handsets().exclude(this.get_data().get_selectedHandsets());
                                } .bind(this));
                            }
                        }
                    ]
                }
            ]
        },
        {
            type: 'panel',
            height: '?',
            width: '?',
            padding: '3',
            halign: 'right',
            orientation: 'horizontal',
            controls: [
                {
                    type: 'button',
                    text: 'Use selected handsets',

                    domHandlers: {
                        'mousedown': function () {
                            if (this.get_data().get_selectedHandsets().length == 0) {
                                Application.showError('Error', 'Please select at least one handset');
                                return;
                            }

                            var handsets = this.get_data().get_selectedHandsets().select('id').join(',');
                            this.domElement.href = Services.ExportService.GetUrl(this.get_data().get_reportName(), null, null, handsets);
                        }
                    },

                    onClick: function () {
                        if (this.get_data().get_selectedHandsets().length != 0) {
                            var wnd = this.get_window().get_window();
                            wnd.close.delay(0, wnd);
                        }
                    }
                },
                {
                    type: 'button',
                    text: 'Use current filter',
                    domHandlers: {
                        'mousedown': function () {
                            var data = this.get_window().get_data();
                            var filter = data.get_currentFilter();
                            var query = data.get_currentSearchQuery();
                            this.domElement.href = Services.ExportService.GetUrl(this.get_data().get_reportName(), filter, query, null)
                        }
                    },

                    onClick: function () {
                        var wnd = this.get_window().get_window();
                        wnd.close.delay(0, wnd);
                    }
                }
            ]
        }
    ]
})