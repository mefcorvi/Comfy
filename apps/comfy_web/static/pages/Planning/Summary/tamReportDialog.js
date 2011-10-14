({

    cssClass: 'report_handset_selection',

    data: [
        { name: 'handsets', value: [].makeSorted('fullName') },
        { name: 'selectedHandsets', value: [].makeSorted('code') },
        { name: 'handsetsDS', value: [].makeDataSource('Handset').filterBy('').orderBy('Name') },
        { name: 'countriesDS', value: [].makeDataSource('Country').filterBy('').orderBy('name') },
        { name: 'operatorsDS', value: [].makeDataSource('Operator').filterBy('IsArchived = false AND this isnot SubsidiaryOperator').orderBy('name') },
        { name: 'reportName' },
        { name: 'activeView', value: 'allHandsets' },
        { name: 'selectedOperator', value: null },
        { name: 'selectedCountry', value: null }
    ],

    onLoad: function () {
        this.get_data().get_handsetsDS().load(function (res) { this.get_data().get_handsets().synchronize(res); } .bind(this));        
        this.get_data().set_reportName('projectSchedule');
    },

    customFunctions : {
        _buildFilterDto: function() {
            var country = this.get_data().get_selectedCountry();
            var operator = this.get_data().get_selectedOperator();
            var handsets = this.get_data().get_selectedHandsets();
                            
            var filterDto = {
                Clauses: [
                    { 
                        CountryId: country != null ? country.get_id() : null ,
                        OperatorId: operator != null ? operator.get_id() : null,
                        HandsetIds: handsets.select('id')
                    }
                ]                
            };

            return filterDto;
        }
    },

    controls: [
        {
            id: 'container',
            type: 'panel',
            orientation: 'horizontal',

            controls: [
                {
                    type: 'panel',
                    layout: 'stack',
                    orientation: 'vertical',
                    showCollapseButton: false,
                    margin: '0 0 3 0',
                    cssClass: 'filter_blocks',                    

                    controls: [
                        {
                            type: 'collapsablePanel',
                            title: 'Selected handsets',
                            showCollapseButton: false,
                            margin: '0 0 0 3',
                            blockName: 'allHandsets',
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
                                                        if (this.get_data().get_activeView() != 'allHandsets')
                                                            return;

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
                            ],
                            bindings: {
                                'data:activeView': function(sender, args) {                                    
                                    if (args.newValue == this.options.blockName) {
                                        this.addCssClass('block_selected');
                                    } else if (args.oldValue == this.options.blockName) {
                                        this.removeCssClass('block_selected');
                                    }
                                }
                            },
                            domHandlers: {
                                'click': function (sender, args) {                         
                                    this.get_data().set_activeView(this.options.blockName);                                    
                                },
                                'mouseover': function() {
                                    this.addCssClass('block_hovered');
                                },
                                'mouseout': function() {
                                    this.removeCssClass('block_hovered');
                                }
                            }     
                        },
                        {
                            type: 'collapsablePanel',
                            title: 'Operator',       
                            height: '70',
                            margin: '0 0 0 3',
                            showCollapseButton: false,
                            blockName: 'operators',
                            controls: [
                                {
                                    type: 'panel',
                                    height: '40',
                                    padding: '5',
                                    bindings: {
                                        'data:selectedOperator': function(sender, args) {
                                            this.set_dataSource(args.newValue);
                                            this.set_visible(!!args.newValue);
                                        },
                                        'name': 'name',
                                        'imageId': 'image'
                                    },                                    

                                    orientation: 'horizontal',

                                    controls: [
                                        {
                                            id: 'image',
                                            type: 'entityImage',                                                        
                                            valign: 'middle',
                                            margin: '0 0 5 0',
                                            mode: 'bounded',
                                            width: 50,
                                            height: 30
                                        },
                                        {
                                            type: 'label',
                                            id: 'name',
                                            width: '*',
                                            height: '18',
                                            valign: 'middle'
                                        },
                                        {
                                            type: 'link',
                                            cssClass: 'delete_button',
                                            width: '16',
                                            height: '16',
                                            valign: 'middle',     
                                            domHandlers: {
                                                'click': function(sender, args) {
                                                    args.stopPropagation();
                                                    this.get_data().set_selectedOperator(null);
                                                }
                                            }                                                                                   
                                        }
                                    ]
                                }
                            ],
                            bindings: {
                                'data:activeView': function(sender, args) {                                    
                                    if (args.newValue == this.options.blockName) {
                                        this.addCssClass('block_selected');
                                    } else if (args.oldValue == this.options.blockName) {
                                        this.removeCssClass('block_selected');
                                    }
                                }
                            },
                            domHandlers: {
                                'click': function (sender, args) {                                    
                                    this.get_data().set_activeView('operators');
                                },
                                'mouseover': function() {
                                    this.addCssClass('block_hovered');
                                },
                                'mouseout': function() {
                                    this.removeCssClass('block_hovered');
                                }
                            }                        
                        },
                        {
                            type: 'collapsablePanel',
                            title: 'Country',
                            height: '70',
                            showCollapseButton: false,
                            blockName: 'countries',
                            controls: [
                                {
                                    type: 'panel',
                                    height: '40',
                                    padding: '5',
                                    bindings: {
                                        'data:selectedCountry': function(sender, args) {
                                            this.set_dataSource(args.newValue);
                                            this.set_visible(!!args.newValue);
                                        },
                                        'name': 'name',
                                        'imageId': 'image'
                                    },                                    

                                    orientation: 'horizontal',

                                    controls: [
                                        {
                                            id: 'image',
                                            type: 'entityImage',                                                        
                                            valign: 'middle',
                                            margin: '0 0 5 0',
                                            mode: 'bounded',
                                            width: 50,
                                            height: 30
                                        },
                                        {
                                            type: 'label',
                                            id: 'name',
                                            width: '*',
                                            height: '18',
                                            valign: 'middle'
                                        },
                                        {
                                            type: 'link',
                                            cssClass: 'delete_button',
                                            width: '16',
                                            height: '16',
                                            valign: 'middle',     
                                            domHandlers: {
                                                'click': function(sender, args) {
                                                    args.stopPropagation();
                                                    this.get_data().set_selectedCountry(null);
                                                }
                                            }                                                                                   
                                        }
                                    ]
                                }
                            ],
                            bindings: {
                                'data:activeView': function(sender, args) {                                    
                                    if (args.newValue == this.options.blockName) {
                                        this.addCssClass('block_selected');
                                    } else if (args.oldValue == this.options.blockName) {
                                        this.removeCssClass('block_selected');
                                    }
                                }
                            },
                            domHandlers: {
                                'click': function (sender, args) {                                    
                                    this.get_data().set_activeView('countries');
                                },
                                'mouseover': function() {
                                    this.addCssClass('block_hovered');
                                },
                                'mouseout': function() {
                                    this.removeCssClass('block_hovered');
                                }
                            }                        
                        }
                    ]
                },
                {
                    type: 'multiView',
                    bindings: {
                        'data:activeView': function(sender, args) {
                            this.set_activeView(args.newValue);
                        }
                    },
                    views: [
                        {
                            type: 'collapsablePanel',
                            id: 'allHandsets',
                            title: 'All handsets',
                            layout: 'stack',
                            orientation: 'vertical',
                            showCollapseButton: false,

                            controls: [
                                {
                                    type: 'scrollablePanel',
                                    controls: [
                                        {
                                            type: 'repeater',
                                            cssClass: 'handset_repeater',
                                            layout: 'wrap',
                                            orientation: 'horizontal',
                                            width: '*',
                                            height: '?',
                                            emptyDataText: 'No handsets found',
                                            template: {
                                                type: 'panel',
                                                height: '40',
                                                width: '49%',
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
                                                        margin: '0 0 5 0',
                                                        valign: 'middle',
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
                        },
                        {
                            type: 'collapsablePanel',
                            id: 'operators',
                            title: 'Operators',
                            showCollapseButton: false,
                            controls: [
                                {
                                    type: 'scrollablePanel',
                                    controls: [
                                        {
                                            type: 'repeater',
                                            cssClass: 'handset_repeater',
                                            layout: 'wrap',
                                            orientation: 'horizontal',
                                            width: '*',
                                            height: '?',
                                            template: {
                                                type: 'panel',
                                                height: '40',
                                                width: '49%',
                                                padding: '5',
                                                bindings: {
                                                    'name': 'name',
                                                    'imageId': 'image'
                                                },

                                                cssClass: 'handset_row',
                                                border: '0 0 0 1',

                                                domHandlers: {
                                                    'click': function () {
                                                        var clickedOperator = this.get_dataSource();
                                                        this.get_data().set_selectedOperator(clickedOperator);
                                                    }
                                                },

                                                orientation: 'horizontal',

                                                controls: [
                                                    {
                                                        id: 'image',
                                                        type: 'entityImage',                                                        
                                                        valign: 'middle',
                                                        margin: '0 0 5 0',
                                                        mode: 'bounded',
                                                        width: 50,
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
                                                var ds = this.get_data().get_operatorsDS();

                                                this.set_dataSource(ds);

                                                if (!ds.isLoaded())                                       
                                                    ds.load();
                                            }
                                        }
                                    ]
                                },
                                {
                                    type: 'searchPane',
                                    onSearch: function (sender, args) {
                                        var filterQuery = args.query || '';

                                        var ds = this.get_data().get_operatorsDS();
                                        ds.set_searchQuery(filterQuery, true);
                                    }
                                }
                            ]
                        },
                        {
                            type: 'collapsablePanel',
                            id: 'countries',
                            title: 'Countries',
                            showCollapseButton: false,
                            controls: [
                                {
                                    type: 'scrollablePanel',
                                    controls: [
                                        {
                                            type: 'repeater',
                                            cssClass: 'handset_repeater',
                                            layout: 'wrap',
                                            orientation: 'horizontal',
                                            width: '*',
                                            height: '?',
                                            template: {
                                                type: 'panel',
                                                height: '30',
                                                width: '49%',
                                                padding: '5',
                                                bindings: {
                                                    'name': 'name',
                                                    'imageId': 'image'
                                                },

                                                cssClass: 'handset_row',
                                                border: '0 0 0 1',

                                                domHandlers: {
                                                    'click': function () {
                                                        var clickedCountry = this.get_dataSource();
                                                        this.get_data().set_selectedCountry(clickedCountry);
                                                    }
                                                },

                                                orientation: 'horizontal',

                                                controls: [
                                                    {
                                                        id: 'image',
                                                        type: 'entityImage',                                                        
                                                        valign: 'middle',
                                                        margin: '0 0 5 0',
                                                        mode: 'bounded',
                                                        width: 40,
                                                        height: 20
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
                                                var ds = this.get_data().get_countriesDS();

                                                this.set_dataSource(ds);

                                                if (!ds.isLoaded())                                       
                                                    ds.load();
                                            }
                                        }
                                    ]
                                },
                                {
                                    type: 'searchPane',
                                    onSearch: function (sender, args) {
                                        var filterQuery = args.query || '';

                                        var ds = this.get_data().get_countriesDS();
                                        ds.set_searchQuery(filterQuery, true);
                                    }
                                }
                            ]
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
                    type: 'label',
                    text: 'Apply filtering to:',
                    valign: 'middle'
                },
                {
                    type: 'button',
                    text: 'All projects',

                    domHandlers: {
                        'mousedown': function () {
                            var filterDto = this.get_window()._buildFilterDto();
                            filterDto.CurrentProjects = false;

                            var filter = $.toJSON(filterDto);
                            this.domElement.href = Services.ExportService.GetUrl(this.get_data().get_reportName(), filter, null, null);
                        }
                    },

                    onClick: function () {
                        var wnd = this.get_window().get_window();
                        wnd.close.delay(0, wnd);
                    }
                },
                {
                    type: 'button',
                    text: 'Current projects',
                    domHandlers: {
                        'mousedown': function () {                                                        
                            var filterDto = this.get_window()._buildFilterDto();
                            filterDto.CurrentProjects = true;                            

                            var filter = $.toJSON(filterDto);
                            this.domElement.href = Services.ExportService.GetUrl(this.get_data().get_reportName(), filter, null, null);
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