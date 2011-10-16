({
    title: "Geography",
    layout: 'stack',
    orientation: 'vertical',
    valign: 'stretch',
    cssClass: 'geography_page',

    data: [
        'countriesFilter',
        'regionsFilter',
        'editedCountry',
        'editedRegion',
        'countryDataSource',
        'regionDataSource'
    ],

    onLoad: function () { },

    controls: [
        {
            type: 'popup',
            width: '300',
            height: '320',
            title: 'Edit Country',
            buttons: ['Save', 'Cancel'],
            controls: [
                {
                    id: 'countryEdit',
                    type: 'countryEdit',
                    width: '100%',
                    height: '100%'
                }
            ],
            onOpen: function () {
                var country = this.get_data().get_editedCountry();

                if (country && !country.isNew()) {
                    this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                    this.set_title('Edit country: ' + country.get_name());
                } else {
                    this.set_buttons(['Save', 'Cancel']);
                    this.set_title('New country');
                }
            },
            onLoad: function () {
                this.get_data().add_editedCountryChanged(function (sender, args) {
                    if (args.newValue) {
                        this.container.countryEdit.set_dataSource(args.newValue);
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onCommand: function (sender, args) {
                var countryEdit = this.container.countryEdit;
                var entity = countryEdit.get_dataSource();

                if (args.button == 'Save') {
                    countryEdit.save(function () {
                        this.get_data().set_editedCountry(null);
                        this.get_data().countryDataSource.load();
                    } .bind(this));
                    return;
                }

                if (args.button == 'Cancel' && !entity.isNew()) {
                    countryEdit.cancel();
                }

                if (args.button == 'Delete' && !entity.isNew()) {
                    Application.showConfirm('Warning', 'Do you really want to delete this country?', function (result) {
                        if (result) {
                            Repository.Delete(entity);
                            this.get_data().set_editedCountry(null);
                        }
                    } .bind(this));

                    return;
                }

                this.get_data().set_editedCountry(null);
            }
        },
        {
            type: 'popup',
            width: '300',
            height: '150',
            title: 'Edit Region',
            buttons: ['Save', 'Cancel'],
            controls: [
                {
                    id: 'regionEdit',
                    type: 'regionEdit',
                    width: '100%',
                    height: '100%'
                }
            ],
            onOpen: function () {
                var region = this.get_data().get_editedRegion();

                if (region && !region.isNew()) {
                    this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                    this.set_title('Edit region: ' + region.get_name());
                } else {
                    this.set_buttons(['Save', 'Cancel']);
                    this.set_title('New region');
                }
            },
            onLoad: function () {
                this.get_data().add_editedRegionChanged(function (sender, args) {
                    if (args.newValue) {
                        this.container.regionEdit.set_dataSource(args.newValue);
                        this.open();
                    } else {
                        this.close();
                    }
                }, this);
            },
            onCommand: function (sender, args) {
                var entity = this.container.regionEdit.get_dataSource();

                if (args.button == 'Save') {
                    Repository.Save(entity, function () {
                        this.get_data().set_editedRegion(null);
                        this.get_data().regionDataSource.load();
                    } .bind(this));
                    return;
                }

                if (args.button == 'Cancel' && !entity.isNew()) {
                    Repository.Reload(entity);
                }

                if (args.button == 'Delete' && !entity.isNew()) {
                    Application.showConfirm('Warining', 'Do you really want to delete this region?', function (result) {
                        if (result) {
                            Repository.Delete(entity);
                            this.get_data().set_editedRegion(null);
                        }
                    } .bind(this));

                    return;
                }

                this.get_data().set_editedRegion(null);
            }
        },
        {
            type: "pageHeader",
            title: "Geograpgy",
            description: "Some description"
        },
        {
            type: 'panel',
            layout: 'stack',
            width: '100%',
            orientation: 'horizontal',
            controls: [
                { //++ Countries
                    type: 'collapsablePanel',
                    title: 'Countries',
                    width: '3*',
                    height: '100%',
                    margin: '0 0 3 0',
                    showCollapseButton: false,
                    buttons: ['Add country'],
                    showStatusBar: true,
                    cssClass: 'countries_block',
                    onCommand: function (sender, args) {
                        if (args.button == 'Add country') {
                            this.get_data().set_editedCountry(new Country());
                        }
                    },
                    controls: [
                        {
                            type: 'scrollablePanel',
                            horizontal: true,
                            vertical: false,
                            controls: [
                                {
                                    type: 'repeater',
                                    layout: 'wrap',
                                    padding: '5',
                                    width: '?',
                                    height: '100%',
                                    orientation: 'vertical',
                                    emptyDataText: 'Countries have been not found',
                                    template: {
                                        type: 'panel',
                                        width: '300px',
                                        height: '25px',
                                        border: '1',
                                        padding: '3',
                                        orientation: 'horizontal',
                                        cssClass: 'country_item',
                                        domHandlers: {
                                            click: function () {
                                                this.get_data().set_editedCountry(this.get_dataSource());
                                            },
                                            mouseover: function () {
                                                this.addCssClass('country_item_hover');
                                            },
                                            mouseout: function () {
                                                this.removeCssClass('country_item_hover');
                                            }
                                        },
                                        bindings: {
                                            'name': 'countryName',
                                            'imageId': 'image',
                                            'code': 'countryCode',
                                            'regionId': function (sender, args) {
                                                var value = args.newValue;
                                                Repository.Get('Region', value, function (result) {
                                                    this.countryRegion.set_dataSource(result);
                                                } .bind(this));
                                            }
                                        },
                                        controls: [
                                            {
                                                id: 'countryRegion',
                                                type: 'label',
                                                width: '80',
                                                height: '18',
                                                cssClass: 'country_region',
                                                margin: '0 0 5 0',
                                                bindings: {
                                                    'name': 'text'
                                                }
                                            },
                                            {
                                                id: 'image',
                                                type: 'entityImage',
                                                mode: 'exactly',
                                                width: 25,
                                                height: 15
                                            },
                                            {
                                                id: 'countryCode',
                                                type: 'label',
                                                width: '35px',
                                                cssClass: 'country_code',
                                                margin: '5 0'
                                            },
                                            {
                                                id: 'countryName',
                                                type: 'label',
                                                width: '*',
                                                cssClass: 'country_name',
                                                text: ''
                                            }
                                        ]
                                    },
                                    customFunctions: {
                                        'filterChanged': function (sender, args) {
                                            this.get_dataSource().set_searchQuery(args.newValue, true);
                                        }
                                    },
                                    onFree: function () {
                                        this.get_data().remove_countriesFilterChanged(this.filterChanged, this);
                                        this.get_dataSource().dispose();
                                    },
                                    onLoad: function () {
                                        var ds = [].makeDataSource('Country').orderBy('name asc').filterBy('');
                                        this.set_dataSource(ds);
                                        ds.load();
                                        this.get_data().countryDataSource = ds;
                                        this.get_data().add_countriesFilterChanged(this.filterChanged, this);
                                    }
                                }
                            ]
                        },
                        {
                            type: 'searchPane',
                            onSearch: function (sender, args) {                            
                                this.get_data().set_countriesFilter(args.query);
                            }
                        }
                    ]
                }, //-- Countries
                { //++ Regions
                type: 'collapsablePanel',
                height: '100%',
                width: '*',
                buttons: ['Add region'],
                showStatusBar: true,
                showCollapseButton: false,
                title: 'Regions',
                cssClass: 'regions_block',
                onCommand: function (sender, args) {
                    if (args.button == 'Add region') {
                        this.get_data().set_editedRegion(new Region());
                    }
                },
                controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    type: 'repeater',
                                    width: '100%',
                                    emptyDataText: 'Regions have been not found',
                                    template: {
                                        type: 'panel',
                                        height: '30px',
                                        border: '1',
                                        cssClass: 'region_item',
                                        padding: '7',
                                        orientation: 'horizontal',
                                        domHandlers: {
                                            click: function () {
                                                this.get_data().set_editedRegion(this.get_dataSource());
                                            },
                                            mouseover: function () {
                                                this.addCssClass('region_item_hover');
                                            },
                                            mouseout: function () {
                                                this.removeCssClass('region_item_hover');
                                            }
                                        },
                                        bindings: {
                                            'name': 'name',
                                            'code': 'code'
                                        },
                                        controls: [
                                            {
                                                id: 'name',
                                                type: 'label',
                                                cssClass: 'region_name',
                                                width: '*'
                                            },
                                            {
                                                id: 'code',
                                                type: 'label',
                                                cssClass: 'region_code',
                                                width: '25'
                                            }
                                        ]
                                    },
                                    customFunctions: {
                                        'filterChanged': function (sender, args) {
                                            this.get_dataSource().set_searchQuery(args.newValue, true);
                                        }
                                    },
                                    onFree: function () {
                                        this.get_dataSource().dispose();
                                        this.get_data().remove_regionsFilterChanged(this.filterChanged, this);
                                    },
                                    onLoad: function () {
                                        this.get_data().add_regionsFilterChanged(this.filterChanged, this);
                                        var ds = [].makeDataSource('Region').orderBy('name').filterBy('');
                                        this.set_dataSource(ds);
                                        ds.load();
                                        this.get_data().regionDataSource = ds;
                                    }
                                }
                            ]
                        },
                        {
                            type: 'searchPane',
                            onSearch: function (sender, args) {
                                this.get_data().set_regionsFilter(args.query);
                            }
                        }
                    ]
            }
            ]
        }
    ]
})