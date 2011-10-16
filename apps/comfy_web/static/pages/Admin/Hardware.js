({
    title: "Hardware",
    layout: 'stack',
    orientation: 'vertical',
    valign: 'stretch',
    cssClass: 'hardware_page',

    customFunctions: {
        'newHandset': function() {
            var handset = new Handset();
            handset.set_releaseDate(new Date());
            this.get_data().set_editedHandset(handset);
        }
    },
    
    data: [
        'editedHandset',
        'handsetFilter'
    ],

    onLoad: function() {},

    controls: [
        // #region Edit Handset
        {
            type: 'popup',
            width: '300',
            height: '500',
            title: 'Edit Handset',
            buttons: ['Save', 'Cancel'],
            controls: [
                {
                    id: 'handsetEdit',
                    type: 'handsetEdit',
                    width: '100%',
                    height: '100%'
                }
            ],
            onOpen: function() {
                var handset = this.get_data().get_editedHandset();
                
                if (handset && !handset.isNew()) {
                    this.set_buttons(['Save', 'Cancel', { text: 'Delete', cssClass: 'link_button red_button'}]);
                } else {
                    this.set_buttons(['Save', 'Cancel']);
                }
            },
            onLoad: function() {
                this.get_data().add_editedHandsetChanged(function(sender, args) {
                    if (args.newValue) {
                        this.container.handsetEdit.set_dataSource(args.newValue);
                        this.open();
                    } else {
                        this.container.handsetEdit.set_dataSource(null);
                        this.close();
                    }
                }, this);
            },
            onCommand: function(sender, args) {
                var entity = this.container.handsetEdit.get_dataSource();
            
                if (args.button == 'Save') {
                    this.container.handsetEdit.save(function() {
                        this.get_data().set_editedHandset(null);
                    }.bind(this));
                    return;
                }
                
                if (args.button == 'Cancel' && !entity.isNew()) {
                    this.container.handsetEdit.cancel();
                }
                
                if (args.button == 'Delete' && !entity.isNew()) {
                    Application.showConfirm('Warning', 'Do you really want to delete this handset?', function(result) {
                        if (result) {
                            Repository.Delete(entity);
                            this.get_data().set_editedHandset(null);
                        }
                    }.bind(this));
                    
                    return;
                }
                
                this.get_data().set_editedHandset(null);
            }
        },
        // #endregion
        {
            type: "pageHeader",
            title: "Hardware",
            description: "Some handware management description"
        },
        {
            type: 'panel',
            orientation: 'horizontal',
            controls: [
                // #region Handsets
                {
                    type: 'collapsablePanel',
                    title: 'Handsets',
                    showCollapseButton: false,
                    showStatusBar: true,
                    buttons: [ 'Add' ],
                    cssClass: 'handsets_block',
                    onCommand: function(sender, args) {
                        if (args.button == 'Add') {
                            this.get_window().newHandset();
                        }
                    },
                    controls: [
                        {
                            type: 'scrollablePanel',
                            controls: [
                                {
                                    type: 'repeater',
                                    width: '100%',
                                    height: '?',
                                    layout: 'wrap',
                                    orientation: 'horizontal',
                                    template: {
                                        type: 'panel',
                                        width: '110',
                                        height: '210',
                                        padding: '5',
                                        cssClass: 'handset_item',
                                        bindings: {
                                            'name': 'name',
                                            'imageId': 'imageId',
                                            'code': 'code',
                                            'releaseDate': 'releaseDate',
                                            'class': 'handsetClass'
                                        },
                                        domHandlers: {
                                            'click': function() {
                                                this.get_data().set_editedHandset(this.get_dataSource());
                                            },
                                            'mouseover': function() {
                                                this.addCssClass('handset_item_hover');
                                            },
                                            'mouseout': function() {
                                                this.removeCssClass('handset_item_hover');
                                            }
                                        },
                                        controls: [
                                            { id: 'imageId', type: 'entityImage', width: 80, height: 120, mode: 'bounded', halign: 'center' },
                                            { id: 'handsetClass', cssClass: 'class', type: 'label', text: '', width: '20', height: '16', halign: 'center', margin: '0 -8 0 0' },
                                            { id: 'name', cssClass: 'name', type: 'label', width: '*', text: '', height: '?', padding: '2px' },
                                            { id: 'code', cssClass: 'code', type: 'label', width: '*', text: '' },
                                            { id: 'releaseDate', cssClass: 'year', type: 'label', width: '*', text: '', format: '{0:d}' }
                                        ]
                                    },
                                    customFunctions: {
                                        'filterChanged': function(sender, args) {
                                            this.get_dataSource().set_searchQuery(args.newValue, true);
                                        }
                                    },
                                    onFree: function() {
                                        this.get_data().remove_handsetFilterChanged(this.filterChanged, this);
                                    },
                                    onLoad: function() {
                                        var ds = [].makeDataSource('Handset').orderBy('name').filterBy('');
                                        this.set_dataSource(ds);
                                        ds.load();
                                        
                                        this.get_data().add_handsetFilterChanged(this.filterChanged, this);
                                        
                                        // create "Add handset" button if it isn't exist yet
                                        if (!this.__addHandsetButton) {
                                            var control = ControlsFactory.create("panel");
                                            control.initFromOptions({
                                                width: 110,
                                                height: 210,
                                                padding: '5',
                                                domHandlers: {
                                                    'click': function() {
                                                        this.get_window().newHandset();
                                                    },
                                                    'mouseover': function() {
                                                        this.addCssClass('handset_item_hover handset_item_add_hover');
                                                    },
                                                    'mouseout': function() {
                                                        this.removeCssClass('handset_item_hover handset_item_add_hover');
                                                    }
                                                },
                                                controls: [
                                                    { type: 'panel', width: 80, height: 120, cssClass: 'add_image', halign: 'center', margin: '0 0 0 5' },
                                                    { type: 'label', text: 'Add new handset', cssClass: 'name', width: '*' }
                                                ],
                                                cssClass: 'handset_item handset_item_add'
                                            });

                                            this.__addHandsetButton = control;
                                            this.controls.add(control, { alwaysFirst: true });
                                        }
                                    }
                                }
                            ]
                        },
                        {
                            type: 'searchPane',
                            onSearch: function(sender, args) {
                                this.get_data().set_handsetFilter(args.query);
                            }
                        }
                    ]
                }

                // #endregion
            ]
        }
    ]
})