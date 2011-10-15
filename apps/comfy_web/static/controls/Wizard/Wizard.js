Type.createNamespace('Phoenix.UI');

Phoenix.UI.Wizard = function() {
    Phoenix.UI.Wizard.constructBase(this);
};

Phoenix.UI.Wizard.prototype = {
    initFromOptions: function(options) {
        var thisObj = this;
        this.addCssClass('wizard_control');
        
        options.onLoad = function() {
            this.set_currentStep(0);
        };
        
        if (isFunction(options.onFinish)) {
            this.add_onFinish(options.onFinish, this);
        }
        
        if (isFunction(options.onStepChanged)) {
            this.add_currentStepChanged(options.onStepChanged, this);
        }
                    
        options.orientation = 'horizontal';
        options.controls = [
            {
                id: 'wizardContent',
                type: 'panel',
                margin: '0 0 3 0',
                cssClass: 'wizard_content',
                
                onInit: function() {
                    thisObj.add_currentStepChanged(function(sender, args) {
                        this.pageHeader.set_title(thisObj.options.steps[args.newValue].title || '');
                        this.pageHeader.set_description(thisObj.options.steps[args.newValue].description || '');
                        var btnBack = this.buttonsPanel.btnBack;
                        var btnNext = this.buttonsPanel.btnNext;
                        
                        btnBack.set_enabled(!thisObj.isFirstStep());
                        btnNext.set_text(thisObj.isFinishStep() ? 'Finish' : 'Next');
                    }, this);
                },

                controls: [
                    {
                        type: 'pageHeader',
                        id: 'pageHeader',
                        width: '100%'
                    },
                    this._initSteps(options),
                    {
                        type: 'panel',
                        height: '30',
                        id: 'buttonsPanel',
                        orientation: 'horizontal',
                        controls: [
                            {
                                type: 'button',
                                id: 'btnBack',
                                text: 'Back',
                                width: '73',
                                d: true,
                                onClick: function() {
                                    thisObj.prevStep();
                                }
                            },
                            {
                                type: 'button',
                                id: 'btnNext',
                                text: 'Next',
                                width: '73',
                                onClick: function() {
                                    thisObj.nextStep();
                                }
                            }
                        ]
                    }
                ]
            },
            this._initStepsNavigation(options)
        ];
    
        Phoenix.UI.Wizard.callBase(this, "initFromOptions", [ options ]);
    },
    
    _initSteps: function(options) {
        var thisObj = this;

        var stepsContainer = {
            type: 'panel',
            id: 'stepsContainer',
            width: '100%',
            controls: [],
            onLoad: function() {
                thisObj.add_currentStepChanged(function(sender, args) {
                    if (!isNullOrUndefined(args.oldValue)) {
                        this['step_' + args.oldValue].hide(true);
                    }
                    
                    this['step_' + args.newValue].show(true);
                }, this);
            }
        };
        
        if (!options.steps || options.steps.length == 0) {
            throw new Error('[Wizard]: wizard must contains at least the one step');
        }
        
        for (var i = 0; i < options.steps.length; i++) {
            var step = options.steps[i];
            
            if (!step.type) {
                step.type = 'panel';
            }
            
            step.width = '100%';
            step.visible = false;
            step.height = '100%';
            step.id = 'step_' + i;
            
            stepsContainer.controls.add(step);
        }
        
        return stepsContainer;
    },
    
    getStep: function(index) {
        return this.wizardContent.stepsContainer['step_' + index];
    },
    
    _initStepsNavigation: function(options) {
        var thisObj = this;
        
        var navControl = {
            type: 'panel',
            width: '130px',
            height: '?',
            cssClass: 'wizardNavigation',
            controls: [],
            onLoad: function() {
                thisObj.add_currentStepChanged(function(sender, args) {
                    if (!isNullOrUndefined(args.oldValue)) {
                        this['stepLink_' + args.oldValue].removeCssClass('stepLink_selected');
                    }
                    
                    this['stepLink_' + args.newValue].addCssClass('stepLink_selected');
                    
                    var btnNext = this.btnPanel.btnNext;
                    var btnBack = this.btnPanel.btnBack;
                    btnNext.set_text(thisObj.isFinishStep() ? 'Finish' : 'Next');
                    
                    btnBack.set_enabled(!thisObj.isFirstStep());
                    
                    if (thisObj.isFinishStep()) {
                        btnNext.addCssClass('finish_step');
                    } else {
                        btnNext.removeCssClass('finish_step');
                    }
                }, this);
            }
        };
        
        navControl.controls.add({
            id: 'btnPanel',
            cssClass: 'navigation_buttons',
            type: 'panel',
            orientation: 'horizontal',
            height: 25,
            controls: [
                {
                    id: 'btnBack',
                    type: 'link',
                    width: '*',
                    height: '16',
                    valign: 'middle',
                    margin: '5 0 0 0',
                    text: 'Back',
                    cssClass: 'btnBack',
                    onClick: function() {
                        thisObj.prevStep();
                    }
                },
                {
                    id: 'btnNext',
                    type: 'link',
                    text: 'Next',
                    width: '*',
                    height: '16',
                    valign: 'middle',
                    margin: '0 0 5 0',
                    cssClass: 'btnNext',
                    onClick: function() {
                        thisObj.nextStep();
                    }
                }
            ]
        });
        
        for (var i = 0; i < options.steps.length; i++) {
            var step = options.steps[i];
            navControl.controls.add({
                id: 'stepLink_' + i,
                type: 'link',
                width: '*',
                padding: '10 2',
                border: '1',
                height: '37',
                cssClass: 'stepLink',
                controls: [
                    {
                        type: 'label',
                        cssClass: 'step_num',
                        text: 'Step ' + (i + 1)
                    },
                    {
                        type: 'label',
                        text: step.title || 'Unnamed'
                    }
                ]
            });
        }

        return navControl;
    },
    
    nextStep: function() {
        if (this._currentStep == this.options.steps.length - 1) {
            this.raise_onFinish();
        }
    
        this.set_currentStep(Math.min(this._currentStep + 1, this.options.steps.length - 1));
    },
    
    prevStep: function() {
        this.set_currentStep(Math.max(this._currentStep - 1, 0));
    },
    
    isFinishStep: function() {
        return this._currentStep == (this.options.steps.length - 1);
    },
    
    isFirstStep: function() {
        return this._currentStep == 0;
    }
};

Auto.Properties(Phoenix.UI.Wizard.prototype, [
    { name: 'currentStep', autoEvent: true }
]);

Auto.Events(Phoenix.UI.Wizard.prototype, [
    'onFinish'
]);

Phoenix.UI.Wizard.createClass('Phoenix.UI.Wizard', Phoenix.UI.Panel);
ControlsFactory.registerControl('wizard', Phoenix.UI.Wizard);
