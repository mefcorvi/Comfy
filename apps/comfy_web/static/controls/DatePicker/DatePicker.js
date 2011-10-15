Type.createNamespace('Phoenix.UI');

Phoenix.UI.DatePicker = function () {
    Phoenix.UI.DatePicker.constructBase(this);
};

Phoenix.UI.DatePicker.prototype = {
    _withTime: null,
    _shortDateFormat: '%d-%b-%y',
    _longDateFormat: '%d-%b-%y %H:%M',

    initFromOptions: function (options) {
        Phoenix.UI.DatePicker.callBase(this, "initFromOptions", [options]);
    },

    defaultOptions: {
        bindings: {
            '*': 'date'
        },
        height: '24px',
        padding: '2px',
        border: '1'
    },

    set_date: function (date) {
        // convert variable date to string even if it is null
        var newDate = new Date(date + "");

        // check if date instance is valid
        if (!newDate.isValid())
            newDate = null;
    
        var oldValue = this._date;

        if (oldValue && newDate && oldValue.toString() == newDate.toString())
            return;

        this._date = newDate;
        
        if (this.domElement) {
            this.domElement.value = newDate ?  newDate.print(this._getFormat()) : '';
        }

        this.raise_dateChanged({
            newValue: newDate,
            oldValue: oldValue
        })
    },

    _getFormat: function() {
        return (this._withTime) ? this._longDateFormat  : this._shortDateFormat;
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('input', domElement);
        this.domElement.value = this._date || '';

        var $element = $(this.domElement);
        $element.change(function() {
            this.set_date($element.val());
        }.bind(this));

        domElement.appendChild(this.domElement);

        var format = this._getFormat();
        var options = {
            showsTime: this._withTime,
            ifFormat: format,
            daFormat: '%l;%M %p, %e %m,  %Y',
            onOpen: function(cal) {
                cal.element.style.zIndex = DepthManager.getNewZIndex();
            },
            onSelect: function(cal) {
                this.set_date(cal.date);
            }.bind(this),
            onClose: function(cal) {
                cal.hide();
                DOM.remove(cal.element);
            }.bind(this)
        };
        
        if (this.get_date()) {
            options['date'] = this.get_date();
        }

        $element.dynDateTime(options);

        /*$element.datepicker({
            dateFormat: 'dd-MM-yy',
            onSelect: function (dataText, inst) {
                var date = $(this.domElement).datepicker('getDate');

                if (date === this._date)
                    return;
                var oldValue = this._date;
                this._date = date;

                this.raise_dateChanged({
                    newValue: date,
                    oldValue: oldValue
                })
            } .bind(this)
        });*/

        Phoenix.UI.DatePicker.callBase(this, "instantiateInDom", [domElement]);
    }
};

Phoenix.UI.DatePicker.createClass('Phoenix.UI.DatePicker', Control);
ControlsFactory.registerControl('datePicker', Phoenix.UI.DatePicker);

Auto.Properties(Phoenix.UI.DatePicker.prototype, [
    { name: 'date', autoEvent: true }
]);