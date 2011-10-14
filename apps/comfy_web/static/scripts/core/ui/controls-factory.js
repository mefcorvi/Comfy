ControlsFactory = {
	_types: {},
	
	create: function(type) {
		if (!ControlsFactory._types.hasOwnProperty(type)) {
			throw new Error('[Controls Factory]: Cannot find a control with \"' + type + '\" type. Please, ensure control with that type is registered');
		}
		
		return new ControlsFactory._types[type]();
	},

	registerControl: function(type, controlType) {
		if (ControlsFactory._types.hasOwnProperty(type)) {
			throw new Error('[Controls Factory]: Control with \"' + type + '\" type is already registered');
		}
		
		ControlsFactory._types[type] = controlType;
	}
};