Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.LayoutFactory = {
	_types: {},
	
	create: function(type, attachToControl) {
		if (!Nimble.Core.LayoutEngines.LayoutFactory._types.hasOwnProperty(type)) {
			throw new Error('[Layout Factory]: Cannot find a layout with \"' + type + '\" type. Please, ensure layout with that type is registered');
		}
		
		return new Nimble.Core.LayoutEngines.LayoutFactory._types[type](attachToControl);
	},

	register: function(type, layoutType) {
		if (Nimble.Core.LayoutEngines.LayoutFactory._types.hasOwnProperty(type)) {
			throw new Error('[Layout Factory]: Layout with \"' + type + '\" type is already registered');
		}
		
		Nimble.Core.LayoutEngines.LayoutFactory._types[type] = layoutType;
	}
};