var Guid = {
    New: function() {
		var res = [], hv;
		var rgx = new RegExp("[2345]");
		
		for (var i = 0; i < 8; i++) {
			hv = (((1 + Math.random()) * 0x10000) | 0).toString(16).substring(1);
			
			if (rgx.exec(i.toString()) != null) {
				if (i == 3) { hv = "6" + hv.substr(1, 3); }
				res.push("-");
			}
			
			res.push(hv.toUpperCase());
		}
		
		return res.join('');
    }
};