
/*!
 * jQuery JavaScript Library v1.4.1
 * http://jquery.com/
 *
 * Copyright 2010, John Resig
 * Dual licensed under the MIT or GPL Version 2 licenses.
 * http://jquery.org/license
 *
 * Includes Sizzle.js
 * http://sizzlejs.com/
 * Copyright 2010, The Dojo Foundation
 * Released under the MIT, BSD, and GPL Licenses.
 *
 * Date: Mon Jan 25 19:43:33 2010 -0500
 */
(function( window, undefined ) {

// Define a local copy of jQuery
var jQuery = function( selector, context ) {
		// The jQuery object is actually just the init constructor 'enhanced'
		return new jQuery.fn.init( selector, context );
	},

	// Map over jQuery in case of overwrite
	_jQuery = window.jQuery,

	// Map over the $ in case of overwrite
	_$ = window.$,

	// Use the correct document accordingly with window argument (sandbox)
	document = window.document,

	// A central reference to the root jQuery(document)
	rootjQuery,

	// A simple way to check for HTML strings or ID strings
	// (both of which we optimize for)
	quickExpr = /^[^<]*(<[\w\W]+>)[^>]*$|^#([\w-]+)$/,

	// Is it a simple selector
	isSimple = /^.[^:#\[\.,]*$/,

	// Check if a string has a non-whitespace character in it
	rnotwhite = /\S/,

	// Used for trimming whitespace
	rtrim = /^(\s|\u00A0)+|(\s|\u00A0)+$/g,

	// Match a standalone tag
	rsingleTag = /^<(\w+)\s*\/?>(?:<\/\1>)?$/,

	// Keep a UserAgent string for use with jQuery.browser
	userAgent = navigator.userAgent,

	// For matching the engine and version of the browser
	browserMatch,
	
	// Has the ready events already been bound?
	readyBound = false,
	
	// The functions to execute on DOM ready
	readyList = [],

	// The ready event handler
	DOMContentLoaded,

	// Save a reference to some core methods
	toString = Object.prototype.toString,
	hasOwnProperty = Object.prototype.hasOwnProperty,
	push = Array.prototype.push,
	slice = Array.prototype.slice,
	indexOf = Array.prototype.indexOf;

jQuery.fn = jQuery.prototype = {
	init: function( selector, context ) {
		var match, elem, ret, doc;

		// Handle $(""), $(null), or $(undefined)
		if ( !selector ) {
			return this;
		}

		// Handle $(DOMElement)
		if ( selector.nodeType ) {
			this.context = this[0] = selector;
			this.length = 1;
			return this;
		}

		// Handle HTML strings
		if ( typeof selector === "string" ) {
			// Are we dealing with HTML string or an ID?
			match = quickExpr.exec( selector );

			// Verify a match, and that no context was specified for #id
			if ( match && (match[1] || !context) ) {

				// HANDLE: $(html) -> $(array)
				if ( match[1] ) {
					doc = (context ? context.ownerDocument || context : document);

					// If a single string is passed in and it's a single tag
					// just do a createElement and skip the rest
					ret = rsingleTag.exec( selector );

					if ( ret ) {
						if ( jQuery.isPlainObject( context ) ) {
							selector = [ document.createElement( ret[1] ) ];
							jQuery.fn.attr.call( selector, context, true );

						} else {
							selector = [ doc.createElement( ret[1] ) ];
						}

					} else {
						ret = buildFragment( [ match[1] ], [ doc ] );
						selector = (ret.cacheable ? ret.fragment.cloneNode(true) : ret.fragment).childNodes;
					}

				// HANDLE: $("#id")
				} else {
					elem = document.getElementById( match[2] );

					if ( elem ) {
						// Handle the case where IE and Opera return items
						// by name instead of ID
						if ( elem.id !== match[2] ) {
							return rootjQuery.find( selector );
						}

						// Otherwise, we inject the element directly into the jQuery object
						this.length = 1;
						this[0] = elem;
					}

					this.context = document;
					this.selector = selector;
					return this;
				}

			// HANDLE: $("TAG")
			} else if ( !context && /^\w+$/.test( selector ) ) {
				this.selector = selector;
				this.context = document;
				selector = document.getElementsByTagName( selector );

			// HANDLE: $(expr, $(...))
			} else if ( !context || context.jquery ) {
				return (context || rootjQuery).find( selector );

			// HANDLE: $(expr, context)
			// (which is just equivalent to: $(context).find(expr)
			} else {
				return jQuery( context ).find( selector );
			}

		// HANDLE: $(function)
		// Shortcut for document ready
		} else if ( jQuery.isFunction( selector ) ) {
			return rootjQuery.ready( selector );
		}

		if (selector.selector !== undefined) {
			this.selector = selector.selector;
			this.context = selector.context;
		}

		return jQuery.isArray( selector ) ?
			this.setArray( selector ) :
			jQuery.makeArray( selector, this );
	},

	// Start with an empty selector
	selector: "",

	// The current version of jQuery being used
	jquery: "1.4.1",

	// The default length of a jQuery object is 0
	length: 0,

	// The number of elements contained in the matched element set
	size: function() {
		return this.length;
	},

	toArray: function() {
		return slice.call( this, 0 );
	},

	// Get the Nth element in the matched element set OR
	// Get the whole matched element set as a clean array
	get: function( num ) {
		return num == null ?

			// Return a 'clean' array
			this.toArray() :

			// Return just the object
			( num < 0 ? this.slice(num)[ 0 ] : this[ num ] );
	},

	// Take an array of elements and push it onto the stack
	// (returning the new matched element set)
	pushStack: function( elems, name, selector ) {
		// Build a new jQuery matched element set
		var ret = jQuery( elems || null );

		// Add the old object onto the stack (as a reference)
		ret.prevObject = this;

		ret.context = this.context;

		if ( name === "find" ) {
			ret.selector = this.selector + (this.selector ? " " : "") + selector;
		} else if ( name ) {
			ret.selector = this.selector + "." + name + "(" + selector + ")";
		}

		// Return the newly-formed element set
		return ret;
	},

	// Force the current matched set of elements to become
	// the specified array of elements (destroying the stack in the process)
	// You should use pushStack() in order to do this, but maintain the stack
	setArray: function( elems ) {
		// Resetting the length to 0, then using the native Array push
		// is a super-fast way to populate an object with array-like properties
		this.length = 0;
		push.apply( this, elems );

		return this;
	},

	// Execute a callback for every element in the matched set.
	// (You can seed the arguments with an array of args, but this is
	// only used internally.)
	each: function( callback, args ) {
		return jQuery.each( this, callback, args );
	},
	
	ready: function( fn ) {
		// Attach the listeners
		jQuery.bindReady();

		// If the DOM is already ready
		if ( jQuery.isReady ) {
			// Execute the function immediately
			fn.call( document, jQuery );

		// Otherwise, remember the function for later
		} else if ( readyList ) {
			// Add the function to the wait list
			readyList.push( fn );
		}

		return this;
	},
	
	eq: function( i ) {
		return i === -1 ?
			this.slice( i ) :
			this.slice( i, +i + 1 );
	},

	first: function() {
		return this.eq( 0 );
	},

	last: function() {
		return this.eq( -1 );
	},

	slice: function() {
		return this.pushStack( slice.apply( this, arguments ),
			"slice", slice.call(arguments).join(",") );
	},

	map: function( callback ) {
		return this.pushStack( jQuery.map(this, function( elem, i ) {
			return callback.call( elem, i, elem );
		}));
	},
	
	end: function() {
		return this.prevObject || jQuery(null);
	},

	// For internal use only.
	// Behaves like an Array's method, not like a jQuery method.
	push: push,
	sort: [].sort,
	splice: [].splice
};

// Give the init function the jQuery prototype for later instantiation
jQuery.fn.init.prototype = jQuery.fn;

jQuery.extend = jQuery.fn.extend = function() {
	// copy reference to target object
	var target = arguments[0] || {}, i = 1, length = arguments.length, deep = false, options, name, src, copy;

	// Handle a deep copy situation
	if ( typeof target === "boolean" ) {
		deep = target;
		target = arguments[1] || {};
		// skip the boolean and the target
		i = 2;
	}

	// Handle case when target is a string or something (possible in deep copy)
	if ( typeof target !== "object" && !jQuery.isFunction(target) ) {
		target = {};
	}

	// extend jQuery itself if only one argument is passed
	if ( length === i ) {
		target = this;
		--i;
	}

	for ( ; i < length; i++ ) {
		// Only deal with non-null/undefined values
		if ( (options = arguments[ i ]) != null ) {
			// Extend the base object
			for ( name in options ) {
				src = target[ name ];
				copy = options[ name ];

				// Prevent never-ending loop
				if ( target === copy ) {
					continue;
				}

				// Recurse if we're merging object literal values or arrays
				if ( deep && copy && ( jQuery.isPlainObject(copy) || jQuery.isArray(copy) ) ) {
					var clone = src && ( jQuery.isPlainObject(src) || jQuery.isArray(src) ) ? src
						: jQuery.isArray(copy) ? [] : {};

					// Never move original objects, clone them
					target[ name ] = jQuery.extend( deep, clone, copy );

				// Don't bring in undefined values
				} else if ( copy !== undefined ) {
					target[ name ] = copy;
				}
			}
		}
	}

	// Return the modified object
	return target;
};

jQuery.extend({
	noConflict: function( deep ) {
		window.$ = _$;

		if ( deep ) {
			window.jQuery = _jQuery;
		}

		return jQuery;
	},
	
	// Is the DOM ready to be used? Set to true once it occurs.
	isReady: false,
	
	// Handle when the DOM is ready
	ready: function() {
		// Make sure that the DOM is not already loaded
		if ( !jQuery.isReady ) {
			// Make sure body exists, at least, in case IE gets a little overzealous (ticket #5443).
			if ( !document.body ) {
				return setTimeout( jQuery.ready, 13 );
			}

			// Remember that the DOM is ready
			jQuery.isReady = true;

			// If there are functions bound, to execute
			if ( readyList ) {
				// Execute all of them
				var fn, i = 0;
				while ( (fn = readyList[ i++ ]) ) {
					fn.call( document, jQuery );
				}

				// Reset the list of functions
				readyList = null;
			}

			// Trigger any bound ready events
			if ( jQuery.fn.triggerHandler ) {
				jQuery( document ).triggerHandler( "ready" );
			}
		}
	},
	
	bindReady: function() {
		if ( readyBound ) {
			return;
		}

		readyBound = true;

		// Catch cases where $(document).ready() is called after the
		// browser event has already occurred.
		if ( document.readyState === "complete" ) {
			return jQuery.ready();
		}

		// Mozilla, Opera and webkit nightlies currently support this event
		if ( document.addEventListener ) {
			// Use the handy event callback
			document.addEventListener( "DOMContentLoaded", DOMContentLoaded, false );
			
			// A fallback to window.onload, that will always work
			window.addEventListener( "load", jQuery.ready, false );

		// If IE event model is used
		} else if ( document.attachEvent ) {
			// ensure firing before onload,
			// maybe late but safe also for iframes
			document.attachEvent("onreadystatechange", DOMContentLoaded);
			
			// A fallback to window.onload, that will always work
			window.attachEvent( "onload", jQuery.ready );

			// If IE and not a frame
			// continually check to see if the document is ready
			var toplevel = false;

			try {
				toplevel = window.frameElement == null;
			} catch(e) {}

			if ( document.documentElement.doScroll && toplevel ) {
				doScrollCheck();
			}
		}
	},

	// See test/unit/core.js for details concerning isFunction.
	// Since version 1.3, DOM methods and functions like alert
	// aren't supported. They return false on IE (#2968).
	isFunction: function( obj ) {
		return toString.call(obj) === "[object Function]";
	},

	isArray: function( obj ) {
		return toString.call(obj) === "[object Array]";
	},

	isPlainObject: function( obj ) {
		// Must be an Object.
		// Because of IE, we also have to check the presence of the constructor property.
		// Make sure that DOM nodes and window objects don't pass through, as well
		if ( !obj || toString.call(obj) !== "[object Object]" || obj.nodeType || obj.setInterval ) {
			return false;
		}
		
		// Not own constructor property must be Object
		if ( obj.constructor
			&& !hasOwnProperty.call(obj, "constructor")
			&& !hasOwnProperty.call(obj.constructor.prototype, "isPrototypeOf") ) {
			return false;
		}
		
		// Own properties are enumerated firstly, so to speed up,
		// if last one is own, then all properties are own.
	
		var key;
		for ( key in obj ) {}
		
		return key === undefined || hasOwnProperty.call( obj, key );
	},

	isEmptyObject: function( obj ) {
		for ( var name in obj ) {
			return false;
		}
		return true;
	},
	
	error: function( msg ) {
		throw msg;
	},
	
	parseJSON: function( data ) {
		if ( typeof data !== "string" || !data ) {
			return null;
		}
		
		// Make sure the incoming data is actual JSON
		// Logic borrowed from http://json.org/json2.js
		if ( /^[\],:{}\s]*$/.test(data.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g, "@")
			.replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, "]")
			.replace(/(?:^|:|,)(?:\s*\[)+/g, "")) ) {

			// Try to use the native JSON parser first
			return window.JSON && window.JSON.parse ?
				window.JSON.parse( data ) :
				(new Function("return " + data))();

		} else {
			jQuery.error( "Invalid JSON: " + data );
		}
	},

	noop: function() {},

	// Evalulates a script in a global context
	globalEval: function( data ) {
		if ( data && rnotwhite.test(data) ) {
			// Inspired by code by Andrea Giammarchi
			// http://webreflection.blogspot.com/2007/08/global-scope-evaluation-and-dom.html
			var head = document.getElementsByTagName("head")[0] || document.documentElement,
				script = document.createElement("script");

			script.type = "text/javascript";

			if ( jQuery.support.scriptEval ) {
				script.appendChild( document.createTextNode( data ) );
			} else {
				script.text = data;
			}

			// Use insertBefore instead of appendChild to circumvent an IE6 bug.
			// This arises when a base node is used (#2709).
			head.insertBefore( script, head.firstChild );
			head.removeChild( script );
		}
	},

	nodeName: function( elem, name ) {
		return elem.nodeName && elem.nodeName.toUpperCase() === name.toUpperCase();
	},

	// args is for internal usage only
	each: function( object, callback, args ) {
		var name, i = 0,
			length = object.length,
			isObj = length === undefined || jQuery.isFunction(object);

		if ( args ) {
			if ( isObj ) {
				for ( name in object ) {
					if ( callback.apply( object[ name ], args ) === false ) {
						break;
					}
				}
			} else {
				for ( ; i < length; ) {
					if ( callback.apply( object[ i++ ], args ) === false ) {
						break;
					}
				}
			}

		// A special, fast, case for the most common use of each
		} else {
			if ( isObj ) {
				for ( name in object ) {
					if ( callback.call( object[ name ], name, object[ name ] ) === false ) {
						break;
					}
				}
			} else {
				for ( var value = object[0];
					i < length && callback.call( value, i, value ) !== false; value = object[++i] ) {}
			}
		}

		return object;
	},

	trim: function( text ) {
		return (text || "").replace( rtrim, "" );
	},

	// results is for internal usage only
	makeArray: function( array, results ) {
		var ret = results || [];

		if ( array != null ) {
			// The window, strings (and functions) also have 'length'
			// The extra typeof function check is to prevent crashes
			// in Safari 2 (See: #3039)
			if ( array.length == null || typeof array === "string" || jQuery.isFunction(array) || (typeof array !== "function" && array.setInterval) ) {
				push.call( ret, array );
			} else {
				jQuery.merge( ret, array );
			}
		}

		return ret;
	},

	inArray: function( elem, array ) {
		if ( array.indexOf ) {
			return array.indexOf( elem );
		}

		for ( var i = 0, length = array.length; i < length; i++ ) {
			if ( array[ i ] === elem ) {
				return i;
			}
		}

		return -1;
	},

	merge: function( first, second ) {
		var i = first.length, j = 0;

		if ( typeof second.length === "number" ) {
			for ( var l = second.length; j < l; j++ ) {
				first[ i++ ] = second[ j ];
			}
		} else {
			while ( second[j] !== undefined ) {
				first[ i++ ] = second[ j++ ];
			}
		}

		first.length = i;

		return first;
	},

	grep: function( elems, callback, inv ) {
		var ret = [];

		// Go through the array, only saving the items
		// that pass the validator function
		for ( var i = 0, length = elems.length; i < length; i++ ) {
			if ( !inv !== !callback( elems[ i ], i ) ) {
				ret.push( elems[ i ] );
			}
		}

		return ret;
	},

	// arg is for internal usage only
	map: function( elems, callback, arg ) {
		var ret = [], value;

		// Go through the array, translating each of the items to their
		// new value (or values).
		for ( var i = 0, length = elems.length; i < length; i++ ) {
			value = callback( elems[ i ], i, arg );

			if ( value != null ) {
				ret[ ret.length ] = value;
			}
		}

		return ret.concat.apply( [], ret );
	},

	// A global GUID counter for objects
	guid: 1,

	proxy: function( fn, proxy, thisObject ) {
		if ( arguments.length === 2 ) {
			if ( typeof proxy === "string" ) {
				thisObject = fn;
				fn = thisObject[ proxy ];
				proxy = undefined;

			} else if ( proxy && !jQuery.isFunction( proxy ) ) {
				thisObject = proxy;
				proxy = undefined;
			}
		}

		if ( !proxy && fn ) {
			proxy = function() {
				return fn.apply( thisObject || this, arguments );
			};
		}

		// Set the guid of unique handler to the same of original handler, so it can be removed
		if ( fn ) {
			proxy.guid = fn.guid = fn.guid || proxy.guid || jQuery.guid++;
		}

		// So proxy can be declared as an argument
		return proxy;
	},

	// Use of jQuery.browser is frowned upon.
	// More details: http://docs.jquery.com/Utilities/jQuery.browser
	uaMatch: function( ua ) {
		ua = ua.toLowerCase();

		var match = /(webkit)[ \/]([\w.]+)/.exec( ua ) ||
			/(opera)(?:.*version)?[ \/]([\w.]+)/.exec( ua ) ||
			/(msie) ([\w.]+)/.exec( ua ) ||
			!/compatible/.test( ua ) && /(mozilla)(?:.*? rv:([\w.]+))?/.exec( ua ) ||
		  	[];

		return { browser: match[1] || "", version: match[2] || "0" };
	},

	browser: {}
});

browserMatch = jQuery.uaMatch( userAgent );
if ( browserMatch.browser ) {
	jQuery.browser[ browserMatch.browser ] = true;
	jQuery.browser.version = browserMatch.version;
}

// Deprecated, use jQuery.browser.webkit instead
if ( jQuery.browser.webkit ) {
	jQuery.browser.safari = true;
}

if ( indexOf ) {
	jQuery.inArray = function( elem, array ) {
		return indexOf.call( array, elem );
	};
}

// All jQuery objects should point back to these
rootjQuery = jQuery(document);

// Cleanup functions for the document ready method
if ( document.addEventListener ) {
	DOMContentLoaded = function() {
		document.removeEventListener( "DOMContentLoaded", DOMContentLoaded, false );
		jQuery.ready();
	};

} else if ( document.attachEvent ) {
	DOMContentLoaded = function() {
		// Make sure body exists, at least, in case IE gets a little overzealous (ticket #5443).
		if ( document.readyState === "complete" ) {
			document.detachEvent( "onreadystatechange", DOMContentLoaded );
			jQuery.ready();
		}
	};
}

// The DOM ready check for Internet Explorer
function doScrollCheck() {
	if ( jQuery.isReady ) {
		return;
	}

	try {
		// If IE is used, use the trick by Diego Perini
		// http://javascript.nwbox.com/IEContentLoaded/
		document.documentElement.doScroll("left");
	} catch( error ) {
		setTimeout( doScrollCheck, 1 );
		return;
	}

	// and execute any waiting functions
	jQuery.ready();
}

function evalScript( i, elem ) {
	if ( elem.src ) {
		jQuery.ajax({
			url: elem.src,
			async: false,
			dataType: "script"
		});
	} else {
		jQuery.globalEval( elem.text || elem.textContent || elem.innerHTML || "" );
	}

	if ( elem.parentNode ) {
		elem.parentNode.removeChild( elem );
	}
}

// Mutifunctional method to get and set values to a collection
// The value/s can be optionally by executed if its a function
function access( elems, key, value, exec, fn, pass ) {
	var length = elems.length;
	
	// Setting many attributes
	if ( typeof key === "object" ) {
		for ( var k in key ) {
			access( elems, k, key[k], exec, fn, value );
		}
		return elems;
	}
	
	// Setting one attribute
	if ( value !== undefined ) {
		// Optionally, function values get executed if exec is true
		exec = !pass && exec && jQuery.isFunction(value);
		
		for ( var i = 0; i < length; i++ ) {
			fn( elems[i], key, exec ? value.call( elems[i], i, fn( elems[i], key ) ) : value, pass );
		}
		
		return elems;
	}
	
	// Getting an attribute
	return length ? fn( elems[0], key ) : null;
}

function now() {
	return (new Date).getTime();
}
(function() {

	jQuery.support = {};

	var root = document.documentElement,
		script = document.createElement("script"),
		div = document.createElement("div"),
		id = "script" + now();

	div.style.display = "none";
	div.innerHTML = "   <link/><table></table><a href='/a' style='color:red;float:left;opacity:.55;'>a</a><input type='checkbox'/>";

	var all = div.getElementsByTagName("*"),
		a = div.getElementsByTagName("a")[0];

	// Can't get basic test support
	if ( !all || !all.length || !a ) {
		return;
	}

	jQuery.support = {
		// IE strips leading whitespace when .innerHTML is used
		leadingWhitespace: div.firstChild.nodeType === 3,

		// Make sure that tbody elements aren't automatically inserted
		// IE will insert them into empty tables
		tbody: !div.getElementsByTagName("tbody").length,

		// Make sure that link elements get serialized correctly by innerHTML
		// This requires a wrapper element in IE
		htmlSerialize: !!div.getElementsByTagName("link").length,

		// Get the style information from getAttribute
		// (IE uses .cssText insted)
		style: /red/.test( a.getAttribute("style") ),

		// Make sure that URLs aren't manipulated
		// (IE normalizes it by default)
		hrefNormalized: a.getAttribute("href") === "/a",

		// Make sure that element opacity exists
		// (IE uses filter instead)
		// Use a regex to work around a WebKit issue. See #5145
		opacity: /^0.55$/.test( a.style.opacity ),

		// Verify style float existence
		// (IE uses styleFloat instead of cssFloat)
		cssFloat: !!a.style.cssFloat,

		// Make sure that if no value is specified for a checkbox
		// that it defaults to "on".
		// (WebKit defaults to "" instead)
		checkOn: div.getElementsByTagName("input")[0].value === "on",

		// Make sure that a selected-by-default option has a working selected property.
		// (WebKit defaults to false instead of true, IE too, if it's in an optgroup)
		optSelected: document.createElement("select").appendChild( document.createElement("option") ).selected,

		// Will be defined later
		checkClone: false,
		scriptEval: false,
		noCloneEvent: true,
		boxModel: null
	};

	script.type = "text/javascript";
	try {
		script.appendChild( document.createTextNode( "window." + id + "=1;" ) );
	} catch(e) {}

	root.insertBefore( script, root.firstChild );

	// Make sure that the execution of code works by injecting a script
	// tag with appendChild/createTextNode
	// (IE doesn't support this, fails, and uses .text instead)
	if ( window[ id ] ) {
		jQuery.support.scriptEval = true;
		delete window[ id ];
	}

	root.removeChild( script );

	if ( div.attachEvent && div.fireEvent ) {
		div.attachEvent("onclick", function click() {
			// Cloning a node shouldn't copy over any
			// bound event handlers (IE does this)
			jQuery.support.noCloneEvent = false;
			div.detachEvent("onclick", click);
		});
		div.cloneNode(true).fireEvent("onclick");
	}

	div = document.createElement("div");
	div.innerHTML = "<input type='radio' name='radiotest' checked='checked'/>";

	var fragment = document.createDocumentFragment();
	fragment.appendChild( div.firstChild );

	// WebKit doesn't clone checked state correctly in fragments
	jQuery.support.checkClone = fragment.cloneNode(true).cloneNode(true).lastChild.checked;

	// Figure out if the W3C box model works as expected
	// document.body must exist before we can do this
	jQuery(function() {
		var div = document.createElement("div");
		div.style.width = div.style.paddingLeft = "1px";

		document.body.appendChild( div );
		jQuery.boxModel = jQuery.support.boxModel = div.offsetWidth === 2;
		document.body.removeChild( div ).style.display = 'none';
		div = null;
	});

	// Technique from Juriy Zaytsev
	// http://thinkweb2.com/projects/prototype/detecting-event-support-without-browser-sniffing/
	var eventSupported = function( eventName ) { 
		var el = document.createElement("div"); 
		eventName = "on" + eventName; 

		var isSupported = (eventName in el); 
		if ( !isSupported ) { 
			el.setAttribute(eventName, "return;"); 
			isSupported = typeof el[eventName] === "function"; 
		} 
		el = null; 

		return isSupported; 
	};
	
	jQuery.support.submitBubbles = eventSupported("submit");
	jQuery.support.changeBubbles = eventSupported("change");

	// release memory in IE
	root = script = div = all = a = null;
})();

jQuery.props = {
	"for": "htmlFor",
	"class": "className",
	readonly: "readOnly",
	maxlength: "maxLength",
	cellspacing: "cellSpacing",
	rowspan: "rowSpan",
	colspan: "colSpan",
	tabindex: "tabIndex",
	usemap: "useMap",
	frameborder: "frameBorder"
};
var expando = "jQuery" + now(), uuid = 0, windowData = {};
var emptyObject = {};

jQuery.extend({
	cache: {},
	
	expando:expando,

	// The following elements throw uncatchable exceptions if you
	// attempt to add expando properties to them.
	noData: {
		"embed": true,
		"object": true,
		"applet": true
	},

	data: function( elem, name, data ) {
		if ( elem.nodeName && jQuery.noData[elem.nodeName.toLowerCase()] ) {
			return;
		}

		elem = elem == window ?
			windowData :
			elem;

		var id = elem[ expando ], cache = jQuery.cache, thisCache;

		// Handle the case where there's no name immediately
		if ( !name && !id ) {
			return null;
		}

		// Compute a unique ID for the element
		if ( !id ) { 
			id = ++uuid;
		}

		// Avoid generating a new cache unless none exists and we
		// want to manipulate it.
		if ( typeof name === "object" ) {
			elem[ expando ] = id;
			thisCache = cache[ id ] = jQuery.extend(true, {}, name);
		} else if ( cache[ id ] ) {
			thisCache = cache[ id ];
		} else if ( typeof data === "undefined" ) {
			thisCache = emptyObject;
		} else {
			thisCache = cache[ id ] = {};
		}

		// Prevent overriding the named cache with undefined values
		if ( data !== undefined ) {
			elem[ expando ] = id;
			thisCache[ name ] = data;
		}

		return typeof name === "string" ? thisCache[ name ] : thisCache;
	},

	removeData: function( elem, name ) {
		if ( elem.nodeName && jQuery.noData[elem.nodeName.toLowerCase()] ) {
			return;
		}

		elem = elem == window ?
			windowData :
			elem;

		var id = elem[ expando ], cache = jQuery.cache, thisCache = cache[ id ];

		// If we want to remove a specific section of the element's data
		if ( name ) {
			if ( thisCache ) {
				// Remove the section of cache data
				delete thisCache[ name ];

				// If we've removed all the data, remove the element's cache
				if ( jQuery.isEmptyObject(thisCache) ) {
					jQuery.removeData( elem );
				}
			}

		// Otherwise, we want to remove all of the element's data
		} else {
			// Clean up the element expando
			try {
				delete elem[ expando ];
			} catch( e ) {
				// IE has trouble directly removing the expando
				// but it's ok with using removeAttribute
				if ( elem.removeAttribute ) {
					elem.removeAttribute( expando );
				}
			}

			// Completely remove the data cache
			delete cache[ id ];
		}
	}
});

jQuery.fn.extend({
	data: function( key, value ) {
		if ( typeof key === "undefined" && this.length ) {
			return jQuery.data( this[0] );

		} else if ( typeof key === "object" ) {
			return this.each(function() {
				jQuery.data( this, key );
			});
		}

		var parts = key.split(".");
		parts[1] = parts[1] ? "." + parts[1] : "";

		if ( value === undefined ) {
			var data = this.triggerHandler("getData" + parts[1] + "!", [parts[0]]);

			if ( data === undefined && this.length ) {
				data = jQuery.data( this[0], key );
			}
			return data === undefined && parts[1] ?
				this.data( parts[0] ) :
				data;
		} else {
			return this.trigger("setData" + parts[1] + "!", [parts[0], value]).each(function() {
				jQuery.data( this, key, value );
			});
		}
	},

	removeData: function( key ) {
		return this.each(function() {
			jQuery.removeData( this, key );
		});
	}
});
jQuery.extend({
	queue: function( elem, type, data ) {
		if ( !elem ) {
			return;
		}

		type = (type || "fx") + "queue";
		var q = jQuery.data( elem, type );

		// Speed up dequeue by getting out quickly if this is just a lookup
		if ( !data ) {
			return q || [];
		}

		if ( !q || jQuery.isArray(data) ) {
			q = jQuery.data( elem, type, jQuery.makeArray(data) );

		} else {
			q.push( data );
		}

		return q;
	},

	dequeue: function( elem, type ) {
		type = type || "fx";

		var queue = jQuery.queue( elem, type ), fn = queue.shift();

		// If the fx queue is dequeued, always remove the progress sentinel
		if ( fn === "inprogress" ) {
			fn = queue.shift();
		}

		if ( fn ) {
			// Add a progress sentinel to prevent the fx queue from being
			// automatically dequeued
			if ( type === "fx" ) {
				queue.unshift("inprogress");
			}

			fn.call(elem, function() {
				jQuery.dequeue(elem, type);
			});
		}
	}
});

jQuery.fn.extend({
	queue: function( type, data ) {
		if ( typeof type !== "string" ) {
			data = type;
			type = "fx";
		}

		if ( data === undefined ) {
			return jQuery.queue( this[0], type );
		}
		return this.each(function( i, elem ) {
			var queue = jQuery.queue( this, type, data );

			if ( type === "fx" && queue[0] !== "inprogress" ) {
				jQuery.dequeue( this, type );
			}
		});
	},
	dequeue: function( type ) {
		return this.each(function() {
			jQuery.dequeue( this, type );
		});
	},

	// Based off of the plugin by Clint Helfers, with permission.
	// http://blindsignals.com/index.php/2009/07/jquery-delay/
	delay: function( time, type ) {
		time = jQuery.fx ? jQuery.fx.speeds[time] || time : time;
		type = type || "fx";

		return this.queue( type, function() {
			var elem = this;
			setTimeout(function() {
				jQuery.dequeue( elem, type );
			}, time );
		});
	},

	clearQueue: function( type ) {
		return this.queue( type || "fx", [] );
	}
});
var rclass = /[\n\t]/g,
	rspace = /\s+/,
	rreturn = /\r/g,
	rspecialurl = /href|src|style/,
	rtype = /(button|input)/i,
	rfocusable = /(button|input|object|select|textarea)/i,
	rclickable = /^(a|area)$/i,
	rradiocheck = /radio|checkbox/;

jQuery.fn.extend({
	attr: function( name, value ) {
		return access( this, name, value, true, jQuery.attr );
	},

	removeAttr: function( name, fn ) {
		return this.each(function(){
			jQuery.attr( this, name, "" );
			if ( this.nodeType === 1 ) {
				this.removeAttribute( name );
			}
		});
	},

	addClass: function( value ) {
		if ( jQuery.isFunction(value) ) {
			return this.each(function(i) {
				var self = jQuery(this);
				self.addClass( value.call(this, i, self.attr("class")) );
			});
		}

		if ( value && typeof value === "string" ) {
			var classNames = (value || "").split( rspace );

			for ( var i = 0, l = this.length; i < l; i++ ) {
				var elem = this[i];

				if ( elem.nodeType === 1 ) {
					if ( !elem.className ) {
						elem.className = value;

					} else {
						var className = " " + elem.className + " ";
						for ( var c = 0, cl = classNames.length; c < cl; c++ ) {
							if ( className.indexOf( " " + classNames[c] + " " ) < 0 ) {
								elem.className += " " + classNames[c];
							}
						}
					}
				}
			}
		}

		return this;
	},

	removeClass: function( value ) {
		if ( jQuery.isFunction(value) ) {
			return this.each(function(i) {
				var self = jQuery(this);
				self.removeClass( value.call(this, i, self.attr("class")) );
			});
		}

		if ( (value && typeof value === "string") || value === undefined ) {
			var classNames = (value || "").split(rspace);

			for ( var i = 0, l = this.length; i < l; i++ ) {
				var elem = this[i];

				if ( elem.nodeType === 1 && elem.className ) {
					if ( value ) {
						var className = (" " + elem.className + " ").replace(rclass, " ");
						for ( var c = 0, cl = classNames.length; c < cl; c++ ) {
							className = className.replace(" " + classNames[c] + " ", " ");
						}
						elem.className = className.substring(1, className.length - 1);

					} else {
						elem.className = "";
					}
				}
			}
		}

		return this;
	},

	toggleClass: function( value, stateVal ) {
		var type = typeof value, isBool = typeof stateVal === "boolean";

		if ( jQuery.isFunction( value ) ) {
			return this.each(function(i) {
				var self = jQuery(this);
				self.toggleClass( value.call(this, i, self.attr("class"), stateVal), stateVal );
			});
		}

		return this.each(function() {
			if ( type === "string" ) {
				// toggle individual class names
				var className, i = 0, self = jQuery(this),
					state = stateVal,
					classNames = value.split( rspace );

				while ( (className = classNames[ i++ ]) ) {
					// check each className given, space seperated list
					state = isBool ? state : !self.hasClass( className );
					self[ state ? "addClass" : "removeClass" ]( className );
				}

			} else if ( type === "undefined" || type === "boolean" ) {
				if ( this.className ) {
					// store className if set
					jQuery.data( this, "__className__", this.className );
				}

				// toggle whole className
				this.className = this.className || value === false ? "" : jQuery.data( this, "__className__" ) || "";
			}
		});
	},

	hasClass: function( selector ) {
		var className = " " + selector + " ";
		for ( var i = 0, l = this.length; i < l; i++ ) {
			if ( (" " + this[i].className + " ").replace(rclass, " ").indexOf( className ) > -1 ) {
				return true;
			}
		}

		return false;
	},

	val: function( value ) {
		if ( value === undefined ) {
			var elem = this[0];

			if ( elem ) {
				if ( jQuery.nodeName( elem, "option" ) ) {
					return (elem.attributes.value || {}).specified ? elem.value : elem.text;
				}

				// We need to handle select boxes special
				if ( jQuery.nodeName( elem, "select" ) ) {
					var index = elem.selectedIndex,
						values = [],
						options = elem.options,
						one = elem.type === "select-one";

					// Nothing was selected
					if ( index < 0 ) {
						return null;
					}

					// Loop through all the selected options
					for ( var i = one ? index : 0, max = one ? index + 1 : options.length; i < max; i++ ) {
						var option = options[ i ];

						if ( option.selected ) {
							// Get the specifc value for the option
							value = jQuery(option).val();

							// We don't need an array for one selects
							if ( one ) {
								return value;
							}

							// Multi-Selects return an array
							values.push( value );
						}
					}

					return values;
				}

				// Handle the case where in Webkit "" is returned instead of "on" if a value isn't specified
				if ( rradiocheck.test( elem.type ) && !jQuery.support.checkOn ) {
					return elem.getAttribute("value") === null ? "on" : elem.value;
				}
				

				// Everything else, we just grab the value
				return (elem.value || "").replace(rreturn, "");

			}

			return undefined;
		}

		var isFunction = jQuery.isFunction(value);

		return this.each(function(i) {
			var self = jQuery(this), val = value;

			if ( this.nodeType !== 1 ) {
				return;
			}

			if ( isFunction ) {
				val = value.call(this, i, self.val());
			}

			// Typecast each time if the value is a Function and the appended
			// value is therefore different each time.
			if ( typeof val === "number" ) {
				val += "";
			}

			if ( jQuery.isArray(val) && rradiocheck.test( this.type ) ) {
				this.checked = jQuery.inArray( self.val(), val ) >= 0;

			} else if ( jQuery.nodeName( this, "select" ) ) {
				var values = jQuery.makeArray(val);

				jQuery( "option", this ).each(function() {
					this.selected = jQuery.inArray( jQuery(this).val(), values ) >= 0;
				});

				if ( !values.length ) {
					this.selectedIndex = -1;
				}

			} else {
				this.value = val;
			}
		});
	}
});

jQuery.extend({
	attrFn: {
		val: true,
		css: true,
		html: true,
		text: true,
		data: true,
		width: true,
		height: true,
		offset: true
	},
		
	attr: function( elem, name, value, pass ) {
		// don't set attributes on text and comment nodes
		if ( !elem || elem.nodeType === 3 || elem.nodeType === 8 ) {
			return undefined;
		}

		if ( pass && name in jQuery.attrFn ) {
			return jQuery(elem)[name](value);
		}

		var notxml = elem.nodeType !== 1 || !jQuery.isXMLDoc( elem ),
			// Whether we are setting (or getting)
			set = value !== undefined;

		// Try to normalize/fix the name
		name = notxml && jQuery.props[ name ] || name;

		// Only do all the following if this is a node (faster for style)
		if ( elem.nodeType === 1 ) {
			// These attributes require special treatment
			var special = rspecialurl.test( name );

			// Safari mis-reports the default selected property of an option
			// Accessing the parent's selectedIndex property fixes it
			if ( name === "selected" && !jQuery.support.optSelected ) {
				var parent = elem.parentNode;
				if ( parent ) {
					parent.selectedIndex;
	
					// Make sure that it also works with optgroups, see #5701
					if ( parent.parentNode ) {
						parent.parentNode.selectedIndex;
					}
				}
			}

			// If applicable, access the attribute via the DOM 0 way
			if ( name in elem && notxml && !special ) {
				if ( set ) {
					// We can't allow the type property to be changed (since it causes problems in IE)
					if ( name === "type" && rtype.test( elem.nodeName ) && elem.parentNode ) {
						jQuery.error( "type property can't be changed" );
					}

					elem[ name ] = value;
				}

				// browsers index elements by id/name on forms, give priority to attributes.
				if ( jQuery.nodeName( elem, "form" ) && elem.getAttributeNode(name) ) {
					return elem.getAttributeNode( name ).nodeValue;
				}

				// elem.tabIndex doesn't always return the correct value when it hasn't been explicitly set
				// http://fluidproject.org/blog/2008/01/09/getting-setting-and-removing-tabindex-values-with-javascript/
				if ( name === "tabIndex" ) {
					var attributeNode = elem.getAttributeNode( "tabIndex" );

					return attributeNode && attributeNode.specified ?
						attributeNode.value :
						rfocusable.test( elem.nodeName ) || rclickable.test( elem.nodeName ) && elem.href ?
							0 :
							undefined;
				}

				return elem[ name ];
			}

			if ( !jQuery.support.style && notxml && name === "style" ) {
				if ( set ) {
					elem.style.cssText = "" + value;
				}

				return elem.style.cssText;
			}

			if ( set ) {
				// convert the value to a string (all browsers do this but IE) see #1070
				elem.setAttribute( name, "" + value );
			}

			var attr = !jQuery.support.hrefNormalized && notxml && special ?
					// Some attributes require a special call on IE
					elem.getAttribute( name, 2 ) :
					elem.getAttribute( name );

			// Non-existent attributes return null, we normalize to undefined
			return attr === null ? undefined : attr;
		}

		// elem is actually elem.style ... set the style
		// Using attr for specific style information is now deprecated. Use style insead.
		return jQuery.style( elem, name, value );
	}
});
var fcleanup = function( nm ) {
	return nm.replace(/[^\w\s\.\|`]/g, function( ch ) {
		return "\\" + ch;
	});
};

/*
 * A number of helper functions used for managing events.
 * Many of the ideas behind this code originated from
 * Dean Edwards' addEvent library.
 */
jQuery.event = {

	// Bind an event to an element
	// Original by Dean Edwards
	add: function( elem, types, handler, data ) {
		if ( elem.nodeType === 3 || elem.nodeType === 8 ) {
			return;
		}

		// For whatever reason, IE has trouble passing the window object
		// around, causing it to be cloned in the process
		if ( elem.setInterval && ( elem !== window && !elem.frameElement ) ) {
			elem = window;
		}

		// Make sure that the function being executed has a unique ID
		if ( !handler.guid ) {
			handler.guid = jQuery.guid++;
		}

		// if data is passed, bind to handler
		if ( data !== undefined ) {
			// Create temporary function pointer to original handler
			var fn = handler;

			// Create unique handler function, wrapped around original handler
			handler = jQuery.proxy( fn );

			// Store data in unique handler
			handler.data = data;
		}

		// Init the element's event structure
		var events = jQuery.data( elem, "events" ) || jQuery.data( elem, "events", {} ),
			handle = jQuery.data( elem, "handle" ), eventHandle;

		if ( !handle ) {
			eventHandle = function() {
				// Handle the second event of a trigger and when
				// an event is called after a page has unloaded
				return typeof jQuery !== "undefined" && !jQuery.event.triggered ?
					jQuery.event.handle.apply( eventHandle.elem, arguments ) :
					undefined;
			};

			handle = jQuery.data( elem, "handle", eventHandle );
		}

		// If no handle is found then we must be trying to bind to one of the
		// banned noData elements
		if ( !handle ) {
			return;
		}

		// Add elem as a property of the handle function
		// This is to prevent a memory leak with non-native
		// event in IE.
		handle.elem = elem;

		// Handle multiple events separated by a space
		// jQuery(...).bind("mouseover mouseout", fn);
		types = types.split( /\s+/ );

		var type, i = 0;

		while ( (type = types[ i++ ]) ) {
			// Namespaced event handlers
			var namespaces = type.split(".");
			type = namespaces.shift();

			if ( i > 1 ) {
				handler = jQuery.proxy( handler );

				if ( data !== undefined ) {
					handler.data = data;
				}
			}

			handler.type = namespaces.slice(0).sort().join(".");

			// Get the current list of functions bound to this event
			var handlers = events[ type ],
				special = this.special[ type ] || {};

			// Init the event handler queue
			if ( !handlers ) {
				handlers = events[ type ] = {};

				// Check for a special event handler
				// Only use addEventListener/attachEvent if the special
				// events handler returns false
				if ( !special.setup || special.setup.call( elem, data, namespaces, handler) === false ) {
					// Bind the global event handler to the element
					if ( elem.addEventListener ) {
						elem.addEventListener( type, handle, false );
					} else if ( elem.attachEvent ) {
						elem.attachEvent( "on" + type, handle );
					}
				}
			}
			
			if ( special.add ) { 
				var modifiedHandler = special.add.call( elem, handler, data, namespaces, handlers ); 
				if ( modifiedHandler && jQuery.isFunction( modifiedHandler ) ) { 
					modifiedHandler.guid = modifiedHandler.guid || handler.guid; 
					modifiedHandler.data = modifiedHandler.data || handler.data; 
					modifiedHandler.type = modifiedHandler.type || handler.type; 
					handler = modifiedHandler; 
				} 
			} 
			
			// Add the function to the element's handler list
			handlers[ handler.guid ] = handler;

			// Keep track of which events have been used, for global triggering
			this.global[ type ] = true;
		}

		// Nullify elem to prevent memory leaks in IE
		elem = null;
	},

	global: {},

	// Detach an event or set of events from an element
	remove: function( elem, types, handler ) {
		// don't do events on text and comment nodes
		if ( elem.nodeType === 3 || elem.nodeType === 8 ) {
			return;
		}

		var events = jQuery.data( elem, "events" ), ret, type, fn;

		if ( events ) {
			// Unbind all events for the element
			if ( types === undefined || (typeof types === "string" && types.charAt(0) === ".") ) {
				for ( type in events ) {
					this.remove( elem, type + (types || "") );
				}
			} else {
				// types is actually an event object here
				if ( types.type ) {
					handler = types.handler;
					types = types.type;
				}

				// Handle multiple events separated by a space
				// jQuery(...).unbind("mouseover mouseout", fn);
				types = types.split(/\s+/);
				var i = 0;
				while ( (type = types[ i++ ]) ) {
					// Namespaced event handlers
					var namespaces = type.split(".");
					type = namespaces.shift();
					var all = !namespaces.length,
						cleaned = jQuery.map( namespaces.slice(0).sort(), fcleanup ),
						namespace = new RegExp("(^|\\.)" + cleaned.join("\\.(?:.*\\.)?") + "(\\.|$)"),
						special = this.special[ type ] || {};

					if ( events[ type ] ) {
						// remove the given handler for the given type
						if ( handler ) {
							fn = events[ type ][ handler.guid ];
							delete events[ type ][ handler.guid ];

						// remove all handlers for the given type
						} else {
							for ( var handle in events[ type ] ) {
								// Handle the removal of namespaced events
								if ( all || namespace.test( events[ type ][ handle ].type ) ) {
									delete events[ type ][ handle ];
								}
							}
						}

						if ( special.remove ) {
							special.remove.call( elem, namespaces, fn);
						}

						// remove generic event handler if no more handlers exist
						for ( ret in events[ type ] ) {
							break;
						}
						if ( !ret ) {
							if ( !special.teardown || special.teardown.call( elem, namespaces ) === false ) {
								if ( elem.removeEventListener ) {
									elem.removeEventListener( type, jQuery.data( elem, "handle" ), false );
								} else if ( elem.detachEvent ) {
									elem.detachEvent( "on" + type, jQuery.data( elem, "handle" ) );
								}
							}
							ret = null;
							delete events[ type ];
						}
					}
				}
			}

			// Remove the expando if it's no longer used
			for ( ret in events ) {
				break;
			}
			if ( !ret ) {
				var handle = jQuery.data( elem, "handle" );
				if ( handle ) {
					handle.elem = null;
				}
				jQuery.removeData( elem, "events" );
				jQuery.removeData( elem, "handle" );
			}
		}
	},

	// bubbling is internal
	trigger: function( event, data, elem /*, bubbling */ ) {
		// Event object or event type
		var type = event.type || event,
			bubbling = arguments[3];

		if ( !bubbling ) {
			event = typeof event === "object" ?
				// jQuery.Event object
				event[expando] ? event :
				// Object literal
				jQuery.extend( jQuery.Event(type), event ) :
				// Just the event type (string)
				jQuery.Event(type);

			if ( type.indexOf("!") >= 0 ) {
				event.type = type = type.slice(0, -1);
				event.exclusive = true;
			}

			// Handle a global trigger
			if ( !elem ) {
				// Don't bubble custom events when global (to avoid too much overhead)
				event.stopPropagation();

				// Only trigger if we've ever bound an event for it
				if ( this.global[ type ] ) {
					jQuery.each( jQuery.cache, function() {
						if ( this.events && this.events[type] ) {
							jQuery.event.trigger( event, data, this.handle.elem );
						}
					});
				}
			}

			// Handle triggering a single element

			// don't do events on text and comment nodes
			if ( !elem || elem.nodeType === 3 || elem.nodeType === 8 ) {
				return undefined;
			}

			// Clean up in case it is reused
			event.result = undefined;
			event.target = elem;

			// Clone the incoming data, if any
			data = jQuery.makeArray( data );
			data.unshift( event );
		}

		event.currentTarget = elem;

		// Trigger the event, it is assumed that "handle" is a function
		var handle = jQuery.data( elem, "handle" );
		if ( handle ) {
			handle.apply( elem, data );
		}

		var parent = elem.parentNode || elem.ownerDocument;

		// Trigger an inline bound script
		try {
			if ( !(elem && elem.nodeName && jQuery.noData[elem.nodeName.toLowerCase()]) ) {
				if ( elem[ "on" + type ] && elem[ "on" + type ].apply( elem, data ) === false ) {
					event.result = false;
				}
			}

		// prevent IE from throwing an error for some elements with some event types, see #3533
		} catch (e) {}

		if ( !event.isPropagationStopped() && parent ) {
			jQuery.event.trigger( event, data, parent, true );

		} else if ( !event.isDefaultPrevented() ) {
			var target = event.target, old,
				isClick = jQuery.nodeName(target, "a") && type === "click";

			if ( !isClick && !(target && target.nodeName && jQuery.noData[target.nodeName.toLowerCase()]) ) {
				try {
					if ( target[ type ] ) {
						// Make sure that we don't accidentally re-trigger the onFOO events
						old = target[ "on" + type ];

						if ( old ) {
							target[ "on" + type ] = null;
						}

						this.triggered = true;
						target[ type ]();
					}

				// prevent IE from throwing an error for some elements with some event types, see #3533
				} catch (e) {}

				if ( old ) {
					target[ "on" + type ] = old;
				}

				this.triggered = false;
			}
		}
	},

	handle: function( event ) {
		// returned undefined or false
		var all, handlers;

		event = arguments[0] = jQuery.event.fix( event || window.event );
		event.currentTarget = this;

		// Namespaced event handlers
		var namespaces = event.type.split(".");
		event.type = namespaces.shift();

		// Cache this now, all = true means, any handler
		all = !namespaces.length && !event.exclusive;

		var namespace = new RegExp("(^|\\.)" + namespaces.slice(0).sort().join("\\.(?:.*\\.)?") + "(\\.|$)");

		handlers = ( jQuery.data(this, "events") || {} )[ event.type ];

		for ( var j in handlers ) {
			var handler = handlers[ j ];

			// Filter the functions by class
			if ( all || namespace.test(handler.type) ) {
				// Pass in a reference to the handler function itself
				// So that we can later remove it
				event.handler = handler;
				event.data = handler.data;

				var ret = handler.apply( this, arguments );

				if ( ret !== undefined ) {
					event.result = ret;
					if ( ret === false ) {
						event.preventDefault();
						event.stopPropagation();
					}
				}

				if ( event.isImmediatePropagationStopped() ) {
					break;
				}

			}
		}

		return event.result;
	},

	props: "altKey attrChange attrName bubbles button cancelable charCode clientX clientY ctrlKey currentTarget data detail eventPhase fromElement handler keyCode layerX layerY metaKey newValue offsetX offsetY originalTarget pageX pageY prevValue relatedNode relatedTarget screenX screenY shiftKey srcElement target toElement view wheelDelta which".split(" "),

	fix: function( event ) {
		if ( event[ expando ] ) {
			return event;
		}

		// store a copy of the original event object
		// and "clone" to set read-only properties
		var originalEvent = event;
		event = jQuery.Event( originalEvent );

		for ( var i = this.props.length, prop; i; ) {
			prop = this.props[ --i ];
			event[ prop ] = originalEvent[ prop ];
		}

		// Fix target property, if necessary
		if ( !event.target ) {
			event.target = event.srcElement || document; // Fixes #1925 where srcElement might not be defined either
		}

		// check if target is a textnode (safari)
		if ( event.target.nodeType === 3 ) {
			event.target = event.target.parentNode;
		}

		// Add relatedTarget, if necessary
		if ( !event.relatedTarget && event.fromElement ) {
			event.relatedTarget = event.fromElement === event.target ? event.toElement : event.fromElement;
		}

		// Calculate pageX/Y if missing and clientX/Y available
		if ( event.pageX == null && event.clientX != null ) {
			var doc = document.documentElement, body = document.body;
			event.pageX = event.clientX + (doc && doc.scrollLeft || body && body.scrollLeft || 0) - (doc && doc.clientLeft || body && body.clientLeft || 0);
			event.pageY = event.clientY + (doc && doc.scrollTop  || body && body.scrollTop  || 0) - (doc && doc.clientTop  || body && body.clientTop  || 0);
		}

		// Add which for key events
		if ( !event.which && ((event.charCode || event.charCode === 0) ? event.charCode : event.keyCode) ) {
			event.which = event.charCode || event.keyCode;
		}

		// Add metaKey to non-Mac browsers (use ctrl for PC's and Meta for Macs)
		if ( !event.metaKey && event.ctrlKey ) {
			event.metaKey = event.ctrlKey;
		}

		// Add which for click: 1 === left; 2 === middle; 3 === right
		// Note: button is not normalized, so don't use it
		if ( !event.which && event.button !== undefined ) {
			event.which = (event.button & 1 ? 1 : ( event.button & 2 ? 3 : ( event.button & 4 ? 2 : 0 ) ));
		}

		return event;
	},

	// Deprecated, use jQuery.guid instead
	guid: 1E8,

	// Deprecated, use jQuery.proxy instead
	proxy: jQuery.proxy,

	special: {
		ready: {
			// Make sure the ready event is setup
			setup: jQuery.bindReady,
			teardown: jQuery.noop
		},

		live: {
			add: function( proxy, data, namespaces, live ) {
				jQuery.extend( proxy, data || {} );

				proxy.guid += data.selector + data.live; 
				data.liveProxy = proxy;

				jQuery.event.add( this, data.live, liveHandler, data ); 
				
			},

			remove: function( namespaces ) {
				if ( namespaces.length ) {
					var remove = 0, name = new RegExp("(^|\\.)" + namespaces[0] + "(\\.|$)");

					jQuery.each( (jQuery.data(this, "events").live || {}), function() {
						if ( name.test(this.type) ) {
							remove++;
						}
					});

					if ( remove < 1 ) {
						jQuery.event.remove( this, namespaces[0], liveHandler );
					}
				}
			},
			special: {}
		},
		beforeunload: {
			setup: function( data, namespaces, fn ) {
				// We only want to do this special case on windows
				if ( this.setInterval ) {
					this.onbeforeunload = fn;
				}

				return false;
			},
			teardown: function( namespaces, fn ) {
				if ( this.onbeforeunload === fn ) {
					this.onbeforeunload = null;
				}
			}
		}
	}
};

jQuery.Event = function( src ) {
	// Allow instantiation without the 'new' keyword
	if ( !this.preventDefault ) {
		return new jQuery.Event( src );
	}

	// Event object
	if ( src && src.type ) {
		this.originalEvent = src;
		this.type = src.type;
	// Event type
	} else {
		this.type = src;
	}

	// timeStamp is buggy for some events on Firefox(#3843)
	// So we won't rely on the native value
	this.timeStamp = now();

	// Mark it as fixed
	this[ expando ] = true;
};

function returnFalse() {
	return false;
}
function returnTrue() {
	return true;
}

// jQuery.Event is based on DOM3 Events as specified by the ECMAScript Language Binding
// http://www.w3.org/TR/2003/WD-DOM-Level-3-Events-20030331/ecma-script-binding.html
jQuery.Event.prototype = {
	preventDefault: function() {
		this.isDefaultPrevented = returnTrue;

		var e = this.originalEvent;
		if ( !e ) {
			return;
		}
		
		// if preventDefault exists run it on the original event
		if ( e.preventDefault ) {
			e.preventDefault();
		}
		// otherwise set the returnValue property of the original event to false (IE)
		e.returnValue = false;
	},
	stopPropagation: function() {
		this.isPropagationStopped = returnTrue;

		var e = this.originalEvent;
		if ( !e ) {
			return;
		}
		// if stopPropagation exists run it on the original event
		if ( e.stopPropagation ) {
			e.stopPropagation();
		}
		// otherwise set the cancelBubble property of the original event to true (IE)
		e.cancelBubble = true;
	},
	stopImmediatePropagation: function() {
		this.isImmediatePropagationStopped = returnTrue;
		this.stopPropagation();
	},
	isDefaultPrevented: returnFalse,
	isPropagationStopped: returnFalse,
	isImmediatePropagationStopped: returnFalse
};

// Checks if an event happened on an element within another element
// Used in jQuery.event.special.mouseenter and mouseleave handlers
var withinElement = function( event ) {
	// Check if mouse(over|out) are still within the same parent element
	var parent = event.relatedTarget;

	// Traverse up the tree
	while ( parent && parent !== this ) {
		// Firefox sometimes assigns relatedTarget a XUL element
		// which we cannot access the parentNode property of
		try {
			parent = parent.parentNode;

		// assuming we've left the element since we most likely mousedover a xul element
		} catch(e) {
			break;
		}
	}

	if ( parent !== this ) {
		// set the correct event type
		event.type = event.data;

		// handle event if we actually just moused on to a non sub-element
		jQuery.event.handle.apply( this, arguments );
	}

},

// In case of event delegation, we only need to rename the event.type,
// liveHandler will take care of the rest.
delegate = function( event ) {
	event.type = event.data;
	jQuery.event.handle.apply( this, arguments );
};

// Create mouseenter and mouseleave events
jQuery.each({
	mouseenter: "mouseover",
	mouseleave: "mouseout"
}, function( orig, fix ) {
	jQuery.event.special[ orig ] = {
		setup: function( data ) {
			jQuery.event.add( this, fix, data && data.selector ? delegate : withinElement, orig );
		},
		teardown: function( data ) {
			jQuery.event.remove( this, fix, data && data.selector ? delegate : withinElement );
		}
	};
});

// submit delegation
if ( !jQuery.support.submitBubbles ) {

jQuery.event.special.submit = {
	setup: function( data, namespaces, fn ) {
		if ( this.nodeName.toLowerCase() !== "form" ) {
			jQuery.event.add(this, "click.specialSubmit." + fn.guid, function( e ) {
				var elem = e.target, type = elem.type;

				if ( (type === "submit" || type === "image") && jQuery( elem ).closest("form").length ) {
					return trigger( "submit", this, arguments );
				}
			});
	 
			jQuery.event.add(this, "keypress.specialSubmit." + fn.guid, function( e ) {
				var elem = e.target, type = elem.type;

				if ( (type === "text" || type === "password") && jQuery( elem ).closest("form").length && e.keyCode === 13 ) {
					return trigger( "submit", this, arguments );
				}
			});

		} else {
			return false;
		}
	},

	remove: function( namespaces, fn ) {
		jQuery.event.remove( this, "click.specialSubmit" + (fn ? "."+fn.guid : "") );
		jQuery.event.remove( this, "keypress.specialSubmit" + (fn ? "."+fn.guid : "") );
	}
};

}

// change delegation, happens here so we have bind.
if ( !jQuery.support.changeBubbles ) {

var formElems = /textarea|input|select/i;

function getVal( elem ) {
	var type = elem.type, val = elem.value;

	if ( type === "radio" || type === "checkbox" ) {
		val = elem.checked;

	} else if ( type === "select-multiple" ) {
		val = elem.selectedIndex > -1 ?
			jQuery.map( elem.options, function( elem ) {
				return elem.selected;
			}).join("-") :
			"";

	} else if ( elem.nodeName.toLowerCase() === "select" ) {
		val = elem.selectedIndex;
	}

	return val;
}

function testChange( e ) {
		var elem = e.target, data, val;

		if ( !formElems.test( elem.nodeName ) || elem.readOnly ) {
			return;
		}

		data = jQuery.data( elem, "_change_data" );
		val = getVal(elem);

		// the current data will be also retrieved by beforeactivate
		if ( e.type !== "focusout" || elem.type !== "radio" ) {
			jQuery.data( elem, "_change_data", val );
		}
		
		if ( data === undefined || val === data ) {
			return;
		}

		if ( data != null || val ) {
			e.type = "change";
			return jQuery.event.trigger( e, arguments[1], elem );
		}
}

jQuery.event.special.change = {
	filters: {
		focusout: testChange, 

		click: function( e ) {
			var elem = e.target, type = elem.type;

			if ( type === "radio" || type === "checkbox" || elem.nodeName.toLowerCase() === "select" ) {
				return testChange.call( this, e );
			}
		},

		// Change has to be called before submit
		// Keydown will be called before keypress, which is used in submit-event delegation
		keydown: function( e ) {
			var elem = e.target, type = elem.type;

			if ( (e.keyCode === 13 && elem.nodeName.toLowerCase() !== "textarea") ||
				(e.keyCode === 32 && (type === "checkbox" || type === "radio")) ||
				type === "select-multiple" ) {
				return testChange.call( this, e );
			}
		},

		// Beforeactivate happens also before the previous element is blurred
		// with this event you can't trigger a change event, but you can store
		// information/focus[in] is not needed anymore
		beforeactivate: function( e ) {
			var elem = e.target;

			if ( elem.nodeName.toLowerCase() === "input" && elem.type === "radio" ) {
				jQuery.data( elem, "_change_data", getVal(elem) );
			}
		}
	},
	setup: function( data, namespaces, fn ) {
		for ( var type in changeFilters ) {
			jQuery.event.add( this, type + ".specialChange." + fn.guid, changeFilters[type] );
		}

		return formElems.test( this.nodeName );
	},
	remove: function( namespaces, fn ) {
		for ( var type in changeFilters ) {
			jQuery.event.remove( this, type + ".specialChange" + (fn ? "."+fn.guid : ""), changeFilters[type] );
		}

		return formElems.test( this.nodeName );
	}
};

var changeFilters = jQuery.event.special.change.filters;

}

function trigger( type, elem, args ) {
	args[0].type = type;
	return jQuery.event.handle.apply( elem, args );
}

// Create "bubbling" focus and blur events
if ( document.addEventListener ) {
	jQuery.each({ focus: "focusin", blur: "focusout" }, function( orig, fix ) {
		jQuery.event.special[ fix ] = {
			setup: function() {
				this.addEventListener( orig, handler, true );
			}, 
			teardown: function() { 
				this.removeEventListener( orig, handler, true );
			}
		};

		function handler( e ) { 
			e = jQuery.event.fix( e );
			e.type = fix;
			return jQuery.event.handle.call( this, e );
		}
	});
}

jQuery.each(["bind", "one"], function( i, name ) {
	jQuery.fn[ name ] = function( type, data, fn ) {
		// Handle object literals
		if ( typeof type === "object" ) {
			for ( var key in type ) {
				this[ name ](key, data, type[key], fn);
			}
			return this;
		}
		
		if ( jQuery.isFunction( data ) ) {
			fn = data;
			data = undefined;
		}

		var handler = name === "one" ? jQuery.proxy( fn, function( event ) {
			jQuery( this ).unbind( event, handler );
			return fn.apply( this, arguments );
		}) : fn;

		return type === "unload" && name !== "one" ?
			this.one( type, data, fn ) :
			this.each(function() {
				jQuery.event.add( this, type, handler, data );
			});
	};
});

jQuery.fn.extend({
	unbind: function( type, fn ) {
		// Handle object literals
		if ( typeof type === "object" && !type.preventDefault ) {
			for ( var key in type ) {
				this.unbind(key, type[key]);
			}
			return this;
		}

		return this.each(function() {
			jQuery.event.remove( this, type, fn );
		});
	},
	trigger: function( type, data ) {
		return this.each(function() {
			jQuery.event.trigger( type, data, this );
		});
	},

	triggerHandler: function( type, data ) {
		if ( this[0] ) {
			var event = jQuery.Event( type );
			event.preventDefault();
			event.stopPropagation();
			jQuery.event.trigger( event, data, this[0] );
			return event.result;
		}
	},

	toggle: function( fn ) {
		// Save reference to arguments for access in closure
		var args = arguments, i = 1;

		// link all the functions, so any of them can unbind this click handler
		while ( i < args.length ) {
			jQuery.proxy( fn, args[ i++ ] );
		}

		return this.click( jQuery.proxy( fn, function( event ) {
			// Figure out which function to execute
			var lastToggle = ( jQuery.data( this, "lastToggle" + fn.guid ) || 0 ) % i;
			jQuery.data( this, "lastToggle" + fn.guid, lastToggle + 1 );

			// Make sure that clicks stop
			event.preventDefault();

			// and execute the function
			return args[ lastToggle ].apply( this, arguments ) || false;
		}));
	},

	hover: function( fnOver, fnOut ) {
		return this.mouseenter( fnOver ).mouseleave( fnOut || fnOver );
	}
});

jQuery.each(["live", "die"], function( i, name ) {
	jQuery.fn[ name ] = function( types, data, fn ) {
		var type, i = 0;

		if ( jQuery.isFunction( data ) ) {
			fn = data;
			data = undefined;
		}

		types = (types || "").split( /\s+/ );

		while ( (type = types[ i++ ]) != null ) {
			type = type === "focus" ? "focusin" : // focus --> focusin
					type === "blur" ? "focusout" : // blur --> focusout
					type === "hover" ? types.push("mouseleave") && "mouseenter" : // hover support
					type;
			
			if ( name === "live" ) {
				// bind live handler
				jQuery( this.context ).bind( liveConvert( type, this.selector ), {
					data: data, selector: this.selector, live: type
				}, fn );

			} else {
				// unbind live handler
				jQuery( this.context ).unbind( liveConvert( type, this.selector ), fn ? { guid: fn.guid + this.selector + type } : null );
			}
		}
		
		return this;
	}
});

function liveHandler( event ) {
	var stop, elems = [], selectors = [], args = arguments,
		related, match, fn, elem, j, i, l, data,
		live = jQuery.extend({}, jQuery.data( this, "events" ).live);

	// Make sure we avoid non-left-click bubbling in Firefox (#3861)
	if ( event.button && event.type === "click" ) {
		return;
	}

	for ( j in live ) {
		fn = live[j];
		if ( fn.live === event.type ||
				fn.altLive && jQuery.inArray(event.type, fn.altLive) > -1 ) {

			data = fn.data;
			if ( !(data.beforeFilter && data.beforeFilter[event.type] && 
					!data.beforeFilter[event.type](event)) ) {
				selectors.push( fn.selector );
			}
		} else {
			delete live[j];
		}
	}

	match = jQuery( event.target ).closest( selectors, event.currentTarget );

	for ( i = 0, l = match.length; i < l; i++ ) {
		for ( j in live ) {
			fn = live[j];
			elem = match[i].elem;
			related = null;

			if ( match[i].selector === fn.selector ) {
				// Those two events require additional checking
				if ( fn.live === "mouseenter" || fn.live === "mouseleave" ) {
					related = jQuery( event.relatedTarget ).closest( fn.selector )[0];
				}

				if ( !related || related !== elem ) {
					elems.push({ elem: elem, fn: fn });
				}
			}
		}
	}

	for ( i = 0, l = elems.length; i < l; i++ ) {
		match = elems[i];
		event.currentTarget = match.elem;
		event.data = match.fn.data;
		if ( match.fn.apply( match.elem, args ) === false ) {
			stop = false;
			break;
		}
	}

	return stop;
}

function liveConvert( type, selector ) {
	return "live." + (type ? type + "." : "") + selector.replace(/\./g, "`").replace(/ /g, "&");
}

jQuery.each( ("blur focus focusin focusout load resize scroll unload click dblclick " +
	"mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave " +
	"change select submit keydown keypress keyup error").split(" "), function( i, name ) {

	// Handle event binding
	jQuery.fn[ name ] = function( fn ) {
		return fn ? this.bind( name, fn ) : this.trigger( name );
	};

	if ( jQuery.attrFn ) {
		jQuery.attrFn[ name ] = true;
	}
});

// Prevent memory leaks in IE
// Window isn't included so as not to unbind existing unload events
// More info:
//  - http://isaacschlueter.com/2006/10/msie-memory-leaks/
if ( window.attachEvent && !window.addEventListener ) {
	window.attachEvent("onunload", function() {
		for ( var id in jQuery.cache ) {
			if ( jQuery.cache[ id ].handle ) {
				// Try/Catch is to handle iframes being unloaded, see #4280
				try {
					jQuery.event.remove( jQuery.cache[ id ].handle.elem );
				} catch(e) {}
			}
		}
	});
}
/*!
 * Sizzle CSS Selector Engine - v1.0
 *  Copyright 2009, The Dojo Foundation
 *  Released under the MIT, BSD, and GPL Licenses.
 *  More information: http://sizzlejs.com/
 */
(function(){

var chunker = /((?:\((?:\([^()]+\)|[^()]+)+\)|\[(?:\[[^[\]]*\]|['"][^'"]*['"]|[^[\]'"]+)+\]|\\.|[^ >+~,(\[\\]+)+|[>+~])(\s*,\s*)?((?:.|\r|\n)*)/g,
	done = 0,
	toString = Object.prototype.toString,
	hasDuplicate = false,
	baseHasDuplicate = true;

// Here we check if the JavaScript engine is using some sort of
// optimization where it does not always call our comparision
// function. If that is the case, discard the hasDuplicate value.
//   Thus far that includes Google Chrome.
[0, 0].sort(function(){
	baseHasDuplicate = false;
	return 0;
});

var Sizzle = function(selector, context, results, seed) {
	results = results || [];
	var origContext = context = context || document;

	if ( context.nodeType !== 1 && context.nodeType !== 9 ) {
		return [];
	}
	
	if ( !selector || typeof selector !== "string" ) {
		return results;
	}

	var parts = [], m, set, checkSet, extra, prune = true, contextXML = isXML(context),
		soFar = selector;
	
	// Reset the position of the chunker regexp (start from head)
	while ( (chunker.exec(""), m = chunker.exec(soFar)) !== null ) {
		soFar = m[3];
		
		parts.push( m[1] );
		
		if ( m[2] ) {
			extra = m[3];
			break;
		}
	}

	if ( parts.length > 1 && origPOS.exec( selector ) ) {
		if ( parts.length === 2 && Expr.relative[ parts[0] ] ) {
			set = posProcess( parts[0] + parts[1], context );
		} else {
			set = Expr.relative[ parts[0] ] ?
				[ context ] :
				Sizzle( parts.shift(), context );

			while ( parts.length ) {
				selector = parts.shift();

				if ( Expr.relative[ selector ] ) {
					selector += parts.shift();
				}
				
				set = posProcess( selector, set );
			}
		}
	} else {
		// Take a shortcut and set the context if the root selector is an ID
		// (but not if it'll be faster if the inner selector is an ID)
		if ( !seed && parts.length > 1 && context.nodeType === 9 && !contextXML &&
				Expr.match.ID.test(parts[0]) && !Expr.match.ID.test(parts[parts.length - 1]) ) {
			var ret = Sizzle.find( parts.shift(), context, contextXML );
			context = ret.expr ? Sizzle.filter( ret.expr, ret.set )[0] : ret.set[0];
		}

		if ( context ) {
			var ret = seed ?
				{ expr: parts.pop(), set: makeArray(seed) } :
				Sizzle.find( parts.pop(), parts.length === 1 && (parts[0] === "~" || parts[0] === "+") && context.parentNode ? context.parentNode : context, contextXML );
			set = ret.expr ? Sizzle.filter( ret.expr, ret.set ) : ret.set;

			if ( parts.length > 0 ) {
				checkSet = makeArray(set);
			} else {
				prune = false;
			}

			while ( parts.length ) {
				var cur = parts.pop(), pop = cur;

				if ( !Expr.relative[ cur ] ) {
					cur = "";
				} else {
					pop = parts.pop();
				}

				if ( pop == null ) {
					pop = context;
				}

				Expr.relative[ cur ]( checkSet, pop, contextXML );
			}
		} else {
			checkSet = parts = [];
		}
	}

	if ( !checkSet ) {
		checkSet = set;
	}

	if ( !checkSet ) {
		Sizzle.error( cur || selector );
	}

	if ( toString.call(checkSet) === "[object Array]" ) {
		if ( !prune ) {
			results.push.apply( results, checkSet );
		} else if ( context && context.nodeType === 1 ) {
			for ( var i = 0; checkSet[i] != null; i++ ) {
				if ( checkSet[i] && (checkSet[i] === true || checkSet[i].nodeType === 1 && contains(context, checkSet[i])) ) {
					results.push( set[i] );
				}
			}
		} else {
			for ( var i = 0; checkSet[i] != null; i++ ) {
				if ( checkSet[i] && checkSet[i].nodeType === 1 ) {
					results.push( set[i] );
				}
			}
		}
	} else {
		makeArray( checkSet, results );
	}

	if ( extra ) {
		Sizzle( extra, origContext, results, seed );
		Sizzle.uniqueSort( results );
	}

	return results;
};

Sizzle.uniqueSort = function(results){
	if ( sortOrder ) {
		hasDuplicate = baseHasDuplicate;
		results.sort(sortOrder);

		if ( hasDuplicate ) {
			for ( var i = 1; i < results.length; i++ ) {
				if ( results[i] === results[i-1] ) {
					results.splice(i--, 1);
				}
			}
		}
	}

	return results;
};

Sizzle.matches = function(expr, set){
	return Sizzle(expr, null, null, set);
};

Sizzle.find = function(expr, context, isXML){
	var set, match;

	if ( !expr ) {
		return [];
	}

	for ( var i = 0, l = Expr.order.length; i < l; i++ ) {
		var type = Expr.order[i], match;
		
		if ( (match = Expr.leftMatch[ type ].exec( expr )) ) {
			var left = match[1];
			match.splice(1,1);

			if ( left.substr( left.length - 1 ) !== "\\" ) {
				match[1] = (match[1] || "").replace(/\\/g, "");
				set = Expr.find[ type ]( match, context, isXML );
				if ( set != null ) {
					expr = expr.replace( Expr.match[ type ], "" );
					break;
				}
			}
		}
	}

	if ( !set ) {
		set = context.getElementsByTagName("*");
	}

	return {set: set, expr: expr};
};

Sizzle.filter = function(expr, set, inplace, not){
	var old = expr, result = [], curLoop = set, match, anyFound,
		isXMLFilter = set && set[0] && isXML(set[0]);

	while ( expr && set.length ) {
		for ( var type in Expr.filter ) {
			if ( (match = Expr.leftMatch[ type ].exec( expr )) != null && match[2] ) {
				var filter = Expr.filter[ type ], found, item, left = match[1];
				anyFound = false;

				match.splice(1,1);

				if ( left.substr( left.length - 1 ) === "\\" ) {
					continue;
				}

				if ( curLoop === result ) {
					result = [];
				}

				if ( Expr.preFilter[ type ] ) {
					match = Expr.preFilter[ type ]( match, curLoop, inplace, result, not, isXMLFilter );

					if ( !match ) {
						anyFound = found = true;
					} else if ( match === true ) {
						continue;
					}
				}

				if ( match ) {
					for ( var i = 0; (item = curLoop[i]) != null; i++ ) {
						if ( item ) {
							found = filter( item, match, i, curLoop );
							var pass = not ^ !!found;

							if ( inplace && found != null ) {
								if ( pass ) {
									anyFound = true;
								} else {
									curLoop[i] = false;
								}
							} else if ( pass ) {
								result.push( item );
								anyFound = true;
							}
						}
					}
				}

				if ( found !== undefined ) {
					if ( !inplace ) {
						curLoop = result;
					}

					expr = expr.replace( Expr.match[ type ], "" );

					if ( !anyFound ) {
						return [];
					}

					break;
				}
			}
		}

		// Improper expression
		if ( expr === old ) {
			if ( anyFound == null ) {
				Sizzle.error( expr );
			} else {
				break;
			}
		}

		old = expr;
	}

	return curLoop;
};

Sizzle.error = function( msg ) {
	throw "Syntax error, unrecognized expression: " + msg;
};

var Expr = Sizzle.selectors = {
	order: [ "ID", "NAME", "TAG" ],
	match: {
		ID: /#((?:[\w\u00c0-\uFFFF-]|\\.)+)/,
		CLASS: /\.((?:[\w\u00c0-\uFFFF-]|\\.)+)/,
		NAME: /\[name=['"]*((?:[\w\u00c0-\uFFFF-]|\\.)+)['"]*\]/,
		ATTR: /\[\s*((?:[\w\u00c0-\uFFFF-]|\\.)+)\s*(?:(\S?=)\s*(['"]*)(.*?)\3|)\s*\]/,
		TAG: /^((?:[\w\u00c0-\uFFFF\*-]|\\.)+)/,
		CHILD: /:(only|nth|last|first)-child(?:\((even|odd|[\dn+-]*)\))?/,
		POS: /:(nth|eq|gt|lt|first|last|even|odd)(?:\((\d*)\))?(?=[^-]|$)/,
		PSEUDO: /:((?:[\w\u00c0-\uFFFF-]|\\.)+)(?:\((['"]?)((?:\([^\)]+\)|[^\(\)]*)+)\2\))?/
	},
	leftMatch: {},
	attrMap: {
		"class": "className",
		"for": "htmlFor"
	},
	attrHandle: {
		href: function(elem){
			return elem.getAttribute("href");
		}
	},
	relative: {
		"+": function(checkSet, part){
			var isPartStr = typeof part === "string",
				isTag = isPartStr && !/\W/.test(part),
				isPartStrNotTag = isPartStr && !isTag;

			if ( isTag ) {
				part = part.toLowerCase();
			}

			for ( var i = 0, l = checkSet.length, elem; i < l; i++ ) {
				if ( (elem = checkSet[i]) ) {
					while ( (elem = elem.previousSibling) && elem.nodeType !== 1 ) {}

					checkSet[i] = isPartStrNotTag || elem && elem.nodeName.toLowerCase() === part ?
						elem || false :
						elem === part;
				}
			}

			if ( isPartStrNotTag ) {
				Sizzle.filter( part, checkSet, true );
			}
		},
		">": function(checkSet, part){
			var isPartStr = typeof part === "string";

			if ( isPartStr && !/\W/.test(part) ) {
				part = part.toLowerCase();

				for ( var i = 0, l = checkSet.length; i < l; i++ ) {
					var elem = checkSet[i];
					if ( elem ) {
						var parent = elem.parentNode;
						checkSet[i] = parent.nodeName.toLowerCase() === part ? parent : false;
					}
				}
			} else {
				for ( var i = 0, l = checkSet.length; i < l; i++ ) {
					var elem = checkSet[i];
					if ( elem ) {
						checkSet[i] = isPartStr ?
							elem.parentNode :
							elem.parentNode === part;
					}
				}

				if ( isPartStr ) {
					Sizzle.filter( part, checkSet, true );
				}
			}
		},
		"": function(checkSet, part, isXML){
			var doneName = done++, checkFn = dirCheck;

			if ( typeof part === "string" && !/\W/.test(part) ) {
				var nodeCheck = part = part.toLowerCase();
				checkFn = dirNodeCheck;
			}

			checkFn("parentNode", part, doneName, checkSet, nodeCheck, isXML);
		},
		"~": function(checkSet, part, isXML){
			var doneName = done++, checkFn = dirCheck;

			if ( typeof part === "string" && !/\W/.test(part) ) {
				var nodeCheck = part = part.toLowerCase();
				checkFn = dirNodeCheck;
			}

			checkFn("previousSibling", part, doneName, checkSet, nodeCheck, isXML);
		}
	},
	find: {
		ID: function(match, context, isXML){
			if ( typeof context.getElementById !== "undefined" && !isXML ) {
				var m = context.getElementById(match[1]);
				return m ? [m] : [];
			}
		},
		NAME: function(match, context){
			if ( typeof context.getElementsByName !== "undefined" ) {
				var ret = [], results = context.getElementsByName(match[1]);

				for ( var i = 0, l = results.length; i < l; i++ ) {
					if ( results[i].getAttribute("name") === match[1] ) {
						ret.push( results[i] );
					}
				}

				return ret.length === 0 ? null : ret;
			}
		},
		TAG: function(match, context){
			return context.getElementsByTagName(match[1]);
		}
	},
	preFilter: {
		CLASS: function(match, curLoop, inplace, result, not, isXML){
			match = " " + match[1].replace(/\\/g, "") + " ";

			if ( isXML ) {
				return match;
			}

			for ( var i = 0, elem; (elem = curLoop[i]) != null; i++ ) {
				if ( elem ) {
					if ( not ^ (elem.className && (" " + elem.className + " ").replace(/[\t\n]/g, " ").indexOf(match) >= 0) ) {
						if ( !inplace ) {
							result.push( elem );
						}
					} else if ( inplace ) {
						curLoop[i] = false;
					}
				}
			}

			return false;
		},
		ID: function(match){
			return match[1].replace(/\\/g, "");
		},
		TAG: function(match, curLoop){
			return match[1].toLowerCase();
		},
		CHILD: function(match){
			if ( match[1] === "nth" ) {
				// parse equations like 'even', 'odd', '5', '2n', '3n+2', '4n-1', '-n+6'
				var test = /(-?)(\d*)n((?:\+|-)?\d*)/.exec(
					match[2] === "even" && "2n" || match[2] === "odd" && "2n+1" ||
					!/\D/.test( match[2] ) && "0n+" + match[2] || match[2]);

				// calculate the numbers (first)n+(last) including if they are negative
				match[2] = (test[1] + (test[2] || 1)) - 0;
				match[3] = test[3] - 0;
			}

			// TODO: Move to normal caching system
			match[0] = done++;

			return match;
		},
		ATTR: function(match, curLoop, inplace, result, not, isXML){
			var name = match[1].replace(/\\/g, "");
			
			if ( !isXML && Expr.attrMap[name] ) {
				match[1] = Expr.attrMap[name];
			}

			if ( match[2] === "~=" ) {
				match[4] = " " + match[4] + " ";
			}

			return match;
		},
		PSEUDO: function(match, curLoop, inplace, result, not){
			if ( match[1] === "not" ) {
				// If we're dealing with a complex expression, or a simple one
				if ( ( chunker.exec(match[3]) || "" ).length > 1 || /^\w/.test(match[3]) ) {
					match[3] = Sizzle(match[3], null, null, curLoop);
				} else {
					var ret = Sizzle.filter(match[3], curLoop, inplace, true ^ not);
					if ( !inplace ) {
						result.push.apply( result, ret );
					}
					return false;
				}
			} else if ( Expr.match.POS.test( match[0] ) || Expr.match.CHILD.test( match[0] ) ) {
				return true;
			}
			
			return match;
		},
		POS: function(match){
			match.unshift( true );
			return match;
		}
	},
	filters: {
		enabled: function(elem){
			return elem.disabled === false && elem.type !== "hidden";
		},
		disabled: function(elem){
			return elem.disabled === true;
		},
		checked: function(elem){
			return elem.checked === true;
		},
		selected: function(elem){
			// Accessing this property makes selected-by-default
			// options in Safari work properly
			elem.parentNode.selectedIndex;
			return elem.selected === true;
		},
		parent: function(elem){
			return !!elem.firstChild;
		},
		empty: function(elem){
			return !elem.firstChild;
		},
		has: function(elem, i, match){
			return !!Sizzle( match[3], elem ).length;
		},
		header: function(elem){
			return /h\d/i.test( elem.nodeName );
		},
		text: function(elem){
			return "text" === elem.type;
		},
		radio: function(elem){
			return "radio" === elem.type;
		},
		checkbox: function(elem){
			return "checkbox" === elem.type;
		},
		file: function(elem){
			return "file" === elem.type;
		},
		password: function(elem){
			return "password" === elem.type;
		},
		submit: function(elem){
			return "submit" === elem.type;
		},
		image: function(elem){
			return "image" === elem.type;
		},
		reset: function(elem){
			return "reset" === elem.type;
		},
		button: function(elem){
			return "button" === elem.type || elem.nodeName.toLowerCase() === "button";
		},
		input: function(elem){
			return /input|select|textarea|button/i.test(elem.nodeName);
		}
	},
	setFilters: {
		first: function(elem, i){
			return i === 0;
		},
		last: function(elem, i, match, array){
			return i === array.length - 1;
		},
		even: function(elem, i){
			return i % 2 === 0;
		},
		odd: function(elem, i){
			return i % 2 === 1;
		},
		lt: function(elem, i, match){
			return i < match[3] - 0;
		},
		gt: function(elem, i, match){
			return i > match[3] - 0;
		},
		nth: function(elem, i, match){
			return match[3] - 0 === i;
		},
		eq: function(elem, i, match){
			return match[3] - 0 === i;
		}
	},
	filter: {
		PSEUDO: function(elem, match, i, array){
			var name = match[1], filter = Expr.filters[ name ];

			if ( filter ) {
				return filter( elem, i, match, array );
			} else if ( name === "contains" ) {
				return (elem.textContent || elem.innerText || getText([ elem ]) || "").indexOf(match[3]) >= 0;
			} else if ( name === "not" ) {
				var not = match[3];

				for ( var i = 0, l = not.length; i < l; i++ ) {
					if ( not[i] === elem ) {
						return false;
					}
				}

				return true;
			} else {
				Sizzle.error( "Syntax error, unrecognized expression: " + name );
			}
		},
		CHILD: function(elem, match){
			var type = match[1], node = elem;
			switch (type) {
				case 'only':
				case 'first':
					while ( (node = node.previousSibling) )	 {
						if ( node.nodeType === 1 ) { 
							return false; 
						}
					}
					if ( type === "first" ) { 
						return true; 
					}
					node = elem;
				case 'last':
					while ( (node = node.nextSibling) )	 {
						if ( node.nodeType === 1 ) { 
							return false; 
						}
					}
					return true;
				case 'nth':
					var first = match[2], last = match[3];

					if ( first === 1 && last === 0 ) {
						return true;
					}
					
					var doneName = match[0],
						parent = elem.parentNode;
	
					if ( parent && (parent.sizcache !== doneName || !elem.nodeIndex) ) {
						var count = 0;
						for ( node = parent.firstChild; node; node = node.nextSibling ) {
							if ( node.nodeType === 1 ) {
								node.nodeIndex = ++count;
							}
						} 
						parent.sizcache = doneName;
					}
					
					var diff = elem.nodeIndex - last;
					if ( first === 0 ) {
						return diff === 0;
					} else {
						return ( diff % first === 0 && diff / first >= 0 );
					}
			}
		},
		ID: function(elem, match){
			return elem.nodeType === 1 && elem.getAttribute("id") === match;
		},
		TAG: function(elem, match){
			return (match === "*" && elem.nodeType === 1) || elem.nodeName.toLowerCase() === match;
		},
		CLASS: function(elem, match){
			return (" " + (elem.className || elem.getAttribute("class")) + " ")
				.indexOf( match ) > -1;
		},
		ATTR: function(elem, match){
			var name = match[1],
				result = Expr.attrHandle[ name ] ?
					Expr.attrHandle[ name ]( elem ) :
					elem[ name ] != null ?
						elem[ name ] :
						elem.getAttribute( name ),
				value = result + "",
				type = match[2],
				check = match[4];

			return result == null ?
				type === "!=" :
				type === "=" ?
				value === check :
				type === "*=" ?
				value.indexOf(check) >= 0 :
				type === "~=" ?
				(" " + value + " ").indexOf(check) >= 0 :
				!check ?
				value && result !== false :
				type === "!=" ?
				value !== check :
				type === "^=" ?
				value.indexOf(check) === 0 :
				type === "$=" ?
				value.substr(value.length - check.length) === check :
				type === "|=" ?
				value === check || value.substr(0, check.length + 1) === check + "-" :
				false;
		},
		POS: function(elem, match, i, array){
			var name = match[2], filter = Expr.setFilters[ name ];

			if ( filter ) {
				return filter( elem, i, match, array );
			}
		}
	}
};

var origPOS = Expr.match.POS;

for ( var type in Expr.match ) {
	Expr.match[ type ] = new RegExp( Expr.match[ type ].source + /(?![^\[]*\])(?![^\(]*\))/.source );
	Expr.leftMatch[ type ] = new RegExp( /(^(?:.|\r|\n)*?)/.source + Expr.match[ type ].source.replace(/\\(\d+)/g, function(all, num){
		return "\\" + (num - 0 + 1);
	}));
}

var makeArray = function(array, results) {
	array = Array.prototype.slice.call( array, 0 );

	if ( results ) {
		results.push.apply( results, array );
		return results;
	}
	
	return array;
};

// Perform a simple check to determine if the browser is capable of
// converting a NodeList to an array using builtin methods.
try {
	Array.prototype.slice.call( document.documentElement.childNodes, 0 );

// Provide a fallback method if it does not work
} catch(e){
	makeArray = function(array, results) {
		var ret = results || [];

		if ( toString.call(array) === "[object Array]" ) {
			Array.prototype.push.apply( ret, array );
		} else {
			if ( typeof array.length === "number" ) {
				for ( var i = 0, l = array.length; i < l; i++ ) {
					ret.push( array[i] );
				}
			} else {
				for ( var i = 0; array[i]; i++ ) {
					ret.push( array[i] );
				}
			}
		}

		return ret;
	};
}

var sortOrder;

if ( document.documentElement.compareDocumentPosition ) {
	sortOrder = function( a, b ) {
		if ( !a.compareDocumentPosition || !b.compareDocumentPosition ) {
			if ( a == b ) {
				hasDuplicate = true;
			}
			return a.compareDocumentPosition ? -1 : 1;
		}

		var ret = a.compareDocumentPosition(b) & 4 ? -1 : a === b ? 0 : 1;
		if ( ret === 0 ) {
			hasDuplicate = true;
		}
		return ret;
	};
} else if ( "sourceIndex" in document.documentElement ) {
	sortOrder = function( a, b ) {
		if ( !a.sourceIndex || !b.sourceIndex ) {
			if ( a == b ) {
				hasDuplicate = true;
			}
			return a.sourceIndex ? -1 : 1;
		}

		var ret = a.sourceIndex - b.sourceIndex;
		if ( ret === 0 ) {
			hasDuplicate = true;
		}
		return ret;
	};
} else if ( document.createRange ) {
	sortOrder = function( a, b ) {
		if ( !a.ownerDocument || !b.ownerDocument ) {
			if ( a == b ) {
				hasDuplicate = true;
			}
			return a.ownerDocument ? -1 : 1;
		}

		var aRange = a.ownerDocument.createRange(), bRange = b.ownerDocument.createRange();
		aRange.setStart(a, 0);
		aRange.setEnd(a, 0);
		bRange.setStart(b, 0);
		bRange.setEnd(b, 0);
		var ret = aRange.compareBoundaryPoints(Range.START_TO_END, bRange);
		if ( ret === 0 ) {
			hasDuplicate = true;
		}
		return ret;
	};
}

// Utility function for retreiving the text value of an array of DOM nodes
function getText( elems ) {
	var ret = "", elem;

	for ( var i = 0; elems[i]; i++ ) {
		elem = elems[i];

		// Get the text from text nodes and CDATA nodes
		if ( elem.nodeType === 3 || elem.nodeType === 4 ) {
			ret += elem.nodeValue;

		// Traverse everything else, except comment nodes
		} else if ( elem.nodeType !== 8 ) {
			ret += getText( elem.childNodes );
		}
	}

	return ret;
}

// Check to see if the browser returns elements by name when
// querying by getElementById (and provide a workaround)
(function(){
	// We're going to inject a fake input element with a specified name
	var form = document.createElement("div"),
		id = "script" + (new Date).getTime();
	form.innerHTML = "<a name='" + id + "'/>";

	// Inject it into the root element, check its status, and remove it quickly
	var root = document.documentElement;
	root.insertBefore( form, root.firstChild );

	// The workaround has to do additional checks after a getElementById
	// Which slows things down for other browsers (hence the branching)
	if ( document.getElementById( id ) ) {
		Expr.find.ID = function(match, context, isXML){
			if ( typeof context.getElementById !== "undefined" && !isXML ) {
				var m = context.getElementById(match[1]);
				return m ? m.id === match[1] || typeof m.getAttributeNode !== "undefined" && m.getAttributeNode("id").nodeValue === match[1] ? [m] : undefined : [];
			}
		};

		Expr.filter.ID = function(elem, match){
			var node = typeof elem.getAttributeNode !== "undefined" && elem.getAttributeNode("id");
			return elem.nodeType === 1 && node && node.nodeValue === match;
		};
	}

	root.removeChild( form );
	root = form = null; // release memory in IE
})();

(function(){
	// Check to see if the browser returns only elements
	// when doing getElementsByTagName("*")

	// Create a fake element
	var div = document.createElement("div");
	div.appendChild( document.createComment("") );

	// Make sure no comments are found
	if ( div.getElementsByTagName("*").length > 0 ) {
		Expr.find.TAG = function(match, context){
			var results = context.getElementsByTagName(match[1]);

			// Filter out possible comments
			if ( match[1] === "*" ) {
				var tmp = [];

				for ( var i = 0; results[i]; i++ ) {
					if ( results[i].nodeType === 1 ) {
						tmp.push( results[i] );
					}
				}

				results = tmp;
			}

			return results;
		};
	}

	// Check to see if an attribute returns normalized href attributes
	div.innerHTML = "<a href='#'></a>";
	if ( div.firstChild && typeof div.firstChild.getAttribute !== "undefined" &&
			div.firstChild.getAttribute("href") !== "#" ) {
		Expr.attrHandle.href = function(elem){
			return elem.getAttribute("href", 2);
		};
	}

	div = null; // release memory in IE
})();

if ( document.querySelectorAll ) {
	(function(){
		var oldSizzle = Sizzle, div = document.createElement("div");
		div.innerHTML = "<p class='TEST'></p>";

		// Safari can't handle uppercase or unicode characters when
		// in quirks mode.
		if ( div.querySelectorAll && div.querySelectorAll(".TEST").length === 0 ) {
			return;
		}
	
		Sizzle = function(query, context, extra, seed){
			context = context || document;

			// Only use querySelectorAll on non-XML documents
			// (ID selectors don't work in non-HTML documents)
			if ( !seed && context.nodeType === 9 && !isXML(context) ) {
				try {
					return makeArray( context.querySelectorAll(query), extra );
				} catch(e){}
			}
		
			return oldSizzle(query, context, extra, seed);
		};

		for ( var prop in oldSizzle ) {
			Sizzle[ prop ] = oldSizzle[ prop ];
		}

		div = null; // release memory in IE
	})();
}

(function(){
	var div = document.createElement("div");

	div.innerHTML = "<div class='test e'></div><div class='test'></div>";

	// Opera can't find a second classname (in 9.6)
	// Also, make sure that getElementsByClassName actually exists
	if ( !div.getElementsByClassName || div.getElementsByClassName("e").length === 0 ) {
		return;
	}

	// Safari caches class attributes, doesn't catch changes (in 3.2)
	div.lastChild.className = "e";

	if ( div.getElementsByClassName("e").length === 1 ) {
		return;
	}
	
	Expr.order.splice(1, 0, "CLASS");
	Expr.find.CLASS = function(match, context, isXML) {
		if ( typeof context.getElementsByClassName !== "undefined" && !isXML ) {
			return context.getElementsByClassName(match[1]);
		}
	};

	div = null; // release memory in IE
})();

function dirNodeCheck( dir, cur, doneName, checkSet, nodeCheck, isXML ) {
	for ( var i = 0, l = checkSet.length; i < l; i++ ) {
		var elem = checkSet[i];
		if ( elem ) {
			elem = elem[dir];
			var match = false;

			while ( elem ) {
				if ( elem.sizcache === doneName ) {
					match = checkSet[elem.sizset];
					break;
				}

				if ( elem.nodeType === 1 && !isXML ){
					elem.sizcache = doneName;
					elem.sizset = i;
				}

				if ( elem.nodeName.toLowerCase() === cur ) {
					match = elem;
					break;
				}

				elem = elem[dir];
			}

			checkSet[i] = match;
		}
	}
}

function dirCheck( dir, cur, doneName, checkSet, nodeCheck, isXML ) {
	for ( var i = 0, l = checkSet.length; i < l; i++ ) {
		var elem = checkSet[i];
		if ( elem ) {
			elem = elem[dir];
			var match = false;

			while ( elem ) {
				if ( elem.sizcache === doneName ) {
					match = checkSet[elem.sizset];
					break;
				}

				if ( elem.nodeType === 1 ) {
					if ( !isXML ) {
						elem.sizcache = doneName;
						elem.sizset = i;
					}
					if ( typeof cur !== "string" ) {
						if ( elem === cur ) {
							match = true;
							break;
						}

					} else if ( Sizzle.filter( cur, [elem] ).length > 0 ) {
						match = elem;
						break;
					}
				}

				elem = elem[dir];
			}

			checkSet[i] = match;
		}
	}
}

var contains = document.compareDocumentPosition ? function(a, b){
	return a.compareDocumentPosition(b) & 16;
} : function(a, b){
	return a !== b && (a.contains ? a.contains(b) : true);
};

var isXML = function(elem){
	// documentElement is verified for cases where it doesn't yet exist
	// (such as loading iframes in IE - #4833) 
	var documentElement = (elem ? elem.ownerDocument || elem : 0).documentElement;
	return documentElement ? documentElement.nodeName !== "HTML" : false;
};

var posProcess = function(selector, context){
	var tmpSet = [], later = "", match,
		root = context.nodeType ? [context] : context;

	// Position selectors must be done after the filter
	// And so must :not(positional) so we move all PSEUDOs to the end
	while ( (match = Expr.match.PSEUDO.exec( selector )) ) {
		later += match[0];
		selector = selector.replace( Expr.match.PSEUDO, "" );
	}

	selector = Expr.relative[selector] ? selector + "*" : selector;

	for ( var i = 0, l = root.length; i < l; i++ ) {
		Sizzle( selector, root[i], tmpSet );
	}

	return Sizzle.filter( later, tmpSet );
};

// EXPOSE
jQuery.find = Sizzle;
jQuery.expr = Sizzle.selectors;
jQuery.expr[":"] = jQuery.expr.filters;
jQuery.unique = Sizzle.uniqueSort;
jQuery.getText = getText;
jQuery.isXMLDoc = isXML;
jQuery.contains = contains;

return;

window.Sizzle = Sizzle;

})();
var runtil = /Until$/,
	rparentsprev = /^(?:parents|prevUntil|prevAll)/,
	// Note: This RegExp should be improved, or likely pulled from Sizzle
	rmultiselector = /,/,
	slice = Array.prototype.slice;

// Implement the identical functionality for filter and not
var winnow = function( elements, qualifier, keep ) {
	if ( jQuery.isFunction( qualifier ) ) {
		return jQuery.grep(elements, function( elem, i ) {
			return !!qualifier.call( elem, i, elem ) === keep;
		});

	} else if ( qualifier.nodeType ) {
		return jQuery.grep(elements, function( elem, i ) {
			return (elem === qualifier) === keep;
		});

	} else if ( typeof qualifier === "string" ) {
		var filtered = jQuery.grep(elements, function( elem ) {
			return elem.nodeType === 1;
		});

		if ( isSimple.test( qualifier ) ) {
			return jQuery.filter(qualifier, filtered, !keep);
		} else {
			qualifier = jQuery.filter( qualifier, filtered );
		}
	}

	return jQuery.grep(elements, function( elem, i ) {
		return (jQuery.inArray( elem, qualifier ) >= 0) === keep;
	});
};

jQuery.fn.extend({
	find: function( selector ) {
		var ret = this.pushStack( "", "find", selector ), length = 0;

		for ( var i = 0, l = this.length; i < l; i++ ) {
			length = ret.length;
			jQuery.find( selector, this[i], ret );

			if ( i > 0 ) {
				// Make sure that the results are unique
				for ( var n = length; n < ret.length; n++ ) {
					for ( var r = 0; r < length; r++ ) {
						if ( ret[r] === ret[n] ) {
							ret.splice(n--, 1);
							break;
						}
					}
				}
			}
		}

		return ret;
	},

	has: function( target ) {
		var targets = jQuery( target );
		return this.filter(function() {
			for ( var i = 0, l = targets.length; i < l; i++ ) {
				if ( jQuery.contains( this, targets[i] ) ) {
					return true;
				}
			}
		});
	},

	not: function( selector ) {
		return this.pushStack( winnow(this, selector, false), "not", selector);
	},

	filter: function( selector ) {
		return this.pushStack( winnow(this, selector, true), "filter", selector );
	},
	
	is: function( selector ) {
		return !!selector && jQuery.filter( selector, this ).length > 0;
	},

	closest: function( selectors, context ) {
		if ( jQuery.isArray( selectors ) ) {
			var ret = [], cur = this[0], match, matches = {}, selector;

			if ( cur && selectors.length ) {
				for ( var i = 0, l = selectors.length; i < l; i++ ) {
					selector = selectors[i];

					if ( !matches[selector] ) {
						matches[selector] = jQuery.expr.match.POS.test( selector ) ? 
							jQuery( selector, context || this.context ) :
							selector;
					}
				}

				while ( cur && cur.ownerDocument && cur !== context ) {
					for ( selector in matches ) {
						match = matches[selector];

						if ( match.jquery ? match.index(cur) > -1 : jQuery(cur).is(match) ) {
							ret.push({ selector: selector, elem: cur });
							delete matches[selector];
						}
					}
					cur = cur.parentNode;
				}
			}

			return ret;
		}

		var pos = jQuery.expr.match.POS.test( selectors ) ? 
			jQuery( selectors, context || this.context ) : null;

		return this.map(function( i, cur ) {
			while ( cur && cur.ownerDocument && cur !== context ) {
				if ( pos ? pos.index(cur) > -1 : jQuery(cur).is(selectors) ) {
					return cur;
				}
				cur = cur.parentNode;
			}
			return null;
		});
	},
	
	// Determine the position of an element within
	// the matched set of elements
	index: function( elem ) {
		if ( !elem || typeof elem === "string" ) {
			return jQuery.inArray( this[0],
				// If it receives a string, the selector is used
				// If it receives nothing, the siblings are used
				elem ? jQuery( elem ) : this.parent().children() );
		}
		// Locate the position of the desired element
		return jQuery.inArray(
			// If it receives a jQuery object, the first element is used
			elem.jquery ? elem[0] : elem, this );
	},

	add: function( selector, context ) {
		var set = typeof selector === "string" ?
				jQuery( selector, context || this.context ) :
				jQuery.makeArray( selector ),
			all = jQuery.merge( this.get(), set );

		return this.pushStack( isDisconnected( set[0] ) || isDisconnected( all[0] ) ?
			all :
			jQuery.unique( all ) );
	},

	andSelf: function() {
		return this.add( this.prevObject );
	}
});

// A painfully simple check to see if an element is disconnected
// from a document (should be improved, where feasible).
function isDisconnected( node ) {
	return !node || !node.parentNode || node.parentNode.nodeType === 11;
}

jQuery.each({
	parent: function( elem ) {
		var parent = elem.parentNode;
		return parent && parent.nodeType !== 11 ? parent : null;
	},
	parents: function( elem ) {
		return jQuery.dir( elem, "parentNode" );
	},
	parentsUntil: function( elem, i, until ) {
		return jQuery.dir( elem, "parentNode", until );
	},
	next: function( elem ) {
		return jQuery.nth( elem, 2, "nextSibling" );
	},
	prev: function( elem ) {
		return jQuery.nth( elem, 2, "previousSibling" );
	},
	nextAll: function( elem ) {
		return jQuery.dir( elem, "nextSibling" );
	},
	prevAll: function( elem ) {
		return jQuery.dir( elem, "previousSibling" );
	},
	nextUntil: function( elem, i, until ) {
		return jQuery.dir( elem, "nextSibling", until );
	},
	prevUntil: function( elem, i, until ) {
		return jQuery.dir( elem, "previousSibling", until );
	},
	siblings: function( elem ) {
		return jQuery.sibling( elem.parentNode.firstChild, elem );
	},
	children: function( elem ) {
		return jQuery.sibling( elem.firstChild );
	},
	contents: function( elem ) {
		return jQuery.nodeName( elem, "iframe" ) ?
			elem.contentDocument || elem.contentWindow.document :
			jQuery.makeArray( elem.childNodes );
	}
}, function( name, fn ) {
	jQuery.fn[ name ] = function( until, selector ) {
		var ret = jQuery.map( this, fn, until );
		
		if ( !runtil.test( name ) ) {
			selector = until;
		}

		if ( selector && typeof selector === "string" ) {
			ret = jQuery.filter( selector, ret );
		}

		ret = this.length > 1 ? jQuery.unique( ret ) : ret;

		if ( (this.length > 1 || rmultiselector.test( selector )) && rparentsprev.test( name ) ) {
			ret = ret.reverse();
		}

		return this.pushStack( ret, name, slice.call(arguments).join(",") );
	};
});

jQuery.extend({
	filter: function( expr, elems, not ) {
		if ( not ) {
			expr = ":not(" + expr + ")";
		}

		return jQuery.find.matches(expr, elems);
	},
	
	dir: function( elem, dir, until ) {
		var matched = [], cur = elem[dir];
		while ( cur && cur.nodeType !== 9 && (until === undefined || cur.nodeType !== 1 || !jQuery( cur ).is( until )) ) {
			if ( cur.nodeType === 1 ) {
				matched.push( cur );
			}
			cur = cur[dir];
		}
		return matched;
	},

	nth: function( cur, result, dir, elem ) {
		result = result || 1;
		var num = 0;

		for ( ; cur; cur = cur[dir] ) {
			if ( cur.nodeType === 1 && ++num === result ) {
				break;
			}
		}

		return cur;
	},

	sibling: function( n, elem ) {
		var r = [];

		for ( ; n; n = n.nextSibling ) {
			if ( n.nodeType === 1 && n !== elem ) {
				r.push( n );
			}
		}

		return r;
	}
});
var rinlinejQuery = / jQuery\d+="(?:\d+|null)"/g,
	rleadingWhitespace = /^\s+/,
	rxhtmlTag = /(<([\w:]+)[^>]*?)\/>/g,
	rselfClosing = /^(?:area|br|col|embed|hr|img|input|link|meta|param)$/i,
	rtagName = /<([\w:]+)/,
	rtbody = /<tbody/i,
	rhtml = /<|&\w+;/,
	rchecked = /checked\s*(?:[^=]|=\s*.checked.)/i,  // checked="checked" or checked (html5)
	fcloseTag = function( all, front, tag ) {
		return rselfClosing.test( tag ) ?
			all :
			front + "></" + tag + ">";
	},
	wrapMap = {
		option: [ 1, "<select multiple='multiple'>", "</select>" ],
		legend: [ 1, "<fieldset>", "</fieldset>" ],
		thead: [ 1, "<table>", "</table>" ],
		tr: [ 2, "<table><tbody>", "</tbody></table>" ],
		td: [ 3, "<table><tbody><tr>", "</tr></tbody></table>" ],
		col: [ 2, "<table><tbody></tbody><colgroup>", "</colgroup></table>" ],
		area: [ 1, "<map>", "</map>" ],
		_default: [ 0, "", "" ]
	};

wrapMap.optgroup = wrapMap.option;
wrapMap.tbody = wrapMap.tfoot = wrapMap.colgroup = wrapMap.caption = wrapMap.thead;
wrapMap.th = wrapMap.td;

// IE can't serialize <link> and <script> tags normally
if ( !jQuery.support.htmlSerialize ) {
	wrapMap._default = [ 1, "div<div>", "</div>" ];
}

jQuery.fn.extend({
	text: function( text ) {
		if ( jQuery.isFunction(text) ) {
			return this.each(function(i) {
				var self = jQuery(this);
				self.text( text.call(this, i, self.text()) );
			});
		}

		if ( typeof text !== "object" && text !== undefined ) {
			return this.empty().append( (this[0] && this[0].ownerDocument || document).createTextNode( text ) );
		}

		return jQuery.getText( this );
	},

	wrapAll: function( html ) {
		if ( jQuery.isFunction( html ) ) {
			return this.each(function(i) {
				jQuery(this).wrapAll( html.call(this, i) );
			});
		}

		if ( this[0] ) {
			// The elements to wrap the target around
			var wrap = jQuery( html, this[0].ownerDocument ).eq(0).clone(true);

			if ( this[0].parentNode ) {
				wrap.insertBefore( this[0] );
			}

			wrap.map(function() {
				var elem = this;

				while ( elem.firstChild && elem.firstChild.nodeType === 1 ) {
					elem = elem.firstChild;
				}

				return elem;
			}).append(this);
		}

		return this;
	},

	wrapInner: function( html ) {
		if ( jQuery.isFunction( html ) ) {
			return this.each(function(i) {
				jQuery(this).wrapInner( html.call(this, i) );
			});
		}

		return this.each(function() {
			var self = jQuery( this ), contents = self.contents();

			if ( contents.length ) {
				contents.wrapAll( html );

			} else {
				self.append( html );
			}
		});
	},

	wrap: function( html ) {
		return this.each(function() {
			jQuery( this ).wrapAll( html );
		});
	},

	unwrap: function() {
		return this.parent().each(function() {
			if ( !jQuery.nodeName( this, "body" ) ) {
				jQuery( this ).replaceWith( this.childNodes );
			}
		}).end();
	},

	append: function() {
		return this.domManip(arguments, true, function( elem ) {
			if ( this.nodeType === 1 ) {
				this.appendChild( elem );
			}
		});
	},

	prepend: function() {
		return this.domManip(arguments, true, function( elem ) {
			if ( this.nodeType === 1 ) {
				this.insertBefore( elem, this.firstChild );
			}
		});
	},

	before: function() {
		if ( this[0] && this[0].parentNode ) {
			return this.domManip(arguments, false, function( elem ) {
				this.parentNode.insertBefore( elem, this );
			});
		} else if ( arguments.length ) {
			var set = jQuery(arguments[0]);
			set.push.apply( set, this.toArray() );
			return this.pushStack( set, "before", arguments );
		}
	},

	after: function() {
		if ( this[0] && this[0].parentNode ) {
			return this.domManip(arguments, false, function( elem ) {
				this.parentNode.insertBefore( elem, this.nextSibling );
			});
		} else if ( arguments.length ) {
			var set = this.pushStack( this, "after", arguments );
			set.push.apply( set, jQuery(arguments[0]).toArray() );
			return set;
		}
	},

	clone: function( events ) {
		// Do the clone
		var ret = this.map(function() {
			if ( !jQuery.support.noCloneEvent && !jQuery.isXMLDoc(this) ) {
				// IE copies events bound via attachEvent when
				// using cloneNode. Calling detachEvent on the
				// clone will also remove the events from the orignal
				// In order to get around this, we use innerHTML.
				// Unfortunately, this means some modifications to
				// attributes in IE that are actually only stored
				// as properties will not be copied (such as the
				// the name attribute on an input).
				var html = this.outerHTML, ownerDocument = this.ownerDocument;
				if ( !html ) {
					var div = ownerDocument.createElement("div");
					div.appendChild( this.cloneNode(true) );
					html = div.innerHTML;
				}

				return jQuery.clean([html.replace(rinlinejQuery, "")
					.replace(rleadingWhitespace, "")], ownerDocument)[0];
			} else {
				return this.cloneNode(true);
			}
		});

		// Copy the events from the original to the clone
		if ( events === true ) {
			cloneCopyEvent( this, ret );
			cloneCopyEvent( this.find("*"), ret.find("*") );
		}

		// Return the cloned set
		return ret;
	},

	html: function( value ) {
		if ( value === undefined ) {
			return this[0] && this[0].nodeType === 1 ?
				this[0].innerHTML.replace(rinlinejQuery, "") :
				null;

		// See if we can take a shortcut and just use innerHTML
		} else if ( typeof value === "string" && !/<script/i.test( value ) &&
			(jQuery.support.leadingWhitespace || !rleadingWhitespace.test( value )) &&
			!wrapMap[ (rtagName.exec( value ) || ["", ""])[1].toLowerCase() ] ) {

			value = value.replace(rxhtmlTag, fcloseTag);

			try {
				for ( var i = 0, l = this.length; i < l; i++ ) {
					// Remove element nodes and prevent memory leaks
					if ( this[i].nodeType === 1 ) {
						jQuery.cleanData( this[i].getElementsByTagName("*") );
						this[i].innerHTML = value;
					}
				}

			// If using innerHTML throws an exception, use the fallback method
			} catch(e) {
				this.empty().append( value );
			}

		} else if ( jQuery.isFunction( value ) ) {
			this.each(function(i){
				var self = jQuery(this), old = self.html();
				self.empty().append(function(){
					return value.call( this, i, old );
				});
			});

		} else {
			this.empty().append( value );
		}

		return this;
	},

	replaceWith: function( value ) {
		if ( this[0] && this[0].parentNode ) {
			// Make sure that the elements are removed from the DOM before they are inserted
			// this can help fix replacing a parent with child elements
			if ( !jQuery.isFunction( value ) ) {
				value = jQuery( value ).detach();

			} else {
				return this.each(function(i) {
					var self = jQuery(this), old = self.html();
					self.replaceWith( value.call( this, i, old ) );
				});
			}

			return this.each(function() {
				var next = this.nextSibling, parent = this.parentNode;

				jQuery(this).remove();

				if ( next ) {
					jQuery(next).before( value );
				} else {
					jQuery(parent).append( value );
				}
			});
		} else {
			return this.pushStack( jQuery(jQuery.isFunction(value) ? value() : value), "replaceWith", value );
		}
	},

	detach: function( selector ) {
		return this.remove( selector, true );
	},

	domManip: function( args, table, callback ) {
		var results, first, value = args[0], scripts = [];

		// We can't cloneNode fragments that contain checked, in WebKit
		if ( !jQuery.support.checkClone && arguments.length === 3 && typeof value === "string" && rchecked.test( value ) ) {
			return this.each(function() {
				jQuery(this).domManip( args, table, callback, true );
			});
		}

		if ( jQuery.isFunction(value) ) {
			return this.each(function(i) {
				var self = jQuery(this);
				args[0] = value.call(this, i, table ? self.html() : undefined);
				self.domManip( args, table, callback );
			});
		}

		if ( this[0] ) {
			// If we're in a fragment, just use that instead of building a new one
			if ( args[0] && args[0].parentNode && args[0].parentNode.nodeType === 11 ) {
				results = { fragment: args[0].parentNode };
			} else {
				results = buildFragment( args, this, scripts );
			}

			first = results.fragment.firstChild;

			if ( first ) {
				table = table && jQuery.nodeName( first, "tr" );

				for ( var i = 0, l = this.length; i < l; i++ ) {
					callback.call(
						table ?
							root(this[i], first) :
							this[i],
						results.cacheable || this.length > 1 || i > 0 ?
							results.fragment.cloneNode(true) :
							results.fragment
					);
				}
			}

			if ( scripts ) {
				jQuery.each( scripts, evalScript );
			}
		}

		return this;

		function root( elem, cur ) {
			return jQuery.nodeName(elem, "table") ?
				(elem.getElementsByTagName("tbody")[0] ||
				elem.appendChild(elem.ownerDocument.createElement("tbody"))) :
				elem;
		}
	}
});

function cloneCopyEvent(orig, ret) {
	var i = 0;

	ret.each(function() {
		if ( this.nodeName !== (orig[i] && orig[i].nodeName) ) {
			return;
		}

		var oldData = jQuery.data( orig[i++] ), curData = jQuery.data( this, oldData ), events = oldData && oldData.events;

		if ( events ) {
			delete curData.handle;
			curData.events = {};

			for ( var type in events ) {
				for ( var handler in events[ type ] ) {
					jQuery.event.add( this, type, events[ type ][ handler ], events[ type ][ handler ].data );
				}
			}
		}
	});
}

function buildFragment( args, nodes, scripts ) {
	var fragment, cacheable, cacheresults, doc;

	// webkit does not clone 'checked' attribute of radio inputs on cloneNode, so don't cache if string has a checked
	if ( args.length === 1 && typeof args[0] === "string" && args[0].length < 512 && args[0].indexOf("<option") < 0 && (jQuery.support.checkClone || !rchecked.test( args[0] )) ) {
		cacheable = true;
		cacheresults = jQuery.fragments[ args[0] ];
		if ( cacheresults ) {
			if ( cacheresults !== 1 ) {
				fragment = cacheresults;
			}
		}
	}

	if ( !fragment ) {
		doc = (nodes && nodes[0] ? nodes[0].ownerDocument || nodes[0] : document);
		fragment = doc.createDocumentFragment();
		jQuery.clean( args, doc, fragment, scripts );
	}

	if ( cacheable ) {
		jQuery.fragments[ args[0] ] = cacheresults ? fragment : 1;
	}

	return { fragment: fragment, cacheable: cacheable };
}

jQuery.fragments = {};

jQuery.each({
	appendTo: "append",
	prependTo: "prepend",
	insertBefore: "before",
	insertAfter: "after",
	replaceAll: "replaceWith"
}, function( name, original ) {
	jQuery.fn[ name ] = function( selector ) {
		var ret = [], insert = jQuery( selector );

		for ( var i = 0, l = insert.length; i < l; i++ ) {
			var elems = (i > 0 ? this.clone(true) : this).get();
			jQuery.fn[ original ].apply( jQuery(insert[i]), elems );
			ret = ret.concat( elems );
		}
		return this.pushStack( ret, name, insert.selector );
	};
});

jQuery.each({
	// keepData is for internal use only--do not document
	remove: function( selector, keepData ) {
		if ( !selector || jQuery.filter( selector, [ this ] ).length ) {
			if ( !keepData && this.nodeType === 1 ) {
				jQuery.cleanData( this.getElementsByTagName("*") );
				jQuery.cleanData( [ this ] );
			}

			if ( this.parentNode ) {
				 this.parentNode.removeChild( this );
			}
		}
	},

	empty: function() {
		// Remove element nodes and prevent memory leaks
		if ( this.nodeType === 1 ) {
			jQuery.cleanData( this.getElementsByTagName("*") );
		}

		// Remove any remaining nodes
		while ( this.firstChild ) {
			this.removeChild( this.firstChild );
		}
	}
}, function( name, fn ) {
	jQuery.fn[ name ] = function() {
		return this.each( fn, arguments );
	};
});

jQuery.extend({
	clean: function( elems, context, fragment, scripts ) {
		context = context || document;

		// !context.createElement fails in IE with an error but returns typeof 'object'
		if ( typeof context.createElement === "undefined" ) {
			context = context.ownerDocument || context[0] && context[0].ownerDocument || document;
		}

		var ret = [];

		jQuery.each(elems, function( i, elem ) {
			if ( typeof elem === "number" ) {
				elem += "";
			}

			if ( !elem ) {
				return;
			}

			// Convert html string into DOM nodes
			if ( typeof elem === "string" && !rhtml.test( elem ) ) {
				elem = context.createTextNode( elem );

			} else if ( typeof elem === "string" ) {
				// Fix "XHTML"-style tags in all browsers
				elem = elem.replace(rxhtmlTag, fcloseTag);

				// Trim whitespace, otherwise indexOf won't work as expected
				var tag = (rtagName.exec( elem ) || ["", ""])[1].toLowerCase(),
					wrap = wrapMap[ tag ] || wrapMap._default,
					depth = wrap[0],
					div = context.createElement("div");

				// Go to html and back, then peel off extra wrappers
				div.innerHTML = wrap[1] + elem + wrap[2];

				// Move to the right depth
				while ( depth-- ) {
					div = div.lastChild;
				}

				// Remove IE's autoinserted <tbody> from table fragments
				if ( !jQuery.support.tbody ) {

					// String was a <table>, *may* have spurious <tbody>
					var hasBody = rtbody.test(elem),
						tbody = tag === "table" && !hasBody ?
							div.firstChild && div.firstChild.childNodes :

							// String was a bare <thead> or <tfoot>
							wrap[1] === "<table>" && !hasBody ?
								div.childNodes :
								[];

					for ( var j = tbody.length - 1; j >= 0 ; --j ) {
						if ( jQuery.nodeName( tbody[ j ], "tbody" ) && !tbody[ j ].childNodes.length ) {
							tbody[ j ].parentNode.removeChild( tbody[ j ] );
						}
					}

				}

				// IE completely kills leading whitespace when innerHTML is used
				if ( !jQuery.support.leadingWhitespace && rleadingWhitespace.test( elem ) ) {
					div.insertBefore( context.createTextNode( rleadingWhitespace.exec(elem)[0] ), div.firstChild );
				}

				elem = jQuery.makeArray( div.childNodes );
			}

			if ( elem.nodeType ) {
				ret.push( elem );
			} else {
				ret = jQuery.merge( ret, elem );
			}

		});

		if ( fragment ) {
			for ( var i = 0; ret[i]; i++ ) {
				if ( scripts && jQuery.nodeName( ret[i], "script" ) && (!ret[i].type || ret[i].type.toLowerCase() === "text/javascript") ) {
					scripts.push( ret[i].parentNode ? ret[i].parentNode.removeChild( ret[i] ) : ret[i] );
				} else {
					if ( ret[i].nodeType === 1 ) {
						ret.splice.apply( ret, [i + 1, 0].concat(jQuery.makeArray(ret[i].getElementsByTagName("script"))) );
					}
					fragment.appendChild( ret[i] );
				}
			}
		}

		return ret;
	},
	
	cleanData: function( elems ) {
		for ( var i = 0, elem, id; (elem = elems[i]) != null; i++ ) {
			jQuery.event.remove( elem );
			jQuery.removeData( elem );
		}
	}
});
// exclude the following css properties to add px
var rexclude = /z-?index|font-?weight|opacity|zoom|line-?height/i,
	ralpha = /alpha\([^)]*\)/,
	ropacity = /opacity=([^)]*)/,
	rfloat = /float/i,
	rdashAlpha = /-([a-z])/ig,
	rupper = /([A-Z])/g,
	rnumpx = /^-?\d+(?:px)?$/i,
	rnum = /^-?\d/,

	cssShow = { position: "absolute", visibility: "hidden", display:"block" },
	cssWidth = [ "Left", "Right" ],
	cssHeight = [ "Top", "Bottom" ],

	// cache check for defaultView.getComputedStyle
	getComputedStyle = document.defaultView && document.defaultView.getComputedStyle,
	// normalize float css property
	styleFloat = jQuery.support.cssFloat ? "cssFloat" : "styleFloat",
	fcamelCase = function( all, letter ) {
		return letter.toUpperCase();
	};

jQuery.fn.css = function( name, value ) {
	return access( this, name, value, true, function( elem, name, value ) {
		if ( value === undefined ) {
			return jQuery.curCSS( elem, name );
		}
		
		if ( typeof value === "number" && !rexclude.test(name) ) {
			value += "px";
		}

		jQuery.style( elem, name, value );
	});
};

jQuery.extend({
	style: function( elem, name, value ) {
		// don't set styles on text and comment nodes
		if ( !elem || elem.nodeType === 3 || elem.nodeType === 8 ) {
			return undefined;
		}

		// ignore negative width and height values #1599
		if ( (name === "width" || name === "height") && parseFloat(value) < 0 ) {
			value = undefined;
		}

		var style = elem.style || elem, set = value !== undefined;

		// IE uses filters for opacity
		if ( !jQuery.support.opacity && name === "opacity" ) {
			if ( set ) {
				// IE has trouble with opacity if it does not have layout
				// Force it by setting the zoom level
				style.zoom = 1;

				// Set the alpha filter to set the opacity
				var opacity = parseInt( value, 10 ) + "" === "NaN" ? "" : "alpha(opacity=" + value * 100 + ")";
				var filter = style.filter || jQuery.curCSS( elem, "filter" ) || "";
				style.filter = ralpha.test(filter) ? filter.replace(ralpha, opacity) : opacity;
			}

			return style.filter && style.filter.indexOf("opacity=") >= 0 ?
				(parseFloat( ropacity.exec(style.filter)[1] ) / 100) + "":
				"";
		}

		// Make sure we're using the right name for getting the float value
		if ( rfloat.test( name ) ) {
			name = styleFloat;
		}

		name = name.replace(rdashAlpha, fcamelCase);

		if ( set ) {
			style[ name ] = value;
		}

		return style[ name ];
	},

	css: function( elem, name, force, extra ) {
		if ( name === "width" || name === "height" ) {
			var val, props = cssShow, which = name === "width" ? cssWidth : cssHeight;

			function getWH() {
				val = name === "width" ? elem.offsetWidth : elem.offsetHeight;

				if ( extra === "border" ) {
					return;
				}

				jQuery.each( which, function() {
					if ( !extra ) {
						val -= parseFloat(jQuery.curCSS( elem, "padding" + this, true)) || 0;
					}

					if ( extra === "margin" ) {
						val += parseFloat(jQuery.curCSS( elem, "margin" + this, true)) || 0;
					} else {
						val -= parseFloat(jQuery.curCSS( elem, "border" + this + "Width", true)) || 0;
					}
				});
			}

			if ( elem.offsetWidth !== 0 ) {
				getWH();
			} else {
				jQuery.swap( elem, props, getWH );
			}

			return Math.max(0, Math.round(val));
		}

		return jQuery.curCSS( elem, name, force );
	},

	curCSS: function( elem, name, force ) {
		var ret, style = elem.style, filter;

		// IE uses filters for opacity
		if ( !jQuery.support.opacity && name === "opacity" && elem.currentStyle ) {
			ret = ropacity.test(elem.currentStyle.filter || "") ?
				(parseFloat(RegExp.$1) / 100) + "" :
				"";

			return ret === "" ?
				"1" :
				ret;
		}

		// Make sure we're using the right name for getting the float value
		if ( rfloat.test( name ) ) {
			name = styleFloat;
		}

		if ( !force && style && style[ name ] ) {
			ret = style[ name ];

		} else if ( getComputedStyle ) {

			// Only "float" is needed here
			if ( rfloat.test( name ) ) {
				name = "float";
			}

			name = name.replace( rupper, "-$1" ).toLowerCase();

			var defaultView = elem.ownerDocument.defaultView;

			if ( !defaultView ) {
				return null;
			}

			var computedStyle = defaultView.getComputedStyle( elem, null );

			if ( computedStyle ) {
				ret = computedStyle.getPropertyValue( name );
			}

			// We should always get a number back from opacity
			if ( name === "opacity" && ret === "" ) {
				ret = "1";
			}

		} else if ( elem.currentStyle ) {
			var camelCase = name.replace(rdashAlpha, fcamelCase);

			ret = elem.currentStyle[ name ] || elem.currentStyle[ camelCase ];

			// From the awesome hack by Dean Edwards
			// http://erik.eae.net/archives/2007/07/27/18.54.15/#comment-102291

			// If we're not dealing with a regular pixel number
			// but a number that has a weird ending, we need to convert it to pixels
			if ( !rnumpx.test( ret ) && rnum.test( ret ) ) {
				// Remember the original values
				var left = style.left, rsLeft = elem.runtimeStyle.left;

				// Put in the new values to get a computed value out
				elem.runtimeStyle.left = elem.currentStyle.left;
				style.left = camelCase === "fontSize" ? "1em" : (ret || 0);
				ret = style.pixelLeft + "px";

				// Revert the changed values
				style.left = left;
				elem.runtimeStyle.left = rsLeft;
			}
		}

		return ret;
	},

	// A method for quickly swapping in/out CSS properties to get correct calculations
	swap: function( elem, options, callback ) {
		var old = {};

		// Remember the old values, and insert the new ones
		for ( var name in options ) {
			old[ name ] = elem.style[ name ];
			elem.style[ name ] = options[ name ];
		}

		callback.call( elem );

		// Revert the old values
		for ( var name in options ) {
			elem.style[ name ] = old[ name ];
		}
	}
});

if ( jQuery.expr && jQuery.expr.filters ) {
	jQuery.expr.filters.hidden = function( elem ) {
		var width = elem.offsetWidth, height = elem.offsetHeight,
			skip = elem.nodeName.toLowerCase() === "tr";

		return width === 0 && height === 0 && !skip ?
			true :
			width > 0 && height > 0 && !skip ?
				false :
				jQuery.curCSS(elem, "display") === "none";
	};

	jQuery.expr.filters.visible = function( elem ) {
		return !jQuery.expr.filters.hidden( elem );
	};
}
var jsc = now(),
	rscript = /<script(.|\s)*?\/script>/gi,
	rselectTextarea = /select|textarea/i,
	rinput = /color|date|datetime|email|hidden|month|number|password|range|search|tel|text|time|url|week/i,
	jsre = /=\?(&|$)/,
	rquery = /\?/,
	rts = /(\?|&)_=.*?(&|$)/,
	rurl = /^(\w+:)?\/\/([^\/?#]+)/,
	r20 = /%20/g;

jQuery.fn.extend({
	// Keep a copy of the old load
	_load: jQuery.fn.load,

	load: function( url, params, callback ) {
		if ( typeof url !== "string" ) {
			return this._load( url );

		// Don't do a request if no elements are being requested
		} else if ( !this.length ) {
			return this;
		}

		var off = url.indexOf(" ");
		if ( off >= 0 ) {
			var selector = url.slice(off, url.length);
			url = url.slice(0, off);
		}

		// Default to a GET request
		var type = "GET";

		// If the second parameter was provided
		if ( params ) {
			// If it's a function
			if ( jQuery.isFunction( params ) ) {
				// We assume that it's the callback
				callback = params;
				params = null;

			// Otherwise, build a param string
			} else if ( typeof params === "object" ) {
				params = jQuery.param( params, jQuery.ajaxSettings.traditional );
				type = "POST";
			}
		}

		var self = this;

		// Request the remote document
		jQuery.ajax({
			url: url,
			type: type,
			dataType: "html",
			data: params,
			complete: function( res, status ) {
				// If successful, inject the HTML into all the matched elements
				if ( status === "success" || status === "notmodified" ) {
					// See if a selector was specified
					self.html( selector ?
						// Create a dummy div to hold the results
						jQuery("<div />")
							// inject the contents of the document in, removing the scripts
							// to avoid any 'Permission Denied' errors in IE
							.append(res.responseText.replace(rscript, ""))

							// Locate the specified elements
							.find(selector) :

						// If not, just inject the full result
						res.responseText );
				}

				if ( callback ) {
					self.each( callback, [res.responseText, status, res] );
				}
			}
		});

		return this;
	},

	serialize: function() {
		return jQuery.param(this.serializeArray());
	},
	serializeArray: function() {
		return this.map(function() {
			return this.elements ? jQuery.makeArray(this.elements) : this;
		})
		.filter(function() {
			return this.name && !this.disabled &&
				(this.checked || rselectTextarea.test(this.nodeName) ||
					rinput.test(this.type));
		})
		.map(function( i, elem ) {
			var val = jQuery(this).val();

			return val == null ?
				null :
				jQuery.isArray(val) ?
					jQuery.map( val, function( val, i ) {
						return { name: elem.name, value: val };
					}) :
					{ name: elem.name, value: val };
		}).get();
	}
});

// Attach a bunch of functions for handling common AJAX events
jQuery.each( "ajaxStart ajaxStop ajaxComplete ajaxError ajaxSuccess ajaxSend".split(" "), function( i, o ) {
	jQuery.fn[o] = function( f ) {
		return this.bind(o, f);
	};
});

jQuery.extend({

	get: function( url, data, callback, type ) {
		// shift arguments if data argument was omited
		if ( jQuery.isFunction( data ) ) {
			type = type || callback;
			callback = data;
			data = null;
		}

		return jQuery.ajax({
			type: "GET",
			url: url,
			data: data,
			success: callback,
			dataType: type
		});
	},

	getScript: function( url, callback ) {
		return jQuery.get(url, null, callback, "script");
	},

	getJSON: function( url, data, callback ) {
		return jQuery.get(url, data, callback, "json");
	},

	post: function( url, data, callback, type ) {
		// shift arguments if data argument was omited
		if ( jQuery.isFunction( data ) ) {
			type = type || callback;
			callback = data;
			data = {};
		}

		return jQuery.ajax({
			type: "POST",
			url: url,
			data: data,
			success: callback,
			dataType: type
		});
	},

	ajaxSetup: function( settings ) {
		jQuery.extend( jQuery.ajaxSettings, settings );
	},

	ajaxSettings: {
		url: location.href,
		global: true,
		type: "GET",
		contentType: "application/x-www-form-urlencoded",
		processData: true,
		async: true,
		/*
		timeout: 0,
		data: null,
		username: null,
		password: null,
		traditional: false,
		*/
		// Create the request object; Microsoft failed to properly
		// implement the XMLHttpRequest in IE7 (can't request local files),
		// so we use the ActiveXObject when it is available
		// This function can be overriden by calling jQuery.ajaxSetup
		xhr: window.XMLHttpRequest && (window.location.protocol !== "file:" || !window.ActiveXObject) ?
			function() {
				return new window.XMLHttpRequest();
			} :
			function() {
				try {
					return new window.ActiveXObject("Microsoft.XMLHTTP");
				} catch(e) {}
			},
		accepts: {
			xml: "application/xml, text/xml",
			html: "text/html",
			script: "text/javascript, application/javascript",
			json: "application/json, text/javascript",
			text: "text/plain",
			_default: "*/*"
		}
	},

	// Last-Modified header cache for next request
	lastModified: {},
	etag: {},

	ajax: function( origSettings ) {
		var s = jQuery.extend(true, {}, jQuery.ajaxSettings, origSettings);
		
		var jsonp, status, data,
			callbackContext = origSettings && origSettings.context || s,
			type = s.type.toUpperCase();

		// convert data if not already a string
		if ( s.data && s.processData && typeof s.data !== "string" ) {
			s.data = jQuery.param( s.data, s.traditional );
		}

		// Handle JSONP Parameter Callbacks
		if ( s.dataType === "jsonp" ) {
			if ( type === "GET" ) {
				if ( !jsre.test( s.url ) ) {
					s.url += (rquery.test( s.url ) ? "&" : "?") + (s.jsonp || "callback") + "=?";
				}
			} else if ( !s.data || !jsre.test(s.data) ) {
				s.data = (s.data ? s.data + "&" : "") + (s.jsonp || "callback") + "=?";
			}
			s.dataType = "json";
		}

		// Build temporary JSONP function
		if ( s.dataType === "json" && (s.data && jsre.test(s.data) || jsre.test(s.url)) ) {
			jsonp = s.jsonpCallback || ("jsonp" + jsc++);

			// Replace the =? sequence both in the query string and the data
			if ( s.data ) {
				s.data = (s.data + "").replace(jsre, "=" + jsonp + "$1");
			}

			s.url = s.url.replace(jsre, "=" + jsonp + "$1");

			// We need to make sure
			// that a JSONP style response is executed properly
			s.dataType = "script";

			// Handle JSONP-style loading
			window[ jsonp ] = window[ jsonp ] || function( tmp ) {
				data = tmp;
				success();
				complete();
				// Garbage collect
				window[ jsonp ] = undefined;

				try {
					delete window[ jsonp ];
				} catch(e) {}

				if ( head ) {
					head.removeChild( script );
				}
			};
		}

		if ( s.dataType === "script" && s.cache === null ) {
			s.cache = false;
		}

		if ( s.cache === false && type === "GET" ) {
			var ts = now();

			// try replacing _= if it is there
			var ret = s.url.replace(rts, "$1_=" + ts + "$2");

			// if nothing was replaced, add timestamp to the end
			s.url = ret + ((ret === s.url) ? (rquery.test(s.url) ? "&" : "?") + "_=" + ts : "");
		}

		// If data is available, append data to url for get requests
		if ( s.data && type === "GET" ) {
			s.url += (rquery.test(s.url) ? "&" : "?") + s.data;
		}

		// Watch for a new set of requests
		if ( s.global && ! jQuery.active++ ) {
			jQuery.event.trigger( "ajaxStart" );
		}

		// Matches an absolute URL, and saves the domain
		var parts = rurl.exec( s.url ),
			remote = parts && (parts[1] && parts[1] !== location.protocol || parts[2] !== location.host);

		// If we're requesting a remote document
		// and trying to load JSON or Script with a GET
		if ( s.dataType === "script" && type === "GET" && remote ) {
			var head = document.getElementsByTagName("head")[0] || document.documentElement;
			var script = document.createElement("script");
			script.src = s.url;
			if ( s.scriptCharset ) {
				script.charset = s.scriptCharset;
			}

			// Handle Script loading
			if ( !jsonp ) {
				var done = false;

				// Attach handlers for all browsers
				script.onload = script.onreadystatechange = function() {
					if ( !done && (!this.readyState ||
							this.readyState === "loaded" || this.readyState === "complete") ) {
						done = true;
						success();
						complete();

						// Handle memory leak in IE
						script.onload = script.onreadystatechange = null;
						if ( head && script.parentNode ) {
							head.removeChild( script );
						}
					}
				};
			}

			// Use insertBefore instead of appendChild  to circumvent an IE6 bug.
			// This arises when a base node is used (#2709 and #4378).
			head.insertBefore( script, head.firstChild );

			// We handle everything using the script element injection
			return undefined;
		}

		var requestDone = false;

		// Create the request object
		var xhr = s.xhr();

		if ( !xhr ) {
			return;
		}

		// Open the socket
		// Passing null username, generates a login popup on Opera (#2865)
		if ( s.username ) {
			xhr.open(type, s.url, s.async, s.username, s.password);
		} else {
			xhr.open(type, s.url, s.async);
		}

		// Need an extra try/catch for cross domain requests in Firefox 3
		try {
			// Set the correct header, if data is being sent
			if ( s.data || origSettings && origSettings.contentType ) {
				xhr.setRequestHeader("Content-Type", s.contentType);
			}

			// Set the If-Modified-Since and/or If-None-Match header, if in ifModified mode.
			if ( s.ifModified ) {
				if ( jQuery.lastModified[s.url] ) {
					xhr.setRequestHeader("If-Modified-Since", jQuery.lastModified[s.url]);
				}

				if ( jQuery.etag[s.url] ) {
					xhr.setRequestHeader("If-None-Match", jQuery.etag[s.url]);
				}
			}

			// Set header so the called script knows that it's an XMLHttpRequest
			// Only send the header if it's not a remote XHR
			if ( !remote ) {
				xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");
			}

			// Set the Accepts header for the server, depending on the dataType
			xhr.setRequestHeader("Accept", s.dataType && s.accepts[ s.dataType ] ?
				s.accepts[ s.dataType ] + ", */*" :
				s.accepts._default );
		} catch(e) {}

		// Allow custom headers/mimetypes and early abort
		if ( s.beforeSend && s.beforeSend.call(callbackContext, xhr, s) === false ) {
			// Handle the global AJAX counter
			if ( s.global && ! --jQuery.active ) {
				jQuery.event.trigger( "ajaxStop" );
			}

			// close opended socket
			xhr.abort();
			return false;
		}

		if ( s.global ) {
			trigger("ajaxSend", [xhr, s]);
		}

		// Wait for a response to come back
		var onreadystatechange = xhr.onreadystatechange = function( isTimeout ) {
			// The request was aborted
			if ( !xhr || xhr.readyState === 0 || isTimeout === "abort" ) {
				// Opera doesn't call onreadystatechange before this point
				// so we simulate the call
				if ( !requestDone ) {
					complete();
				}

				requestDone = true;
				if ( xhr ) {
					xhr.onreadystatechange = jQuery.noop;
				}

			// The transfer is complete and the data is available, or the request timed out
			} else if ( !requestDone && xhr && (xhr.readyState === 4 || isTimeout === "timeout") ) {
				requestDone = true;
				xhr.onreadystatechange = jQuery.noop;

				status = isTimeout === "timeout" ?
					"timeout" :
					!jQuery.httpSuccess( xhr ) ?
						"error" :
						s.ifModified && jQuery.httpNotModified( xhr, s.url ) ?
							"notmodified" :
							"success";

				var errMsg;

				if ( status === "success" ) {
					// Watch for, and catch, XML document parse errors
					try {
						// process the data (runs the xml through httpData regardless of callback)
						data = jQuery.httpData( xhr, s.dataType, s );
					} catch(err) {
						status = "parsererror";
						errMsg = err;
					}
				}

				// Make sure that the request was successful or notmodified
				if ( status === "success" || status === "notmodified" ) {
					// JSONP handles its own success callback
					if ( !jsonp ) {
						success();
					}
				} else {
					jQuery.handleError(s, xhr, status, errMsg);
				}

				// Fire the complete handlers
				complete();

				if ( isTimeout === "timeout" ) {
					xhr.abort();
				}

				// Stop memory leaks
				if ( s.async ) {
					xhr = null;
				}
			}
		};

	    // Override the abort handler, if we can (IE doesn't allow it, but that's OK)
	    // Opera doesn't fire onreadystatechange at all on abort
	    try {
		    var oldAbort = xhr.abort;
		    xhr.abort = function() {
			    if ( xhr ) {
			        xhr.onreadystatechange = jQuery.noop;
			        oldAbort.call( xhr );
			    }

			    onreadystatechange( "abort" );
		    };
	    } catch(e) { }

		// Timeout checker
		if ( s.async && s.timeout > 0 ) {
			setTimeout(function() {
				// Check to see if the request is still happening
				if ( xhr && !requestDone ) {
					onreadystatechange( "timeout" );
				}
			}, s.timeout);
		}

		// Send the data
		try {
			xhr.send( type === "POST" || type === "PUT" || type === "DELETE" ? s.data : null );
		} catch(e) {
			jQuery.handleError(s, xhr, null, e);
			// Fire the complete handlers
			complete();
		}

		// firefox 1.5 doesn't fire statechange for sync requests
		if ( !s.async ) {
			onreadystatechange();
		}

		function success() {
			// If a local callback was specified, fire it and pass it the data
			if ( s.success ) {
				s.success.call( callbackContext, data, status, xhr );
			}

			// Fire the global callback
			if ( s.global ) {
				trigger( "ajaxSuccess", [xhr, s] );
			}
		}

		function complete() {
			// Process result
			if ( s.complete ) {
				s.complete.call( callbackContext, xhr, status);
			}

			// The request was completed
			if ( s.global ) {
				trigger( "ajaxComplete", [xhr, s] );
			}

			// Handle the global AJAX counter
			if ( s.global && ! --jQuery.active ) {
				jQuery.event.trigger( "ajaxStop" );
			}
		}
		
		function trigger(type, args) {
			(s.context ? jQuery(s.context) : jQuery.event).trigger(type, args);
		}

		// return XMLHttpRequest to allow aborting the request etc.
		return xhr;
	},

	handleError: function( s, xhr, status, e ) {
		// If a local callback was specified, fire it
		if ( s.error ) {
			s.error.call( s.context || s, xhr, status, e );
		}

		// Fire the global callback
		if ( s.global ) {
			(s.context ? jQuery(s.context) : jQuery.event).trigger( "ajaxError", [xhr, s, e] );
		}
	},

	// Counter for holding the number of active queries
	active: 0,

	// Determines if an XMLHttpRequest was successful or not
	httpSuccess: function( xhr ) {
		try {
			// IE error sometimes returns 1223 when it should be 204 so treat it as success, see #1450
			return !xhr.status && location.protocol === "file:" ||
				// Opera returns 0 when status is 304
				( xhr.status >= 200 && xhr.status < 300 ) ||
				xhr.status === 304 || xhr.status === 1223 || xhr.status === 0;
		} catch(e) {}

		return false;
	},

	// Determines if an XMLHttpRequest returns NotModified
	httpNotModified: function( xhr, url ) {
		var lastModified = xhr.getResponseHeader("Last-Modified"),
			etag = xhr.getResponseHeader("Etag");

		if ( lastModified ) {
			jQuery.lastModified[url] = lastModified;
		}

		if ( etag ) {
			jQuery.etag[url] = etag;
		}

		// Opera returns 0 when status is 304
		return xhr.status === 304 || xhr.status === 0;
	},

	httpData: function( xhr, type, s ) {
		var ct = xhr.getResponseHeader("content-type") || "",
			xml = type === "xml" || !type && ct.indexOf("xml") >= 0,
			data = xml ? xhr.responseXML : xhr.responseText;

		if ( xml && data.documentElement.nodeName === "parsererror" ) {
			jQuery.error( "parsererror" );
		}

		// Allow a pre-filtering function to sanitize the response
		// s is checked to keep backwards compatibility
		if ( s && s.dataFilter ) {
			data = s.dataFilter( data, type );
		}

		// The filter can actually parse the response
		if ( typeof data === "string" ) {
			// Get the JavaScript object, if JSON is used.
			if ( type === "json" || !type && ct.indexOf("json") >= 0 ) {
				data = jQuery.parseJSON( data );

			// If the type is "script", eval it in global context
			} else if ( type === "script" || !type && ct.indexOf("javascript") >= 0 ) {
				jQuery.globalEval( data );
			}
		}

		return data;
	},

	// Serialize an array of form elements or a set of
	// key/values into a query string
	param: function( a, traditional ) {
		var s = [];
		
		// Set traditional to true for jQuery <= 1.3.2 behavior.
		if ( traditional === undefined ) {
			traditional = jQuery.ajaxSettings.traditional;
		}
		
		// If an array was passed in, assume that it is an array of form elements.
		if ( jQuery.isArray(a) || a.jquery ) {
			// Serialize the form elements
			jQuery.each( a, function() {
				add( this.name, this.value );
			});
			
		} else {
			// If traditional, encode the "old" way (the way 1.3.2 or older
			// did it), otherwise encode params recursively.
			for ( var prefix in a ) {
				buildParams( prefix, a[prefix] );
			}
		}

		// Return the resulting serialization
		return s.join("&").replace(r20, "+");

		function buildParams( prefix, obj ) {
			if ( jQuery.isArray(obj) ) {
				// Serialize array item.
				jQuery.each( obj, function( i, v ) {
					if ( traditional ) {
						// Treat each array item as a scalar.
						add( prefix, v );
					} else {
						// If array item is non-scalar (array or object), encode its
						// numeric index to resolve deserialization ambiguity issues.
						// Note that rack (as of 1.0.0) can't currently deserialize
						// nested arrays properly, and attempting to do so may cause
						// a server error. Possible fixes are to modify rack's
						// deserialization algorithm or to provide an option or flag
						// to force array serialization to be shallow.
						buildParams( prefix + "[" + ( typeof v === "object" || jQuery.isArray(v) ? i : "" ) + "]", v );
					}
				});
					
			} else if ( !traditional && obj != null && typeof obj === "object" ) {
				// Serialize object item.
				jQuery.each( obj, function( k, v ) {
					buildParams( prefix + "[" + k + "]", v );
				});
					
			} else {
				// Serialize scalar item.
				add( prefix, obj );
			}
		}

		function add( key, value ) {
			// If value is a function, invoke it and return its value
			value = jQuery.isFunction(value) ? value() : value;
			s[ s.length ] = encodeURIComponent(key) + "=" + encodeURIComponent(value);
		}
	}
});
var elemdisplay = {},
	rfxtypes = /toggle|show|hide/,
	rfxnum = /^([+-]=)?([\d+-.]+)(.*)$/,
	timerId,
	fxAttrs = [
		// height animations
		[ "height", "marginTop", "marginBottom", "paddingTop", "paddingBottom" ],
		// width animations
		[ "width", "marginLeft", "marginRight", "paddingLeft", "paddingRight" ],
		// opacity animations
		[ "opacity" ]
	];

jQuery.fn.extend({
	show: function( speed, callback ) {
		if ( speed || speed === 0) {
			return this.animate( genFx("show", 3), speed, callback);

		} else {
			for ( var i = 0, l = this.length; i < l; i++ ) {
				var old = jQuery.data(this[i], "olddisplay");

				this[i].style.display = old || "";

				if ( jQuery.css(this[i], "display") === "none" ) {
					var nodeName = this[i].nodeName, display;

					if ( elemdisplay[ nodeName ] ) {
						display = elemdisplay[ nodeName ];

					} else {
						var elem = jQuery("<" + nodeName + " />").appendTo("body");

						display = elem.css("display");

						if ( display === "none" ) {
							display = "block";
						}

						elem.remove();

						elemdisplay[ nodeName ] = display;
					}

					jQuery.data(this[i], "olddisplay", display);
				}
			}

			// Set the display of the elements in a second loop
			// to avoid the constant reflow
			for ( var j = 0, k = this.length; j < k; j++ ) {
				this[j].style.display = jQuery.data(this[j], "olddisplay") || "";
			}

			return this;
		}
	},

	hide: function( speed, callback ) {
		if ( speed || speed === 0 ) {
			return this.animate( genFx("hide", 3), speed, callback);

		} else {
			for ( var i = 0, l = this.length; i < l; i++ ) {
				var old = jQuery.data(this[i], "olddisplay");
				if ( !old && old !== "none" ) {
					jQuery.data(this[i], "olddisplay", jQuery.css(this[i], "display"));
				}
			}

			// Set the display of the elements in a second loop
			// to avoid the constant reflow
			for ( var j = 0, k = this.length; j < k; j++ ) {
				this[j].style.display = "none";
			}

			return this;
		}
	},

	// Save the old toggle function
	_toggle: jQuery.fn.toggle,

	toggle: function( fn, fn2 ) {
		var bool = typeof fn === "boolean";

		if ( jQuery.isFunction(fn) && jQuery.isFunction(fn2) ) {
			this._toggle.apply( this, arguments );

		} else if ( fn == null || bool ) {
			this.each(function() {
				var state = bool ? fn : jQuery(this).is(":hidden");
				jQuery(this)[ state ? "show" : "hide" ]();
			});

		} else {
			this.animate(genFx("toggle", 3), fn, fn2);
		}

		return this;
	},

	fadeTo: function( speed, to, callback ) {
		return this.filter(":hidden").css("opacity", 0).show().end()
					.animate({opacity: to}, speed, callback);
	},

	animate: function( prop, speed, easing, callback ) {
		var optall = jQuery.speed(speed, easing, callback);

		if ( jQuery.isEmptyObject( prop ) ) {
			return this.each( optall.complete );
		}

		return this[ optall.queue === false ? "each" : "queue" ](function() {
			var opt = jQuery.extend({}, optall), p,
				hidden = this.nodeType === 1 && jQuery(this).is(":hidden"),
				self = this;

			for ( p in prop ) {
				var name = p.replace(rdashAlpha, fcamelCase);

				if ( p !== name ) {
					prop[ name ] = prop[ p ];
					delete prop[ p ];
					p = name;
				}

				if ( prop[p] === "hide" && hidden || prop[p] === "show" && !hidden ) {
					return opt.complete.call(this);
				}

				if ( ( p === "height" || p === "width" ) && this.style ) {
					// Store display property
					opt.display = jQuery.css(this, "display");

					// Make sure that nothing sneaks out
					opt.overflow = this.style.overflow;
				}

				if ( jQuery.isArray( prop[p] ) ) {
					// Create (if needed) and add to specialEasing
					(opt.specialEasing = opt.specialEasing || {})[p] = prop[p][1];
					prop[p] = prop[p][0];
				}
			}

			if ( opt.overflow != null ) {
				this.style.overflow = "hidden";
			}

			opt.curAnim = jQuery.extend({}, prop);

			jQuery.each( prop, function( name, val ) {
				var e = new jQuery.fx( self, opt, name );

				if ( rfxtypes.test(val) ) {
					e[ val === "toggle" ? hidden ? "show" : "hide" : val ]( prop );

				} else {
					var parts = rfxnum.exec(val),
						start = e.cur(true) || 0;

					if ( parts ) {
						var end = parseFloat( parts[2] ),
							unit = parts[3] || "px";

						// We need to compute starting value
						if ( unit !== "px" ) {
							self.style[ name ] = (end || 1) + unit;
							start = ((end || 1) / e.cur(true)) * start;
							self.style[ name ] = start + unit;
						}

						// If a +=/-= token was provided, we're doing a relative animation
						if ( parts[1] ) {
							end = ((parts[1] === "-=" ? -1 : 1) * end) + start;
						}

						e.custom( start, end, unit );

					} else {
						e.custom( start, val, "" );
					}
				}
			});

			// For JS strict compliance
			return true;
		});
	},

	stop: function( clearQueue, gotoEnd ) {
		var timers = jQuery.timers;

		if ( clearQueue ) {
			this.queue([]);
		}

		this.each(function() {
			// go in reverse order so anything added to the queue during the loop is ignored
			for ( var i = timers.length - 1; i >= 0; i-- ) {
				if ( timers[i].elem === this ) {
					if (gotoEnd) {
						// force the next step to be the last
						timers[i](true);
					}

					timers.splice(i, 1);
				}
			}
		});

		// start the next in the queue if the last step wasn't forced
		if ( !gotoEnd ) {
			this.dequeue();
		}

		return this;
	}

});

// Generate shortcuts for custom animations
jQuery.each({
	slideDown: genFx("show", 1),
	slideUp: genFx("hide", 1),
	slideToggle: genFx("toggle", 1),
	fadeIn: { opacity: "show" },
	fadeOut: { opacity: "hide" }
}, function( name, props ) {
	jQuery.fn[ name ] = function( speed, callback ) {
		return this.animate( props, speed, callback );
	};
});

jQuery.extend({
	speed: function( speed, easing, fn ) {
		var opt = speed && typeof speed === "object" ? speed : {
			complete: fn || !fn && easing ||
				jQuery.isFunction( speed ) && speed,
			duration: speed,
			easing: fn && easing || easing && !jQuery.isFunction(easing) && easing
		};

		opt.duration = jQuery.fx.off ? 0 : typeof opt.duration === "number" ? opt.duration :
			jQuery.fx.speeds[opt.duration] || jQuery.fx.speeds._default;

		// Queueing
		opt.old = opt.complete;
		opt.complete = function() {
			if ( opt.queue !== false ) {
				jQuery(this).dequeue();
			}
			if ( jQuery.isFunction( opt.old ) ) {
				opt.old.call( this );
			}
		};

		return opt;
	},

	easing: {
		linear: function( p, n, firstNum, diff ) {
			return firstNum + diff * p;
		},
		swing: function( p, n, firstNum, diff ) {
			return ((-Math.cos(p*Math.PI)/2) + 0.5) * diff + firstNum;
		}
	},

	timers: [],

	fx: function( elem, options, prop ) {
		this.options = options;
		this.elem = elem;
		this.prop = prop;

		if ( !options.orig ) {
			options.orig = {};
		}
	}

});

jQuery.fx.prototype = {
	// Simple function for setting a style value
	update: function() {
		if ( this.options.step ) {
			this.options.step.call( this.elem, this.now, this );
		}

		(jQuery.fx.step[this.prop] || jQuery.fx.step._default)( this );

		// Set display property to block for height/width animations
		if ( ( this.prop === "height" || this.prop === "width" ) && this.elem.style ) {
			this.elem.style.display = "block";
		}
	},

	// Get the current size
	cur: function( force ) {
		if ( this.elem[this.prop] != null && (!this.elem.style || this.elem.style[this.prop] == null) ) {
			return this.elem[ this.prop ];
		}

		var r = parseFloat(jQuery.css(this.elem, this.prop, force));
		return r && r > -10000 ? r : parseFloat(jQuery.curCSS(this.elem, this.prop)) || 0;
	},

	// Start an animation from one number to another
	custom: function( from, to, unit ) {
		this.startTime = now();
		this.start = from;
		this.end = to;
		this.unit = unit || this.unit || "px";
		this.now = this.start;
		this.pos = this.state = 0;

		var self = this;
		function t( gotoEnd ) {
			return self.step(gotoEnd);
		}

		t.elem = this.elem;

		if ( t() && jQuery.timers.push(t) && !timerId ) {
			timerId = setInterval(jQuery.fx.tick, 13);
		}
	},

	// Simple 'show' function
	show: function() {
		// Remember where we started, so that we can go back to it later
		this.options.orig[this.prop] = jQuery.style( this.elem, this.prop );
		this.options.show = true;

		// Begin the animation
		// Make sure that we start at a small width/height to avoid any
		// flash of content
		this.custom(this.prop === "width" || this.prop === "height" ? 1 : 0, this.cur());

		// Start by showing the element
		jQuery( this.elem ).show();
	},

	// Simple 'hide' function
	hide: function() {
		// Remember where we started, so that we can go back to it later
		this.options.orig[this.prop] = jQuery.style( this.elem, this.prop );
		this.options.hide = true;

		// Begin the animation
		this.custom(this.cur(), 0);
	},

	// Each step of an animation
	step: function( gotoEnd ) {
		var t = now(), done = true;

		if ( gotoEnd || t >= this.options.duration + this.startTime ) {
			this.now = this.end;
			this.pos = this.state = 1;
			this.update();

			this.options.curAnim[ this.prop ] = true;

			for ( var i in this.options.curAnim ) {
				if ( this.options.curAnim[i] !== true ) {
					done = false;
				}
			}

			if ( done ) {
				if ( this.options.display != null ) {
					// Reset the overflow
					this.elem.style.overflow = this.options.overflow;

					// Reset the display
					var old = jQuery.data(this.elem, "olddisplay");
					this.elem.style.display = old ? old : this.options.display;

					if ( jQuery.css(this.elem, "display") === "none" ) {
						this.elem.style.display = "block";
					}
				}

				// Hide the element if the "hide" operation was done
				if ( this.options.hide ) {
					jQuery(this.elem).hide();
				}

				// Reset the properties, if the item has been hidden or shown
				if ( this.options.hide || this.options.show ) {
					for ( var p in this.options.curAnim ) {
						jQuery.style(this.elem, p, this.options.orig[p]);
					}
				}

				// Execute the complete function
				this.options.complete.call( this.elem );
			}

			return false;

		} else {
			var n = t - this.startTime;
			this.state = n / this.options.duration;

			// Perform the easing function, defaults to swing
			var specialEasing = this.options.specialEasing && this.options.specialEasing[this.prop];
			var defaultEasing = this.options.easing || (jQuery.easing.swing ? "swing" : "linear");
			this.pos = jQuery.easing[specialEasing || defaultEasing](this.state, n, 0, 1, this.options.duration);
			this.now = this.start + ((this.end - this.start) * this.pos);

			// Perform the next step of the animation
			this.update();
		}

		return true;
	}
};

jQuery.extend( jQuery.fx, {
	tick: function() {
		var timers = jQuery.timers;

		for ( var i = 0; i < timers.length; i++ ) {
			if ( !timers[i]() ) {
				timers.splice(i--, 1);
			}
		}

		if ( !timers.length ) {
			jQuery.fx.stop();
		}
	},
		
	stop: function() {
		clearInterval( timerId );
		timerId = null;
	},
	
	speeds: {
		slow: 600,
 		fast: 200,
 		// Default speed
 		_default: 400
	},

	step: {
		opacity: function( fx ) {
			jQuery.style(fx.elem, "opacity", fx.now);
		},

		_default: function( fx ) {
			if ( fx.elem.style && fx.elem.style[ fx.prop ] != null ) {
				fx.elem.style[ fx.prop ] = (fx.prop === "width" || fx.prop === "height" ? Math.max(0, fx.now) : fx.now) + fx.unit;
			} else {
				fx.elem[ fx.prop ] = fx.now;
			}
		}
	}
});

if ( jQuery.expr && jQuery.expr.filters ) {
	jQuery.expr.filters.animated = function( elem ) {
		return jQuery.grep(jQuery.timers, function( fn ) {
			return elem === fn.elem;
		}).length;
	};
}

function genFx( type, num ) {
	var obj = {};

	jQuery.each( fxAttrs.concat.apply([], fxAttrs.slice(0,num)), function() {
		obj[ this ] = type;
	});

	return obj;
}
if ( "getBoundingClientRect" in document.documentElement ) {
	jQuery.fn.offset = function( options ) {
		var elem = this[0];

		if ( options ) { 
			return this.each(function( i ) {
				jQuery.offset.setOffset( this, options, i );
			});
		}

		if ( !elem || !elem.ownerDocument ) {
			return null;
		}

		if ( elem === elem.ownerDocument.body ) {
			return jQuery.offset.bodyOffset( elem );
		}

		var box = elem.getBoundingClientRect(), doc = elem.ownerDocument, body = doc.body, docElem = doc.documentElement,
			clientTop = docElem.clientTop || body.clientTop || 0, clientLeft = docElem.clientLeft || body.clientLeft || 0,
			top  = box.top  + (self.pageYOffset || jQuery.support.boxModel && docElem.scrollTop  || body.scrollTop ) - clientTop,
			left = box.left + (self.pageXOffset || jQuery.support.boxModel && docElem.scrollLeft || body.scrollLeft) - clientLeft;

		return { top: top, left: left };
	};

} else {
	jQuery.fn.offset = function( options ) {
		var elem = this[0];

		if ( options ) { 
			return this.each(function( i ) {
				jQuery.offset.setOffset( this, options, i );
			});
		}

		if ( !elem || !elem.ownerDocument ) {
			return null;
		}

		if ( elem === elem.ownerDocument.body ) {
			return jQuery.offset.bodyOffset( elem );
		}

		jQuery.offset.initialize();

		var offsetParent = elem.offsetParent, prevOffsetParent = elem,
			doc = elem.ownerDocument, computedStyle, docElem = doc.documentElement,
			body = doc.body, defaultView = doc.defaultView,
			prevComputedStyle = defaultView ? defaultView.getComputedStyle( elem, null ) : elem.currentStyle,
			top = elem.offsetTop, left = elem.offsetLeft;

		while ( (elem = elem.parentNode) && elem !== body && elem !== docElem ) {
			if ( jQuery.offset.supportsFixedPosition && prevComputedStyle.position === "fixed" ) {
				break;
			}

			computedStyle = defaultView ? defaultView.getComputedStyle(elem, null) : elem.currentStyle;
			top  -= elem.scrollTop;
			left -= elem.scrollLeft;

			if ( elem === offsetParent ) {
				top  += elem.offsetTop;
				left += elem.offsetLeft;

				if ( jQuery.offset.doesNotAddBorder && !(jQuery.offset.doesAddBorderForTableAndCells && /^t(able|d|h)$/i.test(elem.nodeName)) ) {
					top  += parseFloat( computedStyle.borderTopWidth  ) || 0;
					left += parseFloat( computedStyle.borderLeftWidth ) || 0;
				}

				prevOffsetParent = offsetParent, offsetParent = elem.offsetParent;
			}

			if ( jQuery.offset.subtractsBorderForOverflowNotVisible && computedStyle.overflow !== "visible" ) {
				top  += parseFloat( computedStyle.borderTopWidth  ) || 0;
				left += parseFloat( computedStyle.borderLeftWidth ) || 0;
			}

			prevComputedStyle = computedStyle;
		}

		if ( prevComputedStyle.position === "relative" || prevComputedStyle.position === "static" ) {
			top  += body.offsetTop;
			left += body.offsetLeft;
		}

		if ( jQuery.offset.supportsFixedPosition && prevComputedStyle.position === "fixed" ) {
			top  += Math.max( docElem.scrollTop, body.scrollTop );
			left += Math.max( docElem.scrollLeft, body.scrollLeft );
		}

		return { top: top, left: left };
	};
}

jQuery.offset = {
	initialize: function() {
		var body = document.body, container = document.createElement("div"), innerDiv, checkDiv, table, td, bodyMarginTop = parseFloat( jQuery.curCSS(body, "marginTop", true) ) || 0,
			html = "<div style='position:absolute;top:0;left:0;margin:0;border:5px solid #000;padding:0;width:1px;height:1px;'><div></div></div><table style='position:absolute;top:0;left:0;margin:0;border:5px solid #000;padding:0;width:1px;height:1px;' cellpadding='0' cellspacing='0'><tr><td></td></tr></table>";

		jQuery.extend( container.style, { position: "absolute", top: 0, left: 0, margin: 0, border: 0, width: "1px", height: "1px", visibility: "hidden" } );

		container.innerHTML = html;
		body.insertBefore( container, body.firstChild );
		innerDiv = container.firstChild;
		checkDiv = innerDiv.firstChild;
		td = innerDiv.nextSibling.firstChild.firstChild;

		this.doesNotAddBorder = (checkDiv.offsetTop !== 5);
		this.doesAddBorderForTableAndCells = (td.offsetTop === 5);

		checkDiv.style.position = "fixed", checkDiv.style.top = "20px";
		// safari subtracts parent border width here which is 5px
		this.supportsFixedPosition = (checkDiv.offsetTop === 20 || checkDiv.offsetTop === 15);
		checkDiv.style.position = checkDiv.style.top = "";

		innerDiv.style.overflow = "hidden", innerDiv.style.position = "relative";
		this.subtractsBorderForOverflowNotVisible = (checkDiv.offsetTop === -5);

		this.doesNotIncludeMarginInBodyOffset = (body.offsetTop !== bodyMarginTop);

		body.removeChild( container );
		body = container = innerDiv = checkDiv = table = td = null;
		jQuery.offset.initialize = jQuery.noop;
	},

	bodyOffset: function( body ) {
		var top = body.offsetTop, left = body.offsetLeft;

		jQuery.offset.initialize();

		if ( jQuery.offset.doesNotIncludeMarginInBodyOffset ) {
			top  += parseFloat( jQuery.curCSS(body, "marginTop",  true) ) || 0;
			left += parseFloat( jQuery.curCSS(body, "marginLeft", true) ) || 0;
		}

		return { top: top, left: left };
	},
	
	setOffset: function( elem, options, i ) {
		// set position first, in-case top/left are set even on static elem
		if ( /static/.test( jQuery.curCSS( elem, "position" ) ) ) {
			elem.style.position = "relative";
		}
		var curElem   = jQuery( elem ),
			curOffset = curElem.offset(),
			curTop    = parseInt( jQuery.curCSS( elem, "top",  true ), 10 ) || 0,
			curLeft   = parseInt( jQuery.curCSS( elem, "left", true ), 10 ) || 0;

		if ( jQuery.isFunction( options ) ) {
			options = options.call( elem, i, curOffset );
		}

		var props = {
			top:  (options.top  - curOffset.top)  + curTop,
			left: (options.left - curOffset.left) + curLeft
		};
		
		if ( "using" in options ) {
			options.using.call( elem, props );
		} else {
			curElem.css( props );
		}
	}
};


jQuery.fn.extend({
	position: function() {
		if ( !this[0] ) {
			return null;
		}

		var elem = this[0],

		// Get *real* offsetParent
		offsetParent = this.offsetParent(),

		// Get correct offsets
		offset       = this.offset(),
		parentOffset = /^body|html$/i.test(offsetParent[0].nodeName) ? { top: 0, left: 0 } : offsetParent.offset();

		// Subtract element margins
		// note: when an element has margin: auto the offsetLeft and marginLeft
		// are the same in Safari causing offset.left to incorrectly be 0
		offset.top  -= parseFloat( jQuery.curCSS(elem, "marginTop",  true) ) || 0;
		offset.left -= parseFloat( jQuery.curCSS(elem, "marginLeft", true) ) || 0;

		// Add offsetParent borders
		parentOffset.top  += parseFloat( jQuery.curCSS(offsetParent[0], "borderTopWidth",  true) ) || 0;
		parentOffset.left += parseFloat( jQuery.curCSS(offsetParent[0], "borderLeftWidth", true) ) || 0;

		// Subtract the two offsets
		return {
			top:  offset.top  - parentOffset.top,
			left: offset.left - parentOffset.left
		};
	},

	offsetParent: function() {
		return this.map(function() {
			var offsetParent = this.offsetParent || document.body;
			while ( offsetParent && (!/^body|html$/i.test(offsetParent.nodeName) && jQuery.css(offsetParent, "position") === "static") ) {
				offsetParent = offsetParent.offsetParent;
			}
			return offsetParent;
		});
	}
});


// Create scrollLeft and scrollTop methods
jQuery.each( ["Left", "Top"], function( i, name ) {
	var method = "scroll" + name;

	jQuery.fn[ method ] = function(val) {
		var elem = this[0], win;
		
		if ( !elem ) {
			return null;
		}

		if ( val !== undefined ) {
			// Set the scroll offset
			return this.each(function() {
				win = getWindow( this );

				if ( win ) {
					win.scrollTo(
						!i ? val : jQuery(win).scrollLeft(),
						 i ? val : jQuery(win).scrollTop()
					);

				} else {
					this[ method ] = val;
				}
			});
		} else {
			win = getWindow( elem );

			// Return the scroll offset
			return win ? ("pageXOffset" in win) ? win[ i ? "pageYOffset" : "pageXOffset" ] :
				jQuery.support.boxModel && win.document.documentElement[ method ] ||
					win.document.body[ method ] :
				elem[ method ];
		}
	};
});

function getWindow( elem ) {
	return ("scrollTo" in elem && elem.document) ?
		elem :
		elem.nodeType === 9 ?
			elem.defaultView || elem.parentWindow :
			false;
}
// Create innerHeight, innerWidth, outerHeight and outerWidth methods
jQuery.each([ "Height", "Width" ], function( i, name ) {

	var type = name.toLowerCase();

	// innerHeight and innerWidth
	jQuery.fn["inner" + name] = function() {
		return this[0] ?
			jQuery.css( this[0], type, false, "padding" ) :
			null;
	};

	// outerHeight and outerWidth
	jQuery.fn["outer" + name] = function( margin ) {
		return this[0] ?
			jQuery.css( this[0], type, false, margin ? "margin" : "border" ) :
			null;
	};

	jQuery.fn[ type ] = function( size ) {
		// Get window width or height
		var elem = this[0];
		if ( !elem ) {
			return size == null ? null : this;
		}
		
		if ( jQuery.isFunction( size ) ) {
			return this.each(function( i ) {
				var self = jQuery( this );
				self[ type ]( size.call( this, i, self[ type ]() ) );
			});
		}

		return ("scrollTo" in elem && elem.document) ? // does it walk and quack like a window?
			// Everyone else use document.documentElement or document.body depending on Quirks vs Standards mode
			elem.document.compatMode === "CSS1Compat" && elem.document.documentElement[ "client" + name ] ||
			elem.document.body[ "client" + name ] :

			// Get document width or height
			(elem.nodeType === 9) ? // is it a document
				// Either scroll[Width/Height] or offset[Width/Height], whichever is greater
				Math.max(
					elem.documentElement["client" + name],
					elem.body["scroll" + name], elem.documentElement["scroll" + name],
					elem.body["offset" + name], elem.documentElement["offset" + name]
				) :

				// Get or set width or height on the element
				size === undefined ?
					// Get width or height on the element
					jQuery.css( elem, type ) :

					// Set the width or height on the element (default to pixels if value is unitless)
					this.css( type, typeof size === "string" ? size : size + "px" );
	};

});
// Expose jQuery to the global object
window.jQuery = window.$ = jQuery;

})(window);
/**
 * Cookie plugin
 *
 * Copyright (c) 2006 Klaus Hartl (stilbuero.de)
 * Dual licensed under the MIT and GPL licenses:
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.gnu.org/licenses/gpl.html
 *
 */

/**
 * Create a cookie with the given name and value and other optional parameters.
 *
 * @example $.cookie('the_cookie', 'the_value');
 * @desc Set the value of a cookie.
 * @example $.cookie('the_cookie', 'the_value', { expires: 7, path: '/', domain: 'jquery.com', secure: true });
 * @desc Create a cookie with all available options.
 * @example $.cookie('the_cookie', 'the_value');
 * @desc Create a session cookie.
 * @example $.cookie('the_cookie', null);
 * @desc Delete a cookie by passing null as value. Keep in mind that you have to use the same path and domain
 *       used when the cookie was set.
 *
 * @param String name The name of the cookie.
 * @param String value The value of the cookie.
 * @param Object options An object literal containing key/value pairs to provide optional cookie attributes.
 * @option Number|Date expires Either an integer specifying the expiration date from now on in days or a Date object.
 *                             If a negative value is specified (e.g. a date in the past), the cookie will be deleted.
 *                             If set to null or omitted, the cookie will be a session cookie and will not be retained
 *                             when the the browser exits.
 * @option String path The value of the path atribute of the cookie (default: path of page that created the cookie).
 * @option String domain The value of the domain attribute of the cookie (default: domain of page that created the cookie).
 * @option Boolean secure If true, the secure attribute of the cookie will be set and the cookie transmission will
 *                        require a secure protocol (like HTTPS).
 * @type undefined
 *
 * @name $.cookie
 * @cat Plugins/Cookie
 * @author Klaus Hartl/klaus.hartl@stilbuero.de
 */

/**
 * Get the value of a cookie with the given name.
 *
 * @example $.cookie('the_cookie');
 * @desc Get the value of a cookie.
 *
 * @param String name The name of the cookie.
 * @return The value of the cookie.
 * @type String
 *
 * @name $.cookie
 * @cat Plugins/Cookie
 * @author Klaus Hartl/klaus.hartl@stilbuero.de
 */
jQuery.cookie = function(name, value, options) {
    if (typeof value != 'undefined') { // name and value given, set cookie
        options = options || {};
        if (value === null) {
            value = '';
            options.expires = -1;
        }
        var expires = '';
        if (options.expires && (typeof options.expires == 'number' || options.expires.toUTCString)) {
            var date;
            if (typeof options.expires == 'number') {
                date = new Date();
                date.setTime(date.getTime() + (options.expires * 24 * 60 * 60 * 1000));
            } else {
                date = options.expires;
            }
            expires = '; expires=' + date.toUTCString(); // use expires attribute, max-age is not supported by IE
        }
        // CAUTION: Needed to parenthesize options.path and options.domain
        // in the following expressions, otherwise they evaluate to undefined
        // in the packed version for some reason...
        var path = options.path ? '; path=' + (options.path) : '';
        var domain = options.domain ? '; domain=' + (options.domain) : '';
        var secure = options.secure ? '; secure' : '';
        document.cookie = [name, '=', encodeURIComponent(value), expires, path, domain, secure].join('');
    } else { // only name given, get cookie
        var cookieValue = null;
        if (document.cookie && document.cookie != '') {
            var cookies = document.cookie.split(';');
            for (var i = 0; i < cookies.length; i++) {
                var cookie = jQuery.trim(cookies[i]);
                // Does this cookie string begin with the name we want?
                if (cookie.substring(0, name.length + 1) == (name + '=')) {
                    cookieValue = decodeURIComponent(cookie.substring(name.length + 1));
                    break;
                }
            }
        }
        return cookieValue;
    }
};
/* Copyright (c) 2006 Brandon Aaron (http://brandonaaron.net)
 * Dual licensed under the MIT (http://www.opensource.org/licenses/mit-license.php) 
 * and GPL (http://www.opensource.org/licenses/gpl-license.php) licenses.
 *
 * $LastChangedDate: 2007-06-20 03:23:36 +0200 (Mi, 20 Jun 2007) $
 * $Rev: 2110 $
 *
 * Version 2.1
 */

(function($){

/**
 * The bgiframe is chainable and applies the iframe hack to get 
 * around zIndex issues in IE6. It will only apply itself in IE 
 * and adds a class to the iframe called 'bgiframe'. The iframe
 * is appeneded as the first child of the matched element(s) 
 * with a tabIndex and zIndex of -1.
 * 
 * By default the plugin will take borders, sized with pixel units,
 * into account. If a different unit is used for the border's width,
 * then you will need to use the top and left settings as explained below.
 *
 * NOTICE: This plugin has been reported to cause perfromance problems
 * when used on elements that change properties (like width, height and
 * opacity) a lot in IE6. Most of these problems have been caused by 
 * the expressions used to calculate the elements width, height and 
 * borders. Some have reported it is due to the opacity filter. All 
 * these settings can be changed if needed as explained below.
 *
 * @example $('div').bgiframe();
 * @before <div><p>Paragraph</p></div>
 * @result <div><iframe class="bgiframe".../><p>Paragraph</p></div>
 *
 * @param Map settings Optional settings to configure the iframe.
 * @option String|Number top The iframe must be offset to the top
 * 		by the width of the top border. This should be a negative 
 *      number representing the border-top-width. If a number is 
 * 		is used here, pixels will be assumed. Otherwise, be sure
 *		to specify a unit. An expression could also be used. 
 * 		By default the value is "auto" which will use an expression 
 * 		to get the border-top-width if it is in pixels.
 * @option String|Number left The iframe must be offset to the left
 * 		by the width of the left border. This should be a negative 
 *      number representing the border-left-width. If a number is 
 * 		is used here, pixels will be assumed. Otherwise, be sure
 *		to specify a unit. An expression could also be used. 
 * 		By default the value is "auto" which will use an expression 
 * 		to get the border-left-width if it is in pixels.
 * @option String|Number width This is the width of the iframe. If
 *		a number is used here, pixels will be assume. Otherwise, be sure
 * 		to specify a unit. An experssion could also be used.
 *		By default the value is "auto" which will use an experssion
 * 		to get the offsetWidth.
 * @option String|Number height This is the height of the iframe. If
 *		a number is used here, pixels will be assume. Otherwise, be sure
 * 		to specify a unit. An experssion could also be used.
 *		By default the value is "auto" which will use an experssion
 * 		to get the offsetHeight.
 * @option Boolean opacity This is a boolean representing whether or not
 * 		to use opacity. If set to true, the opacity of 0 is applied. If
 *		set to false, the opacity filter is not applied. Default: true.
 * @option String src This setting is provided so that one could change 
 *		the src of the iframe to whatever they need.
 *		Default: "javascript:false;"
 *
 * @name bgiframe
 * @type jQuery
 * @cat Plugins/bgiframe
 * @author Brandon Aaron (brandon.aaron@gmail.com || http://brandonaaron.net)
 */
$.fn.bgIframe = $.fn.bgiframe = function(s) {
	// This is only for IE6
	if ( $.browser.msie && parseInt($.browser.version) <= 6 ) {
		s = $.extend({
			top     : 'auto', // auto == .currentStyle.borderTopWidth
			left    : 'auto', // auto == .currentStyle.borderLeftWidth
			width   : 'auto', // auto == offsetWidth
			height  : 'auto', // auto == offsetHeight
			opacity : true,
			src     : 'javascript:false;'
		}, s || {});
		var prop = function(n){return n&&n.constructor==Number?n+'px':n;},
		    html = '<iframe class="bgiframe"frameborder="0"tabindex="-1"src="'+s.src+'"'+
		               'style="display:block;position:absolute;z-index:-1;'+
			               (s.opacity !== false?'filter:Alpha(Opacity=\'0\');':'')+
					       'top:'+(s.top=='auto'?'expression(((parseInt(this.parentNode.currentStyle.borderTopWidth)||0)*-1)+\'px\')':prop(s.top))+';'+
					       'left:'+(s.left=='auto'?'expression(((parseInt(this.parentNode.currentStyle.borderLeftWidth)||0)*-1)+\'px\')':prop(s.left))+';'+
					       'width:'+(s.width=='auto'?'expression(this.parentNode.offsetWidth+\'px\')':prop(s.width))+';'+
					       'height:'+(s.height=='auto'?'expression(this.parentNode.offsetHeight+\'px\')':prop(s.height))+';'+
					'"/>';
		return this.each(function() {
			if ( $('> iframe.bgiframe', this).length == 0 )
				this.insertBefore( document.createElement(html), this.firstChild );
		});
	}
	return this;
};

// Add browser.version if it doesn't exist
if (!$.browser.version)
	$.browser.version = navigator.userAgent.toLowerCase().match(/.+(?:rv|it|ra|ie)[\/: ]([\d.]+)/)[1];

})(jQuery);
/*

jQuery Browser Plugin
* Version 2.3
* 2008-09-17 19:27:05
* URL: http://jquery.thewikies.com/browser
* Description: jQuery Browser Plugin extends browser detection capabilities and can assign browser selectors to CSS classes.
* Author: Nate Cavanaugh, Minhchau Dang, & Jonathan Neal
* Copyright: Copyright (c) 2008 Jonathan Neal under dual MIT/GPL license.
* JSLint: This javascript file passes JSLint verification.
*/
/*jslint
bitwise: true,
browser: true,
eqeqeq: true,
forin: true,
nomen: true,
plusplus: true,
undef: true,
white: true
*/
/*global
jQuery
*/

(function($) {
    $.browserTest = function(a, z) {
        var u = 'unknown', x = 'X', m = function(r, h) {
            for (var i = 0; i < h.length; i = i + 1) {
                r = r.replace(h[i][0], h[i][1]);
            }

            return r;
        }, c = function(i, a, b, c) {
            var r = {
                name: m((a.exec(i) || [u, u])[1], b)
            };

            r[r.name] = true;

            r.version = (c.exec(i) || [x, x, x, x])[3];

            if (r.name.match(/safari/) && r.version > 400) {
                r.version = '2.0';
            }

            if (r.name === 'presto') {
                r.version = ($.browser.version > 9.27) ? 'futhark' : 'linear_b';
            }
            r.versionNumber = parseFloat(r.version, 10) || 0;
            r.versionX = (r.version !== x) ? (r.version + '').substr(0, 1) : x;
            r.className = r.name + r.versionX;

            return r;
        };

        a = (a.match(/Opera|Navigator|Minefield|KHTML|Chrome/) ? m(a, [
			[/(Firefox|MSIE|KHTML,\slike\sGecko|Konqueror)/, ''],
			['Chrome Safari', 'Chrome'],
			['KHTML', 'Konqueror'],
			['Minefield', 'Firefox'],
			['Navigator', 'Netscape']
		]) : a).toLowerCase();

        $.browser = $.extend((!z) ? $.browser : {}, c(a, /(camino|chrome|firefox|netscape|konqueror|lynx|msie|opera|safari)/, [], /(camino|chrome|firefox|netscape|netscape6|opera|version|konqueror|lynx|msie|safari)(\/|\s)([a-z0-9\.\+]*?)(\;|dev|rel|\s|$|\))/));

        $.layout = c(a, /(gecko|konqueror|msie|opera|webkit)/, [
			['konqueror', 'khtml'],
			['msie', 'trident'],
			['opera', 'presto']
		], /(applewebkit|rv|konqueror|msie)(\:|\/|\s)([a-z0-9\.]*?)(\;|\)|\s)/);

        $.os = {
            name: (/(win|mac|linux|sunos|solaris|iphone)/.exec(navigator.platform.toLowerCase()) || [u])[0].replace('sunos', 'solaris')
        };

        if (!z) {
            $('html').addClass([$.os.name, $.browser.name, $.browser.className, $.layout.name, $.layout.className].join(' '));
        }
    };

    $.browserTest(navigator.userAgent);
})(jQuery);
/*
 * jQuery history plugin
 * 
 * The MIT License
 * 
 * Copyright (c) 2006-2009 Taku Sano (Mikage Sawatari)
 * Copyright (c) 2010 Takayuki Miwa
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

(function($) {
    var locationWrapper = {
        put: function(hash, win) {
            (win || window).location.hash = this.encoder(hash);
        },
        get: function(win) {
            var hash = ((win || window).location.hash).replace(/^#/, '');
            try {
                return $.browser.mozilla ? hash : decodeURIComponent(hash);
            }
            catch (error) {
                return hash;
            }
        },
        encoder: encodeURIComponent
    };

    var iframeWrapper = {
        id: "__jQuery_history",
        init: function() {
            var html = '<iframe id="'+ this.id +'" style="display:none" src="javascript:false;" />';
            $("body").prepend(html);
            return this;
        },
        _document: function() {
            return $("#"+ this.id)[0].contentWindow.document;
        },
        put: function(hash) {
            var doc = this._document();
            doc.open();
            doc.close();
            locationWrapper.put(hash, doc);
        },
        get: function() {
            return locationWrapper.get(this._document());
        }
    };

    function initObjects(options) {
        options = $.extend({
                unescape: false
            }, options || {});

        locationWrapper.encoder = encoder(options.unescape);

        function encoder(unescape_) {
            if(unescape_ === true) {
                return function(hash){ return hash; };
            }
            if(typeof unescape_ == "string" &&
               (unescape_ = partialDecoder(unescape_.split("")))
               || typeof unescape_ == "function") {
                return function(hash) { return unescape_(encodeURIComponent(hash)); };
            }
            return encodeURIComponent;
        }

        function partialDecoder(chars) {
            var re = new RegExp($.map(chars, encodeURIComponent).join("|"), "ig");
            return function(enc) { return enc.replace(re, decodeURIComponent); };
        }
    }

    var implementations = {};

    implementations.base = {
        callback: undefined,
        type: undefined,

        check: function() {},
        load:  function(hash) {},
        init:  function(callback, options) {
            initObjects(options);
            self.callback = callback;
            self._options = options;
            self._init();
        },

        _init: function() {},
        _options: {}
    };

    implementations.timer = {
        _appState: undefined,
        _init: function() {
            var current_hash = locationWrapper.get();
            self._appState = current_hash;
            self.callback(current_hash);
            setInterval(self.check, 100);
        },
        check: function() {
            var current_hash = locationWrapper.get();
            if(current_hash != self._appState) {
                self._appState = current_hash;
                self.callback(current_hash);
            }
        },
        load: function(hash) {
            if(hash != self._appState) {
                locationWrapper.put(hash);
                self._appState = hash;
                self.callback(hash);
            }
        }
    };

    implementations.iframeTimer = {
        _appState: undefined,
        _init: function() {
            var current_hash = locationWrapper.get();
            self._appState = current_hash;
            iframeWrapper.init().put(current_hash);
            self.callback(current_hash);
            setInterval(self.check, 100);
        },
        check: function() {
            var iframe_hash = iframeWrapper.get(),
                location_hash = locationWrapper.get();

            if (location_hash != iframe_hash) {
                if (location_hash == self._appState) {    // user used Back or Forward button
                    self._appState = iframe_hash;
                    locationWrapper.put(iframe_hash);
                    self.callback(iframe_hash); 
                } else {                              // user loaded new bookmark
                    self._appState = location_hash;  
                    iframeWrapper.put(location_hash);
                    self.callback(location_hash);
                }
            }
        },
        load: function(hash) {
            if(hash != self._appState) {
                locationWrapper.put(hash);
                iframeWrapper.put(hash);
                self._appState = hash;
                self.callback(hash);
            }
        }
    };

    implementations.hashchangeEvent = {
        _init: function() {
            self.callback(locationWrapper.get());
            $(window).bind('hashchange', self.check);
        },
        check: function() {
            self.callback(locationWrapper.get());
        },
        load: function(hash) {
            locationWrapper.put(hash);
        }
    };

    implementations.html5StateHistory = {
        _init: function() {
            self.callback(locationWrapper.get());
            $(window).bind('hashchange', self.check);
            $(window).bind('popstate', self.html5check);
        },
        check: function() {
            self.callback(locationWrapper.get());
        },
        html5check: function(e) {
            if(!arguments.callee.first_run) {
                arguments.callee.first_run = true;
                return;
            }
            if(location.hash != "") {
                self.check();
            } else {
                self.callback(location.pathname);
            }
        },
        load: function(hash) {
            if(!hash.indexOf('/')) {
                history.pushState({history: true}, "", hash);
                self.html5check();
            } else {
                locationWrapper.put(hash);
            }
        }
    };

    var self = $.extend({}, implementations.base);

    if($.browser.msie && ($.browser.version < 8 || document.documentMode < 8)) {
        self.type = 'iframeTimer';
    } else if('pushState' in history) {
        self.type = 'html5StateHistory';
    } else if("onhashchange" in window) {
        self.type = 'hashchangeEvent';
    } else {
        self.type = 'timer';
    }

    $.extend(self, implementations[self.type]);
    $.history = self;
})(jQuery);

/*! Copyright (c) 2009 Brandon Aaron (http://brandonaaron.net)
 * Dual licensed under the MIT (http://www.opensource.org/licenses/mit-license.php)
 * and GPL (http://www.opensource.org/licenses/gpl-license.php) licenses.
 * Thanks to: http://adomas.org/javascript-mouse-wheel/ for some pointers.
 * Thanks to: Mathias Bank(http://www.mathias-bank.de) for a scope bug fix.
 *
 * Version: 3.0.2
 * 
 * Requires: 1.2.2+
 */

(function($) {

var types = ['DOMMouseScroll', 'mousewheel'];

$.event.special.mousewheel = {
	setup: function() {
		if ( this.addEventListener )
			for ( var i=types.length; i; )
				this.addEventListener( types[--i], handler, false );
		else
			this.onmousewheel = handler;
	},
	
	teardown: function() {
		if ( this.removeEventListener )
			for ( var i=types.length; i; )
				this.removeEventListener( types[--i], handler, false );
		else
			this.onmousewheel = null;
	}
};

$.fn.extend({
	mousewheel: function(fn) {
		return fn ? this.bind("mousewheel", fn) : this.trigger("mousewheel");
	},
	
	unmousewheel: function(fn) {
		return this.unbind("mousewheel", fn);
	}
});


function handler(event) {
	var args = [].slice.call( arguments, 1 ), delta = 0, returnValue = true;
	
	event = $.event.fix(event || window.event);
	event.type = "mousewheel";
	
	if ( event.wheelDelta ) delta = event.wheelDelta/120;
	if ( event.detail     ) delta = -event.detail/3;
	
	// Add events and delta to the front of the arguments
	args.unshift(event, delta);

	return $.event.handle.apply(this, args);
}

})(jQuery);
/*
    Modified version of dynDateTime. Please, don't override it with original plugin.
*/

(function($) {

	$.fn.dynDateTime = function(options) {
	  
		  // plugin defaults
		  $.fn.dynDateTime.defaults = {
			inputField:    null,
			displayArea:  null,
			button:       null,
			eventName:    "click",
			ifFormat:     "%Y/%m/%d",
			daFormat:     "%Y/%m/%d",
			singleClick:  true,
			disableFunc:  null,
			dateStatusFunc: null,
			dateText:     null,
			firstDay:     null,
			align:        "Br",
			range:        [1900, 2999],
			weekNumbers:  true,
			flat:         null,
			flatCallback: null,
			onSelect:     null,
			onClose:      null,
			onUpdate:     null,
			date:         null,
			showsTime:    false,
			timeFormat:   "24",
			electric:     true,
			step:         2,
			position:     null,
			cache:        false,
			showOthers:   false,
			multiple:     null,
			debug:        false
		  };		
		
		var opts = $.extend({}, $.fn.dynDateTime.defaults, options);
	
		return this.each(function() {
		
			//opts are shared per jQuery call; create per inputField closures of non-shared values for each instance
			var this_inputField = this;
			var this_displayArea = null;
			var this_button = null;
			var this_flat = null;
			if(opts.displayArea) {
				try {
					this_displayArea = eval("jQuery(this)" + opts.displayArea + ".get(0);");
				} catch (err) {
					opts.displayArea = null;
				}
			}
			if(opts.button) {
				try {
					this_button = eval("jQuery(this)" + opts.button + ".get(0);");
				} catch (err) {
					opts.button = null;
				}
			}
			if(opts.flat) {
				try {
					this_flat = eval("jQuery(this)" + opts.flat + ".get(0);");
				} catch (err) {
					opts.flat = null;
				}
			}

			
		
			if (!(this_flat || opts.multiple || this_inputField || this_displayArea || this_button)) {
				log("opts");
				//nothing to do
				return;
			}

            if (/*this_inputField && */opts.date) {
                this_inputField.value = opts.date.print(opts.ifFormat);
            }
			
			//default onSelect
			function onSelect(cal) {
				var p = cal.opts;
				var update = (cal.dateClicked || p.electric);
				if (update && this_inputField) {
					this_inputField.value = cal.date.print(p.ifFormat);
					if (typeof this_inputField.onchange == "function")
						this_inputField.onchange();
				}
				if (update && this_displayArea)
					this_displayArea.innerHTML = cal.date.print(p.daFormat);
				if (update && typeof p.onUpdate == "function")
					p.onUpdate(cal);
				if (update && p.flat) {
					if (typeof p.flatCallback == "function")
						p.flatCallback(cal);
				}

                if (opts.onSelect) {
                    opts.onSelect(cal);
                }

				if (update && p.singleClick && cal.dateClicked)
					cal.callCloseHandler();
			};
		
			//flat setup
			if (this_flat != null) {

				var cal = new DynCalendar(opts.firstDay, opts.date, onSelect);
				cal.showsOtherMonths = opts.showOthers;
				cal.showsTime = opts.showsTime;
				cal.time24 = (opts.timeFormat == "24");
				cal.opts = opts;
				cal.weekNumbers = opts.weekNumbers;
				cal.setRange(opts.range[0], opts.range[1]);
				cal.setDateStatusHandler(opts.dateStatusFunc);
				cal.getDateText = opts.dateText;
				if (opts.ifFormat) {
					cal.setDateFormat(opts.ifFormat);
				}
				if (this_inputField && typeof this_inputField.value == "string") {
					//cal.parseDate(this_inputField.value);
					log("rar");
				}
				cal.create(this_flat);
				cal.show();
				return;
			}
		
			//find the element to mount show/hide event 
			var triggerEl = this_button || this_displayArea || this_inputField  ;

			//bolt on event
			triggerEl["on" + opts.eventName] = function() {
				log("clicked");
				var dateEl = this_inputField || this_displayArea;
				var dateFmt = this_inputField ? opts.ifFormat : opts.daFormat;
				var mustCreate = false;
				var cal = triggerEl.__dynCalendar;
				if (dateEl)
					opts.date = Date.parseDate(dateEl.value || dateEl.innerHTML, dateFmt);
				if (!(cal && opts.cache)) {
					triggerEl.__dynCalendar = cal = new DynCalendar(opts.firstDay,
									     opts.date,
									     onSelect,
									     opts.onClose || function(cal) { cal.hide(); });
					cal.showsTime = opts.showsTime;
					cal.time24 = (opts.timeFormat == "24");
					cal.weekNumbers = opts.weekNumbers;
					mustCreate = true;
				} else {
					if (opts.date)
						cal.setDate(opts.date);
					cal.hide();
				}
				if (opts.multiple) {
					cal.multiple = {};
					for (var i = opts.multiple.length; --i >= 0;) {
						var d = opts.multiple[i];
						var ds = d.print("%Y%m%d");
						cal.multiple[ds] = d;
					}
				}
				cal.showsOtherMonths = opts.showOthers;
				cal.yearStep = opts.step;
				cal.setRange(opts.range[0], opts.range[1]);
				cal.opts = opts;
				cal.setDateStatusHandler(opts.dateStatusFunc);
				cal.getDateText = opts.dateText;
				cal.setDateFormat(dateFmt);
				if (mustCreate)
					cal.create();
				cal.refresh();
				if (!opts.position)
					cal.showAtElement(this_button || this_displayArea || this_inputField, opts.align);
				else
					cal.showAt(opts.position[0], opts.position[1]);

                if (opts.onOpen) {
                    opts.onOpen(cal);
                }

				return false;
			};
		});
		
		// private function for debugging
		function log(msg) {
			if(opts.debug) {
				window.loadFirebugConsole(); // file:// based access seems to disable this for me!
				if (window.console && window.console.log ) window.console.log("dynDateTime: " + msg);
			}
		};
	};
})(jQuery);

// Begin old dynarch code

/*  Copyright Mihai Bazon, 2002-2005  |  www.bazon.net/mishoo
 * -----------------------------------------------------------
 *
 * The DHTML DynCalendar, version 1.0 "It is happening again"
 *
 * Details and latest version at:
 * www.dynarch.com/projects/calendar
 *
 * This script is developed by Dynarch.com.  Visit us at www.dynarch.com.
 *
 * This script is distributed under the GNU Lesser General Public License.
 * Read the entire license text here: http://www.gnu.org/licenses/lgpl.html
 */

// $Id: calendar.js,v 1.51 2005/03/07 16:44:31 mishoo Exp $

/** The DynCalendar object constructor. */
DynCalendar = function (firstDayOfWeek, dateStr, onSelected, onClose) {
	// member variables
	this.activeDiv = null;
	this.currentDateEl = null;
	this.getDateStatus = null;
	this.getDateToolTip = null;
	this.getDateText = null;
	this.timeout = null;
	this.onSelected = onSelected || null;
	this.onClose = onClose || null;
	this.dragging = false;
	this.hidden = false;
	this.minYear = 1970;
	this.maxYear = 2050;
	this.dateFormat = DynCalendar._TT["DEF_DATE_FORMAT"];
	this.ttDateFormat = DynCalendar._TT["TT_DATE_FORMAT"];
	this.isPopup = true;
	this.weekNumbers = true;
	this.firstDayOfWeek = typeof firstDayOfWeek == "number" ? firstDayOfWeek : DynCalendar._FD; // 0 for Sunday, 1 for Monday, etc.
	this.showsOtherMonths = false;
	this.dateStr = dateStr;
	this.ar_days = null;
	this.showsTime = false;
	this.time24 = true;
	this.yearStep = 2;
	this.hiliteToday = true;
	this.multiple = null;
	// HTML elements
	this.table = null;
	this.element = null;
	this.tbody = null;
	this.firstdayname = null;
	// Combo boxes
	this.monthsCombo = null;
	this.yearsCombo = null;
	this.hilitedMonth = null;
	this.activeMonth = null;
	this.hilitedYear = null;
	this.activeYear = null;
	// Information
	this.dateClicked = false;

	// one-time initializations
	if (typeof DynCalendar._SDN == "undefined") {
		// table of short day names
		if (typeof DynCalendar._SDN_len == "undefined")
			DynCalendar._SDN_len = 3;
		var ar = new Array();
		for (var i = 8; i > 0;) {
			ar[--i] = DynCalendar._DN[i].substr(0, DynCalendar._SDN_len);
		}
		DynCalendar._SDN = ar;
		// table of short month names
		if (typeof DynCalendar._SMN_len == "undefined")
			DynCalendar._SMN_len = 3;
		ar = new Array();
		for (var i = 12; i > 0;) {
			ar[--i] = DynCalendar._MN[i].substr(0, DynCalendar._SMN_len);
		}
		DynCalendar._SMN = ar;
	}
};

// ** constants

/// "static", needed for event handlers.
DynCalendar._C = null;

/// detect a special case of "web browser"
DynCalendar.is_ie = ( /msie/i.test(navigator.userAgent) &&
		   !/opera/i.test(navigator.userAgent) );

DynCalendar.is_ie5 = ( DynCalendar.is_ie && /msie 5\.0/i.test(navigator.userAgent) );

/// detect Opera browser
DynCalendar.is_opera = /opera/i.test(navigator.userAgent);

/// detect KHTML-based browsers
DynCalendar.is_khtml = /Konqueror|Safari|KHTML/i.test(navigator.userAgent);

// BEGIN: UTILITY FUNCTIONS; beware that these might be moved into a separate
//        library, at some point.

DynCalendar.getAbsolutePos = function(el) {
	var SL = 0, ST = 0;
	var is_div = /^div$/i.test(el.tagName);
	if (is_div && el.scrollLeft)
		
		SL = el.scrollLeft;
	if (is_div && el.scrollTop)
		ST = el.scrollTop;
	var r = { x: el.offsetLeft - SL, y: el.offsetTop - ST };
	if (el.offsetParent) {
		var tmp = this.getAbsolutePos(el.offsetParent);
		r.x += tmp.x;
		r.y += tmp.y;
	}
	return r;
};

DynCalendar.isRelated = function (el, evt) {
	var related = evt.relatedTarget;
	if (!related) {
		var type = evt.type;
		if (type == "mouseover") {
			related = evt.fromElement;
		} else if (type == "mouseout") {
			related = evt.toElement;
		}
	}
	while (related) {
		if (related == el) {
			return true;
		}
		related = related.parentNode;
	}
	return false;
};

DynCalendar.removeClass = function(el, className) {
	if (!(el && el.className)) {
		return;
	}
	var cls = el.className.split(" ");
	var ar = new Array();
	for (var i = cls.length; i > 0;) {
		if (cls[--i] != className) {
			ar[ar.length] = cls[i];
		}
	}
	el.className = ar.join(" ");
};

DynCalendar.addClass = function(el, className) {
	DynCalendar.removeClass(el, className);
	el.className += " " + className;
};

// FIXME: the following 2 functions totally suck, are useless and should be replaced immediately.
DynCalendar.getElement = function(ev) {
	var f = DynCalendar.is_ie ? window.event.srcElement : ev.currentTarget;
	while (f.nodeType != 1 || /^div$/i.test(f.tagName))
		f = f.parentNode;
	return f;
};

DynCalendar.getTargetElement = function(ev) {
	var f = DynCalendar.is_ie ? window.event.srcElement : ev.target;
	while (f.nodeType != 1)
		f = f.parentNode;
	return f;
};

DynCalendar.stopEvent = function(ev) {
	ev || (ev = window.event);
	if (DynCalendar.is_ie) {
		ev.cancelBubble = true;
		ev.returnValue = false;
	} else {
		ev.preventDefault();
		ev.stopPropagation();
	}
	return false;
};

DynCalendar.addEvent = function(el, evname, func) {
	if (el.attachEvent) { // IE
		el.attachEvent("on" + evname, func);
	} else if (el.addEventListener) { // Gecko / W3C
		el.addEventListener(evname, func, true);
	} else {
		el["on" + evname] = func;
	}
};

DynCalendar.removeEvent = function(el, evname, func) {
	if (el.detachEvent) { // IE
		el.detachEvent("on" + evname, func);
	} else if (el.removeEventListener) { // Gecko / W3C
		el.removeEventListener(evname, func, true);
	} else {
		el["on" + evname] = null;
	}
};

DynCalendar.createElement = function(type, parent) {
	var el = null;
	if (document.createElementNS) {
		// use the XHTML namespace; IE won't normally get here unless
		// _they_ "fix" the DOM2 implementation.
		el = document.createElementNS("http://www.w3.org/1999/xhtml", type);
	} else {
		el = document.createElement(type);
	}
	if (typeof parent != "undefined") {
		parent.appendChild(el);
	}
	return el;
};

// END: UTILITY FUNCTIONS

// BEGIN: CALENDAR STATIC FUNCTIONS

/** Internal -- adds a set of events to make some element behave like a button. */
DynCalendar._add_evs = function(el) {
	with (DynCalendar) {
		addEvent(el, "mouseover", dayMouseOver);
		addEvent(el, "mousedown", dayMouseDown);
		addEvent(el, "mouseout", dayMouseOut);
		if (is_ie) {
			addEvent(el, "dblclick", dayMouseDblClick);
			el.setAttribute("unselectable", true);
		}
	}
};

DynCalendar.findMonth = function(el) {
	if (typeof el.month != "undefined") {
		return el;
	} else if (!!el.parentNode && typeof el.parentNode.month != "undefined") {
		return el.parentNode;
	}
	return null;
};

DynCalendar.findYear = function(el) {
	if (typeof el.year != "undefined") {
		return el;
	} else if (!!el.parentNode && typeof el.parentNode.year != "undefined") {
		return el.parentNode;
	}
	return null;
};

DynCalendar.showMonthsCombo = function () {
	var cal = DynCalendar._C;
	if (!cal) {
		return false;
	}
	var cal = cal;
	var cd = cal.activeDiv;
	var mc = cal.monthsCombo;
	if (cal.hilitedMonth) {
		DynCalendar.removeClass(cal.hilitedMonth, "hilite");
	}
	if (cal.activeMonth) {
		DynCalendar.removeClass(cal.activeMonth, "active");
	}
	var mon = cal.monthsCombo.getElementsByTagName("div")[cal.date.getMonth()];
	DynCalendar.addClass(mon, "active");
	cal.activeMonth = mon;
	var s = mc.style;
	s.display = "block";
	if (cd.navtype < 0)
		s.left = cd.offsetLeft + "px";
	else {
		var mcw = mc.offsetWidth;
		if (typeof mcw == "undefined")
			// Konqueror brain-dead techniques
			mcw = 50;
		s.left = (cd.offsetLeft + cd.offsetWidth - mcw) + "px";
	}
	s.top = (cd.offsetTop + cd.offsetHeight) + "px";
};

DynCalendar.showYearsCombo = function (fwd) {
	var cal = DynCalendar._C;
	if (!cal) {
		return false;
	}
	var cal = cal;
	var cd = cal.activeDiv;
	var yc = cal.yearsCombo;
	if (cal.hilitedYear) {
		DynCalendar.removeClass(cal.hilitedYear, "hilite");
	}
	if (cal.activeYear) {
		DynCalendar.removeClass(cal.activeYear, "active");
	}
	cal.activeYear = null;
	var Y = cal.date.getFullYear() + (fwd ? 1 : -1);
	var yr = yc.firstChild;
	var show = false;
	for (var i = 12; i > 0; --i) {
		if (Y >= cal.minYear && Y <= cal.maxYear) {
			yr.innerHTML = Y;
			yr.year = Y;
			yr.style.display = "block";
			show = true;
		} else {
			yr.style.display = "none";
		}
		yr = yr.nextSibling;
		Y += fwd ? cal.yearStep : -cal.yearStep;
	}
	if (show) {
		var s = yc.style;
		s.display = "block";
		if (cd.navtype < 0)
			s.left = cd.offsetLeft + "px";
		else {
			var ycw = yc.offsetWidth;
			if (typeof ycw == "undefined")
				// Konqueror brain-dead techniques
				ycw = 50;
			s.left = (cd.offsetLeft + cd.offsetWidth - ycw) + "px";
		}
		s.top = (cd.offsetTop + cd.offsetHeight) + "px";
	}
};

// event handlers

DynCalendar.tableMouseUp = function(ev) {
	var cal = DynCalendar._C;
	if (!cal) {
		return false;
	}
	if (cal.timeout) {
		clearTimeout(cal.timeout);
	}
	var el = cal.activeDiv;
	if (!el) {
		return false;
	}
	var target = DynCalendar.getTargetElement(ev);
	ev || (ev = window.event);
	DynCalendar.removeClass(el, "active");
	if (target == el || target.parentNode == el) {
		DynCalendar.cellClick(el, ev);
	}
	var mon = DynCalendar.findMonth(target);
	var date = null;
	if (mon) {
		date = new Date(cal.date);
		if (mon.month != date.getMonth()) {
			date.setMonth(mon.month);
			cal.setDate(date);
			cal.dateClicked = false;
			cal.callHandler();
		}
	} else {
		var year = DynCalendar.findYear(target);
		if (year) {
			date = new Date(cal.date);
			if (year.year != date.getFullYear()) {
				date.setFullYear(year.year);
				cal.setDate(date);
				cal.dateClicked = false;
				cal.callHandler();
			}
		}
	}
	with (DynCalendar) {
		removeEvent(document, "mouseup", tableMouseUp);
		removeEvent(document, "mouseover", tableMouseOver);
		removeEvent(document, "mousemove", tableMouseOver);
		cal._hideCombos();
		_C = null;
		return stopEvent(ev);
	}
};

DynCalendar.tableMouseOver = function (ev) {
	var cal = DynCalendar._C;
	if (!cal) {
		return;
	}
	var el = cal.activeDiv;
	var target = DynCalendar.getTargetElement(ev);
	if (target == el || target.parentNode == el) {
		DynCalendar.addClass(el, "hilite active");
		DynCalendar.addClass(el.parentNode, "rowhilite");
	} else {
		if (typeof el.navtype == "undefined" || (el.navtype != 50 && (el.navtype == 0 || Math.abs(el.navtype) > 2)))
			DynCalendar.removeClass(el, "active");
		DynCalendar.removeClass(el, "hilite");
		DynCalendar.removeClass(el.parentNode, "rowhilite");
	}
	ev || (ev = window.event);
	if (el.navtype == 50 && target != el) {
		var pos = DynCalendar.getAbsolutePos(el);
		var w = el.offsetWidth;
		var x = ev.clientX;
		var dx;
		var decrease = true;
		if (x > pos.x + w) {
			dx = x - pos.x - w;
			decrease = false;
		} else
			dx = pos.x - x;

		if (dx < 0) dx = 0;
		var range = el._range;
		var current = el._current;
		var count = Math.floor(dx / 10) % range.length;
		for (var i = range.length; --i >= 0;)
			if (range[i] == current)
				break;
		while (count-- > 0)
			if (decrease) {
				if (--i < 0)
					i = range.length - 1;
			} else if ( ++i >= range.length )
				i = 0;
		var newval = range[i];
		el.innerHTML = newval;

		cal.onUpdateTime();
	}
	var mon = DynCalendar.findMonth(target);
	if (mon) {
		if (mon.month != cal.date.getMonth()) {
			if (cal.hilitedMonth) {
				DynCalendar.removeClass(cal.hilitedMonth, "hilite");
			}
			DynCalendar.addClass(mon, "hilite");
			cal.hilitedMonth = mon;
		} else if (cal.hilitedMonth) {
			DynCalendar.removeClass(cal.hilitedMonth, "hilite");
		}
	} else {
		if (cal.hilitedMonth) {
			DynCalendar.removeClass(cal.hilitedMonth, "hilite");
		}
		var year = DynCalendar.findYear(target);
		if (year) {
			if (year.year != cal.date.getFullYear()) {
				if (cal.hilitedYear) {
					DynCalendar.removeClass(cal.hilitedYear, "hilite");
				}
				DynCalendar.addClass(year, "hilite");
				cal.hilitedYear = year;
			} else if (cal.hilitedYear) {
				DynCalendar.removeClass(cal.hilitedYear, "hilite");
			}
		} else if (cal.hilitedYear) {
			DynCalendar.removeClass(cal.hilitedYear, "hilite");
		}
	}
	return DynCalendar.stopEvent(ev);
};

DynCalendar.tableMouseDown = function (ev) {
	if (DynCalendar.getTargetElement(ev) == DynCalendar.getElement(ev)) {
		return DynCalendar.stopEvent(ev);
	}
};

DynCalendar.calDragIt = function (ev) {
	var cal = DynCalendar._C;
	if (!(cal && cal.dragging)) {
		return false;
	}
	var posX;
	var posY;
	if (DynCalendar.is_ie) {
		posY = window.event.clientY + document.body.scrollTop;
		posX = window.event.clientX + document.body.scrollLeft;
	} else {
		posX = ev.pageX;
		posY = ev.pageY;
	}
	cal.hideShowCovered();
	var st = cal.element.style;
	st.left = (posX - cal.xOffs) + "px";
	st.top = (posY - cal.yOffs) + "px";
	return DynCalendar.stopEvent(ev);
};

DynCalendar.calDragEnd = function (ev) {
	var cal = DynCalendar._C;
	if (!cal) {
		return false;
	}
	cal.dragging = false;
	with (DynCalendar) {
		removeEvent(document, "mousemove", calDragIt);
		removeEvent(document, "mouseup", calDragEnd);
		tableMouseUp(ev);
	}
	cal.hideShowCovered();
};

DynCalendar.dayMouseDown = function(ev) {
	var el = DynCalendar.getElement(ev);
	if (el.disabled) {
		return false;
	}
	var cal = el.calendar;
	cal.activeDiv = el;
	DynCalendar._C = cal;
	if (el.navtype != 300) with (DynCalendar) {
		if (el.navtype == 50) {
			el._current = el.innerHTML;
			addEvent(document, "mousemove", tableMouseOver);
		} else
			addEvent(document, DynCalendar.is_ie5 ? "mousemove" : "mouseover", tableMouseOver);
		addClass(el, "hilite active");
		addEvent(document, "mouseup", tableMouseUp);
	} else if (cal.isPopup) {
		cal._dragStart(ev);
	}
	if (el.navtype == -1 || el.navtype == 1) {
		if (cal.timeout) clearTimeout(cal.timeout);
		cal.timeout = setTimeout("DynCalendar.showMonthsCombo()", 250);
	} else if (el.navtype == -2 || el.navtype == 2) {
		if (cal.timeout) clearTimeout(cal.timeout);
		cal.timeout = setTimeout((el.navtype > 0) ? "DynCalendar.showYearsCombo(true)" : "DynCalendar.showYearsCombo(false)", 250);
	} else {
		cal.timeout = null;
	}
	return DynCalendar.stopEvent(ev);
};

DynCalendar.dayMouseDblClick = function(ev) {
	DynCalendar.cellClick(DynCalendar.getElement(ev), ev || window.event);
	if (DynCalendar.is_ie) {
		document.selection.empty();
	}
};

DynCalendar.dayMouseOver = function(ev) {
	var el = DynCalendar.getElement(ev);
	if (DynCalendar.isRelated(el, ev) || DynCalendar._C || el.disabled) {
		return false;
	}
	if (el.ttip) {
		if (el.ttip.substr(0, 1) == "_") {
			el.ttip = el.caldate.print(el.calendar.ttDateFormat) + el.ttip.substr(1);
		}
		el.calendar.tooltips.innerHTML = el.ttip;
	}
	if (el.navtype != 300) {
		DynCalendar.addClass(el, "hilite");
		if (el.caldate) {
			DynCalendar.addClass(el.parentNode, "rowhilite");
		}
	}
	return DynCalendar.stopEvent(ev);
};

DynCalendar.dayMouseOut = function(ev) {
	with (DynCalendar) {
		var el = getElement(ev);
		if (isRelated(el, ev) || _C || el.disabled)
			return false;
		removeClass(el, "hilite");
		if (el.caldate)
			removeClass(el.parentNode, "rowhilite");
		if (el.calendar)
			el.calendar.tooltips.innerHTML = _TT["SEL_DATE"];
		return stopEvent(ev);
	}
};

/**
 *  A generic "click" handler :) handles all types of buttons defined in this
 *  calendar.
 */
DynCalendar.cellClick = function(el, ev) {
	var cal = el.calendar;
	var closing = false;
	var newdate = false;
	var date = null;
	if (typeof el.navtype == "undefined") {
		if (cal.currentDateEl) {
			DynCalendar.removeClass(cal.currentDateEl, "selected");
			DynCalendar.addClass(el, "selected");
			closing = (cal.currentDateEl == el);
			if (!closing) {
				cal.currentDateEl = el;
			}
		}
		cal.date.setDateOnly(el.caldate);
		date = cal.date;
		var other_month = !(cal.dateClicked = !el.otherMonth);
		if (!other_month && !cal.currentDateEl)
			cal._toggleMultipleDate(new Date(date));
		else
			newdate = !el.disabled;
		// a date was clicked
		if (other_month)
			cal._init(cal.firstDayOfWeek, date);
	} else {
		if (el.navtype == 200) {
			DynCalendar.removeClass(el, "hilite");
			cal.callCloseHandler();
			return;
		}
		date = new Date(cal.date);
		if (el.navtype == 0)
			date.setDateOnly(new Date()); // TODAY
		// unless "today" was clicked, we assume no date was clicked so
		// the selected handler will know not to close the calenar when
		// in single-click mode.
		// cal.dateClicked = (el.navtype == 0);
		cal.dateClicked = false;
		var year = date.getFullYear();
		var mon = date.getMonth();
		function setMonth(m) {
			var day = date.getDate();
			var max = date.getMonthDays(m);
			if (day > max) {
				date.setDate(max);
			}
			date.setMonth(m);
		};
		switch (el.navtype) {
		    case 400:
			DynCalendar.removeClass(el, "hilite");
			var text = DynCalendar._TT["ABOUT"];
			if (typeof text != "undefined") {
				text += cal.showsTime ? DynCalendar._TT["ABOUT_TIME"] : "";
			} else {
				// FIXME: this should be removed as soon as lang files get updated!
				text = "Help and about box text is not translated into this language.\n" +
					"If you know this language and you feel generous please update\n" +
					"the corresponding file in \"lang\" subdir to match calendar-en.js\n" +
					"and send it back to <mihai_bazon@yahoo.com> to get it into the distribution  ;-)\n\n" +
					"Thank you!\n" +
					"http://dynarch.com/mishoo/calendar.epl\n";
			}
			alert(text);
			return;
		    case -2:
			if (year > cal.minYear) {
				date.setFullYear(year - 1);
			}
			break;
		    case -1:
			if (mon > 0) {
				setMonth(mon - 1);
			} else if (year-- > cal.minYear) {
				date.setFullYear(year);
				setMonth(11);
			}
			break;
		    case 1:
			if (mon < 11) {
				setMonth(mon + 1);
			} else if (year < cal.maxYear) {
				date.setFullYear(year + 1);
				setMonth(0);
			}
			break;
		    case 2:
			if (year < cal.maxYear) {
				date.setFullYear(year + 1);
			}
			break;
		    case 100:
			cal.setFirstDayOfWeek(el.fdow);
			return;
		    case 50:
			var range = el._range;
			var current = el.innerHTML;
			for (var i = range.length; --i >= 0;)
				if (range[i] == current)
					break;
			if (ev && ev.shiftKey) {
				if (--i < 0)
					i = range.length - 1;
			} else if ( ++i >= range.length )
				i = 0;
			var newval = range[i];
			el.innerHTML = newval;
			cal.onUpdateTime();
			return;
		    case 0:
			// TODAY will bring us here
			if ((typeof cal.getDateStatus == "function") &&
			    cal.getDateStatus(date, date.getFullYear(), date.getMonth(), date.getDate())) {
				return false;
			}
			break;
		}
		if (!date.equalsTo(cal.date)) {
			cal.setDate(date);
			newdate = true;
		} else if (el.navtype == 0)
			newdate = closing = true;
	}
	if (newdate) {
		ev && cal.callHandler();
	}
	if (closing) {
		DynCalendar.removeClass(el, "hilite");
		ev && cal.callCloseHandler();
	}
};

// END: CALENDAR STATIC FUNCTIONS

// BEGIN: CALENDAR OBJECT FUNCTIONS

/**
 *  This function creates the calendar inside the given parent.  If _par is
 *  null than it creates a popup calendar inside the BODY element.  If _par is
 *  an element, be it BODY, then it creates a non-popup calendar (still
 *  hidden).  Some properties need to be set before calling this function.
 */
DynCalendar.prototype.create = function(_par)
{
    var parent = null;
    if (!_par)
    {
        // default parent is the document body, in which case we create
        // a popup calendar.
        parent = document.getElementsByTagName("body")[0];
        this.isPopup = true;
    } else
    {
        parent = _par;
        this.isPopup = false;
    }
    this.date = this.dateStr ? new Date(this.dateStr) : new Date();

    var table = DynCalendar.createElement("table");
    this.table = table;
    table.cellSpacing = 0;
    table.cellPadding = 0;
    table.calendar = this;
    DynCalendar.addEvent(table, "mousedown", DynCalendar.tableMouseDown);

    var div = DynCalendar.createElement("div");
    this.element = div;
    div.className = "calendar";
    if (this.isPopup)
    {
        div.style.position = "absolute";
        div.style.display = "none";
    }

    var innerDiv = DynCalendar.createElement("div");
    innerDiv.className = "calendar_container";
    this.innerDiv = innerDiv;

    innerDiv.appendChild(table);
    div.appendChild(innerDiv);

    var thead = DynCalendar.createElement("thead", table);
    var cell = null;
    var row = null;

    var cal = this;
    var hh = function(text, cs, navtype)
    {
        cell = DynCalendar.createElement("td", row);
        cell.colSpan = cs;
        cell.className = "button";
        if (navtype != 0 && Math.abs(navtype) <= 2)
            cell.className += " nav";
        DynCalendar._add_evs(cell);
        cell.calendar = cal;
        cell.navtype = navtype;
        cell.innerHTML = "<div unselectable='on'>" + text + "</div>";
        return cell;
    };

    row = DynCalendar.createElement("tr", thead);
    var title_length = 8;
    (this.isPopup) && --title_length;
    (this.weekNumbers) && ++title_length;

    //hh("?", 1, 400).ttip = DynCalendar._TT["INFO"];
    var titleTd = DynCalendar.createElement("td", row);
    titleTd.colSpan = title_length;
    titleTd.className = "titlecell";
    titleTd.innerHTML = "";

    var titleDiv = DynCalendar.createElement("div", titleTd);
    titleDiv.className = "titlecontainer";

    var btnClose = DynCalendar.createElement("a", titleDiv);
    btnClose.className = "closebutton";
    btnClose.innerHTML = "&nbsp";

    $(btnClose).click(function()
    {
        DynCalendar.removeClass(this, "hilite");
        cal.callCloseHandler();
        return;
    });

    $(btnClose).hover(function() { $(this).addClass("hilite"); }, function() { $(this).removeClass("hilite"); });

    var titleSpan = DynCalendar.createElement("span", titleDiv);
    titleSpan.unselectable = 'on';
    titleSpan.calendar = cal;
    titleSpan.navtype = 300;
    DynCalendar._add_evs(titleSpan);

    this.title = titleSpan;
    this.title.className = "title";
    if (this.isPopup)
    {
        titleTd.ttip = DynCalendar._TT["DRAG_TO_MOVE"];
        this.title.ttip = DynCalendar._TT["DRAG_TO_MOVE"];
        this.title.style.cursor = "move";
        //hh("&#x00d7;", 1, 200).ttip = DynCalendar._TT["CLOSE"];
    }

    row = DynCalendar.createElement("tr", thead);
    row.className = "headrow";

    this._nav_py = hh("&#x00ab;", 1, -2);
    this._nav_py.ttip = DynCalendar._TT["PREV_YEAR"];

    this._nav_pm = hh("&#x2039;", 1, -1);
    this._nav_pm.ttip = DynCalendar._TT["PREV_MONTH"];

    this._nav_now = hh(DynCalendar._TT["TODAY"], this.weekNumbers ? 4 : 3, 0);
    this._nav_now.ttip = DynCalendar._TT["GO_TODAY"];

    this._nav_nm = hh("&#x203a;", 1, 1);
    this._nav_nm.ttip = DynCalendar._TT["NEXT_MONTH"];

    this._nav_ny = hh("&#x00bb;", 1, 2);
    this._nav_ny.ttip = DynCalendar._TT["NEXT_YEAR"];

    // day names
    row = DynCalendar.createElement("tr", thead);
    row.className = "daynames";
    if (this.weekNumbers)
    {
        cell = DynCalendar.createElement("td", row);
        cell.className = "name wn";
        cell.innerHTML = DynCalendar._TT["WK"];
    }
    for (var i = 7; i > 0; --i)
    {
        cell = DynCalendar.createElement("td", row);
        if (!i)
        {
            cell.navtype = 100;
            cell.calendar = this;
            DynCalendar._add_evs(cell);
        }
    }
    this.firstdayname = (this.weekNumbers) ? row.firstChild.nextSibling : row.firstChild;
    this._displayWeekdays();

    var tbody = DynCalendar.createElement("tbody", table);
    this.tbody = tbody;

    for (i = 6; i > 0; --i)
    {
        row = DynCalendar.createElement("tr", tbody);
        if (this.weekNumbers)
        {
            cell = DynCalendar.createElement("td", row);
        }
        for (var j = 7; j > 0; --j)
        {
            cell = DynCalendar.createElement("td", row);
            cell.calendar = this;
            DynCalendar._add_evs(cell);
        }
    }

    if (this.showsTime)
    {
        row = DynCalendar.createElement("tr", tbody);
        row.className = "time";

        cell = DynCalendar.createElement("td", row);
        cell.className = "time";
        cell.colSpan = 2;
        cell.innerHTML = DynCalendar._TT["TIME"] || "&nbsp;";

        cell = DynCalendar.createElement("td", row);
        cell.className = "time";
        cell.colSpan = this.weekNumbers ? 4 : 3;

        (function()
        {
            function makeTimePart(className, init, range_start, range_end)
            {
                var part = DynCalendar.createElement("span", cell);
                part.className = className;
                part.innerHTML = init;
                part.calendar = cal;
                part.ttip = DynCalendar._TT["TIME_PART"];
                part.navtype = 50;
                part._range = [];
                if (typeof range_start != "number")
                    part._range = range_start;
                else
                {
                    for (var i = range_start; i <= range_end; ++i)
                    {
                        var txt;
                        if (i < 10 && range_end >= 10) txt = '0' + i;
                        else txt = '' + i;
                        part._range[part._range.length] = txt;
                    }
                }
                DynCalendar._add_evs(part);
                return part;
            };
            var hrs = cal.date.getHours();
            var mins = cal.date.getMinutes();
            var t12 = !cal.time24;
            var pm = (hrs > 12);
            if (t12 && pm) hrs -= 12;
            var H = makeTimePart("hour", hrs, t12 ? 1 : 0, t12 ? 12 : 23);
            var span = DynCalendar.createElement("span", cell);
            span.innerHTML = ":";
            span.className = "colon";
            var M = makeTimePart("minute", mins, 0, 59);
            var AP = null;
            cell = DynCalendar.createElement("td", row);
            cell.className = "time";
            cell.colSpan = 2;
            if (t12)
                AP = makeTimePart("ampm", pm ? "pm" : "am", ["am", "pm"]);
            else
                cell.innerHTML = "&nbsp;";

            cal.onSetTime = function()
            {
                var pm, hrs = this.date.getHours(),
					mins = this.date.getMinutes();
                if (t12)
                {
                    pm = (hrs >= 12);
                    if (pm) hrs -= 12;
                    if (hrs == 0) hrs = 12;
                    AP.innerHTML = pm ? "pm" : "am";
                }
                H.innerHTML = (hrs < 10) ? ("0" + hrs) : hrs;
                M.innerHTML = (mins < 10) ? ("0" + mins) : mins;
            };

            cal.onUpdateTime = function()
            {
                var date = this.date;
                var h = parseInt(H.innerHTML, 10);
                if (t12)
                {
                    if (/pm/i.test(AP.innerHTML) && h < 12)
                        h += 12;
                    else if (/am/i.test(AP.innerHTML) && h == 12)
                        h = 0;
                }
                var d = date.getDate();
                var m = date.getMonth();
                var y = date.getFullYear();
                date.setHours(h);
                date.setMinutes(parseInt(M.innerHTML, 10));
                date.setFullYear(y);
                date.setMonth(m);
                date.setDate(d);
                this.dateClicked = false;
                this.callHandler();
            };
        })();
    } else
    {
        this.onSetTime = this.onUpdateTime = function() { };
    }

    var tfoot = DynCalendar.createElement("tfoot", table);

    row = DynCalendar.createElement("tr", tfoot);
    row.className = "footrow";

    cell = hh(DynCalendar._TT["SEL_DATE"], this.weekNumbers ? 8 : 7, 300);
    cell.className = "ttip";
    if (this.isPopup)
    {
        cell.ttip = DynCalendar._TT["DRAG_TO_MOVE"];
        cell.style.cursor = "move";
    }
    this.tooltips = cell;

    div = DynCalendar.createElement("div", this.element);
    this.monthsCombo = div;
    div.className = "combo";
    for (i = 0; i < DynCalendar._MN.length; ++i)
    {
        var mn = DynCalendar.createElement("div");
        mn.className = DynCalendar.is_ie ? "label-IEfix" : "label";
        mn.month = i;
        mn.innerHTML = DynCalendar._SMN[i];
        div.appendChild(mn);
    }

    div = DynCalendar.createElement("div", this.element);
    this.yearsCombo = div;
    div.className = "combo";
    for (i = 12; i > 0; --i)
    {
        var yr = DynCalendar.createElement("div");
        yr.className = DynCalendar.is_ie ? "label-IEfix" : "label";
        div.appendChild(yr);
    }

    this._init(this.firstDayOfWeek, this.date);
    parent.appendChild(this.element);
}; 

/** keyboard navigation, only for popup calendars */
DynCalendar._keyEvent = function(ev) {
	var cal = window._dynarch_popupCalendar;
	if (!cal || cal.multiple)
		return false;
	(DynCalendar.is_ie) && (ev = window.event);
	var act = (DynCalendar.is_ie || ev.type == "keypress"),
		K = ev.keyCode;
	if (ev.ctrlKey) {
		switch (K) {
		    case 37: // KEY left
			act && DynCalendar.cellClick(cal._nav_pm);
			break;
		    case 38: // KEY up
			act && DynCalendar.cellClick(cal._nav_py);
			break;
		    case 39: // KEY right
			act && DynCalendar.cellClick(cal._nav_nm);
			break;
		    case 40: // KEY down
			act && DynCalendar.cellClick(cal._nav_ny);
			break;
		    default:
			return false;
		}
	} else switch (K) {
	    case 32: // KEY space (now)
		DynCalendar.cellClick(cal._nav_now);
		break;
	    case 27: // KEY esc
		act && cal.callCloseHandler();
		break;
	    case 37: // KEY left
	    case 38: // KEY up
	    case 39: // KEY right
	    case 40: // KEY down
		if (act) {
			var prev, x, y, ne, el, step;
			prev = K == 37 || K == 38;
			step = (K == 37 || K == 39) ? 1 : 7;
			function setVars() {
				el = cal.currentDateEl;
				var p = el.pos;
				x = p & 15;
				y = p >> 4;
				ne = cal.ar_days[y][x];
			};setVars();
			function prevMonth() {
				var date = new Date(cal.date);
				date.setDate(date.getDate() - step);
				cal.setDate(date);
			};
			function nextMonth() {
				var date = new Date(cal.date);
				date.setDate(date.getDate() + step);
				cal.setDate(date);
			};
			while (1) {
				switch (K) {
				    case 37: // KEY left
					if (--x >= 0)
						ne = cal.ar_days[y][x];
					else {
						x = 6;
						K = 38;
						continue;
					}
					break;
				    case 38: // KEY up
					if (--y >= 0)
						ne = cal.ar_days[y][x];
					else {
						prevMonth();
						setVars();
					}
					break;
				    case 39: // KEY right
					if (++x < 7)
						ne = cal.ar_days[y][x];
					else {
						x = 0;
						K = 40;
						continue;
					}
					break;
				    case 40: // KEY down
					if (++y < cal.ar_days.length)
						ne = cal.ar_days[y][x];
					else {
						nextMonth();
						setVars();
					}
					break;
				}
				break;
			}
			if (ne) {
				if (!ne.disabled)
					DynCalendar.cellClick(ne);
				else if (prev)
					prevMonth();
				else
					nextMonth();
			}
		}
		break;
	    case 13: // KEY enter
		if (act)
			DynCalendar.cellClick(cal.currentDateEl, ev);
		break;
	    default:
		return false;
	}
	return DynCalendar.stopEvent(ev);
};

/**
 *  (RE)Initializes the calendar to the given date and firstDayOfWeek
 */
DynCalendar.prototype._init = function (firstDayOfWeek, date) {
	var today = new Date(),
		TY = today.getFullYear(),
		TM = today.getMonth(),
		TD = today.getDate();
	this.table.style.visibility = "hidden";
	var year = date.getFullYear();
	if (year < this.minYear) {
		year = this.minYear;
		date.setFullYear(year);
	} else if (year > this.maxYear) {
		year = this.maxYear;
		date.setFullYear(year);
	}
	this.firstDayOfWeek = firstDayOfWeek;
	this.date = new Date(date);
	var month = date.getMonth();
	var mday = date.getDate();
	var no_days = date.getMonthDays();

	// calendar voodoo for computing the first day that would actually be
	// displayed in the calendar, even if it's from the previous month.
	// WARNING: this is magic. ;-)
	date.setDate(1);
	var day1 = (date.getDay() - this.firstDayOfWeek) % 7;
	if (day1 < 0)
		day1 += 7;
	date.setDate(-day1);
	date.setDate(date.getDate() + 1);

	var row = this.tbody.firstChild;
	var MN = DynCalendar._SMN[month];
	var ar_days = this.ar_days = new Array();
	var weekend = DynCalendar._TT["WEEKEND"];
	var dates = this.multiple ? (this.datesCells = {}) : null;
	for (var i = 0; i < 6; ++i, row = row.nextSibling) {
		var cell = row.firstChild;
		if (this.weekNumbers) {
			cell.className = "day wn";
			cell.innerHTML = date.getWeekNumber();
			cell = cell.nextSibling;
		}
		row.className = "daysrow";
		var hasdays = false, iday, dpos = ar_days[i] = [];
		for (var j = 0; j < 7; ++j, cell = cell.nextSibling, date.setDate(iday + 1)) {
			iday = date.getDate();
			var wday = date.getDay();
			cell.className = "day";
			cell.pos = i << 4 | j;
			dpos[j] = cell;
			var current_month = (date.getMonth() == month);
			if (!current_month) {
				if (this.showsOtherMonths) {
					cell.className += " othermonth";
					cell.otherMonth = true;
				} else {
					cell.className = "emptycell";
					cell.innerHTML = "&nbsp;";
					cell.disabled = true;
					continue;
				}
			} else {
				cell.otherMonth = false;
				hasdays = true;
			}
			cell.disabled = false;
			cell.innerHTML = this.getDateText ? this.getDateText(date, iday) : iday;
			if (dates)
				dates[date.print("%Y%m%d")] = cell;
			if (this.getDateStatus) {
				var status = this.getDateStatus(date, year, month, iday);
				if (this.getDateToolTip) {
					var toolTip = this.getDateToolTip(date, year, month, iday);
					if (toolTip)
						cell.title = toolTip;
				}
				if (status === true) {
					cell.className += " disabled";
					cell.disabled = true;
				} else {
					if (/disabled/i.test(status))
						cell.disabled = true;
					cell.className += " " + status;
				}
			}
			if (!cell.disabled) {
				cell.caldate = new Date(date);
				cell.ttip = "_";
				if (!this.multiple && current_month
				    && iday == mday && this.hiliteToday) {
					cell.className += " selected";
					this.currentDateEl = cell;
				}
				if (date.getFullYear() == TY &&
				    date.getMonth() == TM &&
				    iday == TD) {
					cell.className += " today";
					cell.ttip += DynCalendar._TT["PART_TODAY"];
				}
				if (weekend.indexOf(wday.toString()) != -1)
					cell.className += cell.otherMonth ? " oweekend" : " weekend";
			}
		}
		if (!(hasdays || this.showsOtherMonths))
			row.className = "emptyrow";
	}
	this.title.innerHTML = DynCalendar._MN[month] + ", " + year;
	this.onSetTime();
	this.table.style.visibility = "visible";
	this._initMultipleDates();
	// PROFILE
	// this.tooltips.innerHTML = "Generated in " + ((new Date()) - today) + " ms";
};

DynCalendar.prototype._initMultipleDates = function() {
	if (this.multiple) {
		for (var i in this.multiple) {
			var cell = this.datesCells[i];
			var d = this.multiple[i];
			if (!d)
				continue;
			if (cell)
				cell.className += " selected";
		}
	}
};

DynCalendar.prototype._toggleMultipleDate = function(date) {
	if (this.multiple) {
		var ds = date.print("%Y%m%d");
		var cell = this.datesCells[ds];
		if (cell) {
			var d = this.multiple[ds];
			if (!d) {
				DynCalendar.addClass(cell, "selected");
				this.multiple[ds] = date;
			} else {
				DynCalendar.removeClass(cell, "selected");
				delete this.multiple[ds];
			}
		}
	}
};

DynCalendar.prototype.setDateToolTipHandler = function (unaryFunction) {
	this.getDateToolTip = unaryFunction;
};

/**
 *  Calls _init function above for going to a certain date (but only if the
 *  date is different than the currently selected one).
 */
DynCalendar.prototype.setDate = function (date) {
	if (!date.equalsTo(this.date)) {
		this._init(this.firstDayOfWeek, date);
	}
};

/**
 *  Refreshes the calendar.  Useful if the "disabledHandler" function is
 *  dynamic, meaning that the list of disabled date can change at runtime.
 *  Just * call this function if you think that the list of disabled dates
 *  should * change.
 */
DynCalendar.prototype.refresh = function () {
	this._init(this.firstDayOfWeek, this.date);
};

/** Modifies the "firstDayOfWeek" parameter (pass 0 for Synday, 1 for Monday, etc.). */
DynCalendar.prototype.setFirstDayOfWeek = function (firstDayOfWeek) {
	this._init(firstDayOfWeek, this.date);
	this._displayWeekdays();
};

/**
 *  Allows customization of what dates are enabled.  The "unaryFunction"
 *  parameter must be a function object that receives the date (as a JS Date
 *  object) and returns a boolean value.  If the returned value is true then
 *  the passed date will be marked as disabled.
 */
DynCalendar.prototype.setDateStatusHandler = DynCalendar.prototype.setDisabledHandler = function (unaryFunction) {
	this.getDateStatus = unaryFunction;
};

/** Customization of allowed year range for the calendar. */
DynCalendar.prototype.setRange = function (a, z) {
	this.minYear = a;
	this.maxYear = z;
};

/** Calls the first user handler (selectedHandler). */
DynCalendar.prototype.callHandler = function () {
	if (this.onSelected) {
		this.onSelected(this, this.date.print(this.dateFormat));
	}
};

/** Calls the second user handler (closeHandler). */
DynCalendar.prototype.callCloseHandler = function () {
	if (this.onClose) {
		this.onClose(this);
	}
	this.hideShowCovered();
};

/** Removes the calendar object from the DOM tree and destroys it. */
DynCalendar.prototype.destroy = function () {
	var el = this.element.parentNode;
	el.removeChild(this.element);
	DynCalendar._C = null;
	window._dynarch_popupCalendar = null;
};

/**
 *  Moves the calendar element to a different section in the DOM tree (changes
 *  its parent).
 */
DynCalendar.prototype.reparent = function (new_parent) {
	var el = this.element;
	el.parentNode.removeChild(el);
	new_parent.appendChild(el);
};

// This gets called when the user presses a mouse button anywhere in the
// document, if the calendar is shown.  If the click was outside the open
// calendar this function closes it.
DynCalendar._checkCalendar = function(ev) {
	var calendar = window._dynarch_popupCalendar;
	if (!calendar) {
		return false;
	}
	var el = DynCalendar.is_ie ? DynCalendar.getElement(ev) : DynCalendar.getTargetElement(ev);
	for (; el != null && el != calendar.element; el = el.parentNode);
	if (el == null) {
		// calls closeHandler which should hide the calendar.
		window._dynarch_popupCalendar.callCloseHandler();
		return DynCalendar.stopEvent(ev);
	}
};

/** Shows the calendar. */
DynCalendar.prototype.show = function () {
	var rows = this.table.getElementsByTagName("tr");
	for (var i = rows.length; i > 0;) {
		var row = rows[--i];
		DynCalendar.removeClass(row, "rowhilite");
		var cells = row.getElementsByTagName("td");
		for (var j = cells.length; j > 0;) {
			var cell = cells[--j];
			DynCalendar.removeClass(cell, "hilite");
			DynCalendar.removeClass(cell, "active");
		}
	}
	this.element.style.display = "block";
	this.hidden = false;
	if (this.isPopup) {
		window._dynarch_popupCalendar = this;
		DynCalendar.addEvent(document, "keydown", DynCalendar._keyEvent);
		DynCalendar.addEvent(document, "keypress", DynCalendar._keyEvent);
		DynCalendar.addEvent(document, "mousedown", DynCalendar._checkCalendar);
	}
	this.hideShowCovered();
};

/**
 *  Hides the calendar.  Also removes any "hilite" from the class of any TD
 *  element.
 */
DynCalendar.prototype.hide = function () {
	if (this.isPopup) {
		DynCalendar.removeEvent(document, "keydown", DynCalendar._keyEvent);
		DynCalendar.removeEvent(document, "keypress", DynCalendar._keyEvent);
		DynCalendar.removeEvent(document, "mousedown", DynCalendar._checkCalendar);
	}
	this.element.style.display = "none";
	this.hidden = true;
	this.hideShowCovered();
};

/**
 *  Shows the calendar at a given absolute position (beware that, depending on
 *  the calendar element style -- position property -- this might be relative
 *  to the parent's containing rectangle).
 */
DynCalendar.prototype.showAt = function (x, y) {
	var s = this.element.style;
	s.left = x + "px";
	s.top = y + "px";
	this.show();
};

/** Shows the calendar near a given element. */
DynCalendar.prototype.showAtElement = function (el, opts) {
	var self = this;
	var p = DynCalendar.getAbsolutePos(el);
	if (!opts || typeof opts != "string") {
		this.showAt(p.x, p.y + el.offsetHeight);
		return true;
	}
	function fixPosition(box) {
		if (box.x < 0)
			box.x = 0;
		if (box.y < 0)
			box.y = 0;
		var cp = document.createElement("div");
		var s = cp.style;
		s.position = "absolute";
		s.right = s.bottom = s.width = s.height = "0px";
		document.body.appendChild(cp);
		var br = DynCalendar.getAbsolutePos(cp);
		document.body.removeChild(cp);
		if (DynCalendar.is_ie) {
			br.y += document.body.scrollTop;
			br.x += document.body.scrollLeft;
		} else {
			br.y += window.scrollY;
			br.x += window.scrollX;
		}
		var tmp = box.x + box.width - br.x;
		if (tmp > 0) box.x -= tmp;
		tmp = box.y + box.height - br.y;
		if (tmp > 0) box.y -= tmp;
	};
	this.element.style.display = "block";
	DynCalendar.continuation_for_the_fucking_khtml_browser = function() {
		var w = self.element.offsetWidth;
		var h = self.element.offsetHeight;
		self.element.style.display = "none";
		var valign = opts.substr(0, 1);
		var halign = "l";
		if (opts.length > 1) {
			halign = opts.substr(1, 1);
		}
		// vertical alignment
		switch (valign) {
		    case "T": p.y -= h; break;
		    case "B": p.y += el.offsetHeight; break;
		    case "C": p.y += (el.offsetHeight - h) / 2; break;
		    case "t": p.y += el.offsetHeight - h; break;
		    case "b": break; // already there
		}
		// horizontal alignment
		switch (halign) {
		    case "L": p.x -= w; break;
		    case "R": p.x += el.offsetWidth; break;
		    case "C": p.x += (el.offsetWidth - w) / 2; break;
		    case "l": p.x += el.offsetWidth - w; break;
		    case "r": break; // already there
		}
		p.width = w;
		p.height = h + 40;
		self.monthsCombo.style.display = "none";
		fixPosition(p);
		self.showAt(p.x, p.y);
	};
	if (DynCalendar.is_khtml)
		setTimeout("DynCalendar.continuation_for_the_fucking_khtml_browser()", 10);
	else
		DynCalendar.continuation_for_the_fucking_khtml_browser();
};

/** Customizes the date format. */
DynCalendar.prototype.setDateFormat = function (str) {
	this.dateFormat = str;
};

/** Customizes the tooltip date format. */
DynCalendar.prototype.setTtDateFormat = function (str) {
	this.ttDateFormat = str;
};

/**
 *  Tries to identify the date represented in a string.  If successful it also
 *  calls this.setDate which moves the calendar to the given date.
 */
DynCalendar.prototype.parseDate = function(str, fmt) {
	if (!fmt)
		fmt = this.dateFormat;
	this.setDate(Date.parseDate(str, fmt));
};

DynCalendar.prototype.hideShowCovered = function () {
	if (!DynCalendar.is_ie && !DynCalendar.is_opera)
		return;
	function getVisib(obj){
		var value = obj.style.visibility;
		if (!value) {
			if (document.defaultView && typeof (document.defaultView.getComputedStyle) == "function") { // Gecko, W3C
				if (!DynCalendar.is_khtml)
					value = document.defaultView.
						getComputedStyle(obj, "").getPropertyValue("visibility");
				else
					value = '';
			} else if (obj.currentStyle) { // IE
				value = obj.currentStyle.visibility;
			} else
				value = '';
		}
		return value;
	};

	var tags = new Array("applet", "iframe", "select");
	var el = this.element;

	var p = DynCalendar.getAbsolutePos(el);
	var EX1 = p.x;
	var EX2 = el.offsetWidth + EX1;
	var EY1 = p.y;
	var EY2 = el.offsetHeight + EY1;

	for (var k = tags.length; k > 0; ) {
		var ar = document.getElementsByTagName(tags[--k]);
		var cc = null;

		for (var i = ar.length; i > 0;) {
			cc = ar[--i];

			p = DynCalendar.getAbsolutePos(cc);
			var CX1 = p.x;
			var CX2 = cc.offsetWidth + CX1;
			var CY1 = p.y;
			var CY2 = cc.offsetHeight + CY1;

			if (this.hidden || (CX1 > EX2) || (CX2 < EX1) || (CY1 > EY2) || (CY2 < EY1)) {
				if (!cc.__msh_save_visibility) {
					cc.__msh_save_visibility = getVisib(cc);
				}
				cc.style.visibility = cc.__msh_save_visibility;
			} else {
				if (!cc.__msh_save_visibility) {
					cc.__msh_save_visibility = getVisib(cc);
				}
				cc.style.visibility = "hidden";
			}
		}
	}
};

/** Internal function; it displays the bar with the names of the weekday. */
DynCalendar.prototype._displayWeekdays = function () {
	var fdow = this.firstDayOfWeek;
	var cell = this.firstdayname;
	var weekend = DynCalendar._TT["WEEKEND"];
	for (var i = 0; i < 7; ++i) {
		cell.className = "day name";
		var realday = (i + fdow) % 7;
		if (i) {
			cell.ttip = DynCalendar._TT["DAY_FIRST"].replace("%s", DynCalendar._DN[realday]);
			cell.navtype = 100;
			cell.calendar = this;
			cell.fdow = realday;
			DynCalendar._add_evs(cell);
		}
		if (weekend.indexOf(realday.toString()) != -1) {
			DynCalendar.addClass(cell, "weekend");
		}
		cell.innerHTML = DynCalendar._SDN[(i + fdow) % 7];
		cell = cell.nextSibling;
	}
};

/** Internal function.  Hides all combo boxes that might be displayed. */
DynCalendar.prototype._hideCombos = function () {
	this.monthsCombo.style.display = "none";
	this.yearsCombo.style.display = "none";
};

/** Internal function.  Starts dragging the element. */
DynCalendar.prototype._dragStart = function (ev) {
	if (this.dragging) {
		return;
	}
	this.dragging = true;
	var posX;
	var posY;
	if (DynCalendar.is_ie) {
		posY = window.event.clientY + document.body.scrollTop;
		posX = window.event.clientX + document.body.scrollLeft;
	} else {
		posY = ev.clientY + window.scrollY;
		posX = ev.clientX + window.scrollX;
	}
	var st = this.element.style;
	this.xOffs = posX - parseInt(st.left);
	this.yOffs = posY - parseInt(st.top);
	with (DynCalendar) {
		addEvent(document, "mousemove", calDragIt);
		addEvent(document, "mouseup", calDragEnd);
	}
};

// BEGIN: DATE OBJECT PATCHES

/** Adds the number of days array to the Date object. */
Date._MD = new Array(31,28,31,30,31,30,31,31,30,31,30,31);

/** Constants used for time computations */
Date.SECOND = 1000 /* milliseconds */;
Date.MINUTE = 60 * Date.SECOND;
Date.HOUR   = 60 * Date.MINUTE;
Date.DAY    = 24 * Date.HOUR;
Date.WEEK   =  7 * Date.DAY;

Date.parseDate = function(str, fmt) {
	var today = new Date();
	var y = 0;
	var m = -1;
	var d = 0;
	var a = str.split(/\W+/);
	var b = fmt.match(/%./g);
	var i = 0, j = 0;
	var hr = 0;
	var min = 0;
	for (i = 0; i < a.length; ++i) {
		if (!a[i])
			continue;
		switch (b[i]) {
		    case "%d":
		    case "%e":
			d = parseInt(a[i], 10);
			break;

		    case "%m":
			m = parseInt(a[i], 10) - 1;
			break;

		    case "%Y":
		    case "%y":
			y = parseInt(a[i], 10);
			(y < 100) && (y += (y > 29) ? 1900 : 2000);
			break;

		    case "%b":
		    case "%B":
			for (j = 0; j < 12; ++j) {
				if (DynCalendar._MN[j].substr(0, a[i].length).toLowerCase() == a[i].toLowerCase()) { m = j; break; }
			}
			break;

		    case "%H":
		    case "%I":
		    case "%k":
		    case "%l":
			hr = parseInt(a[i], 10);
			break;

		    case "%P":
		    case "%p":
			if (/pm/i.test(a[i]) && hr < 12)
				hr += 12;
			else if (/am/i.test(a[i]) && hr >= 12)
				hr -= 12;
			break;

		    case "%M":
			min = parseInt(a[i], 10);
			break;
		}
	}
	if (isNaN(y)) y = today.getFullYear();
	if (isNaN(m)) m = today.getMonth();
	if (isNaN(d)) d = today.getDate();
	if (isNaN(hr)) hr = today.getHours();
	if (isNaN(min)) min = today.getMinutes();
	if (y != 0 && m != -1 && d != 0)
		return new Date(y, m, d, hr, min, 0);
	y = 0; m = -1; d = 0;
	for (i = 0; i < a.length; ++i) {
		if (a[i].search(/[a-zA-Z]+/) != -1) {
			var t = -1;
			for (j = 0; j < 12; ++j) {
				if (DynCalendar._MN[j].substr(0, a[i].length).toLowerCase() == a[i].toLowerCase()) { t = j; break; }
			}
			if (t != -1) {
				if (m != -1) {
					d = m+1;
				}
				m = t;
			}
		} else if (parseInt(a[i], 10) <= 12 && m == -1) {
			m = a[i]-1;
		} else if (parseInt(a[i], 10) > 31 && y == 0) {
			y = parseInt(a[i], 10);
			(y < 100) && (y += (y > 29) ? 1900 : 2000);
		} else if (d == 0) {
			d = a[i];
		}
	}
	if (y == 0)
		y = today.getFullYear();
	if (m != -1 && d != 0)
		return new Date(y, m, d, hr, min, 0);
	return today;
};

/** Returns the number of days in the current month */
Date.prototype.getMonthDays = function(month) {
	var year = this.getFullYear();
	if (typeof month == "undefined") {
		month = this.getMonth();
	}
	if (((0 == (year%4)) && ( (0 != (year%100)) || (0 == (year%400)))) && month == 1) {
		return 29;
	} else {
		return Date._MD[month];
	}
};

/** Returns the number of day in the year. */
Date.prototype.getDayOfYear = function() {
	var now = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0, 0, 0);
	var then = new Date(this.getFullYear(), 0, 0, 0, 0, 0);
	var time = now - then;
	return Math.floor(time / Date.DAY);
};

/** Returns the number of the week in year, as defined in ISO 8601. */
Date.prototype.getWeekNumber = function() {
	var d = new Date(this.getFullYear(), this.getMonth(), this.getDate(), 0, 0, 0);
	var DoW = d.getDay();
	d.setDate(d.getDate() - (DoW + 6) % 7 + 3); // Nearest Thu
	var ms = d.valueOf(); // GMT
	d.setMonth(0);
	d.setDate(4); // Thu in Week 1
	return Math.round((ms - d.valueOf()) / (7 * 864e5)) + 1;
};

/** Checks date and time equality */
Date.prototype.equalsTo = function(date) {
	return ((this.getFullYear() == date.getFullYear()) &&
		(this.getMonth() == date.getMonth()) &&
		(this.getDate() == date.getDate()) &&
		(this.getHours() == date.getHours()) &&
		(this.getMinutes() == date.getMinutes()));
};

/** Set only the year, month, date parts (keep existing time) */
Date.prototype.setDateOnly = function(date) {
	var tmp = new Date(date);
	this.setDate(1);
	this.setFullYear(tmp.getFullYear());
	this.setMonth(tmp.getMonth());
	this.setDate(tmp.getDate());
};

/** Prints the date in a string according to the given format. */
Date.prototype.print = function (str) {
	var m = this.getMonth();
	var d = this.getDate();
	var y = this.getFullYear();
	var wn = this.getWeekNumber();
	var w = this.getDay();
	var s = {};
	var hr = this.getHours();
	var pm = (hr >= 12);
	var ir = (pm) ? (hr - 12) : hr;
	var dy = this.getDayOfYear();
	if (ir == 0)
		ir = 12;
	var min = this.getMinutes();
	var sec = this.getSeconds();
	s["%a"] = DynCalendar._SDN[w]; // abbreviated weekday name [FIXME: I18N]
	s["%A"] = DynCalendar._DN[w]; // full weekday name
	s["%b"] = DynCalendar._SMN[m]; // abbreviated month name [FIXME: I18N]
	s["%B"] = DynCalendar._MN[m]; // full month name
	// FIXME: %c : preferred date and time representation for the current locale
	s["%C"] = 1 + Math.floor(y / 100); // the century number
	s["%d"] = (d < 10) ? ("0" + d) : d; // the day of the month (range 01 to 31)
	s["%e"] = d; // the day of the month (range 1 to 31)
	// FIXME: %D : american date style: %m/%d/%y
	// FIXME: %E, %F, %G, %g, %h (man strftime)
	s["%H"] = (hr < 10) ? ("0" + hr) : hr; // hour, range 00 to 23 (24h format)
	s["%I"] = (ir < 10) ? ("0" + ir) : ir; // hour, range 01 to 12 (12h format)
	s["%j"] = (dy < 100) ? ((dy < 10) ? ("00" + dy) : ("0" + dy)) : dy; // day of the year (range 001 to 366)
	s["%k"] = hr;		// hour, range 0 to 23 (24h format)
	s["%l"] = ir;		// hour, range 1 to 12 (12h format)
	s["%m"] = (m < 9) ? ("0" + (1+m)) : (1+m); // month, range 01 to 12
	s["%M"] = (min < 10) ? ("0" + min) : min; // minute, range 00 to 59
	s["%n"] = "\n";		// a newline character
	s["%p"] = pm ? "PM" : "AM";
	s["%P"] = pm ? "pm" : "am";
	// FIXME: %r : the time in am/pm notation %I:%M:%S %p
	// FIXME: %R : the time in 24-hour notation %H:%M
	s["%s"] = Math.floor(this.getTime() / 1000);
	s["%S"] = (sec < 10) ? ("0" + sec) : sec; // seconds, range 00 to 59
	s["%t"] = "\t";		// a tab character
	// FIXME: %T : the time in 24-hour notation (%H:%M:%S)
	s["%U"] = s["%W"] = s["%V"] = (wn < 10) ? ("0" + wn) : wn;
	s["%u"] = w + 1;	// the day of the week (range 1 to 7, 1 = MON)
	s["%w"] = w;		// the day of the week (range 0 to 6, 0 = SUN)
	// FIXME: %x : preferred date representation for the current locale without the time
	// FIXME: %X : preferred time representation for the current locale without the date
	s["%y"] = ('' + y).substr(2, 2); // year without the century (range 00 to 99)
	s["%Y"] = y;		// year with the century
	s["%%"] = "%";		// a literal '%' character

	var re = /%./g;
	if (!DynCalendar.is_ie5 && !DynCalendar.is_khtml)
		return str.replace(re, function (par) { return s[par] || par; });

	var a = str.match(re);
	for (var i = 0; i < a.length; i++) {
		var tmp = s[a[i]];
		if (tmp) {
			re = new RegExp(a[i], 'g');
			str = str.replace(re, tmp);
		}
	}

	return str;
};

Date.prototype.__msh_oldSetFullYear = Date.prototype.setFullYear;
Date.prototype.setFullYear = function(y) {
	var d = new Date(this);
	d.__msh_oldSetFullYear(y);
	if (d.getMonth() != this.getMonth())
		this.setDate(28);
	this.__msh_oldSetFullYear(y);
};

// END: DATE OBJECT PATCHES


// global object that remembers the calendar
window._dynarch_popupCalendar = null;

DynCalendar._DN = new Array
    ("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"); DynCalendar._SDN = new Array
    ("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"); DynCalendar._FD = 0; DynCalendar._MN = new Array
    ("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"); DynCalendar._SMN = new Array
    ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");
DynCalendar._TT = {};
DynCalendar._TT["INFO"] = "About the calendar";
DynCalendar._TT["ABOUT"] = "DHTML Date/Time Selector\n" + "(c) dynarch.com 2002-2005 / Author: Mihai Bazon\n" + "For latest version visit: http://www.dynarch.com/projects/calendar/\n" + "Distributed under GNU LGPL.  See http://gnu.org/licenses/lgpl.html for details." + "\n\n" + "Date selection:\n" + "- Use the \xab, \xbb buttons to select year\n" + "- Use the " + String.fromCharCode(0x2039) + ", " + String.fromCharCode(0x203a) + " buttons to select month\n" + "- Hold mouse button on any of the above buttons for faster selection.";
DynCalendar._TT["ABOUT_TIME"] = "\n\n" + "Time selection:\n" + "- Click on any of the time parts to increase it\n" + "- or Shift-click to decrease it\n" + "- or click and drag for faster selection.";
DynCalendar._TT["PREV_YEAR"] = "Prev. year (hold for menu)";
DynCalendar._TT["PREV_MONTH"] = "Prev. month (hold for menu)";
DynCalendar._TT["GO_TODAY"] = "Go Today";
DynCalendar._TT["NEXT_MONTH"] = "Next month (hold for menu)";
DynCalendar._TT["NEXT_YEAR"] = "Next year (hold for menu)";
DynCalendar._TT["SEL_DATE"] = "Select date";
DynCalendar._TT["DRAG_TO_MOVE"] = "Drag to move";
DynCalendar._TT["PART_TODAY"] = " (today)";
DynCalendar._TT["DAY_FIRST"] = "Display %s first";
DynCalendar._TT["WEEKEND"] = "0,6";
DynCalendar._TT["CLOSE"] = "Close";
DynCalendar._TT["TODAY"] = "Today";
DynCalendar._TT["TIME_PART"] = "(Shift-)Click or drag to change value";
DynCalendar._TT["DEF_DATE_FORMAT"] = "%d";
DynCalendar._TT["TT_DATE_FORMAT"] = "%d";
DynCalendar._TT["WK"] = "wn";
DynCalendar._TT["TIME"] = "Time:";
/*!
 * jQuery UI 1.8.1
 *
 * Copyright (c) 2010 AUTHORS.txt (http://jqueryui.com/about)
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 *
 * http://docs.jquery.com/UI
 */
jQuery.ui||function(c){c.ui={version:"1.8.1",plugin:{add:function(a,b,d){a=c.ui[a].prototype;for(var e in d){a.plugins[e]=a.plugins[e]||[];a.plugins[e].push([b,d[e]])}},call:function(a,b,d){if((b=a.plugins[b])&&a.element[0].parentNode)for(var e=0;e<b.length;e++)a.options[b[e][0]]&&b[e][1].apply(a.element,d)}},contains:function(a,b){return document.compareDocumentPosition?a.compareDocumentPosition(b)&16:a!==b&&a.contains(b)},hasScroll:function(a,b){if(c(a).css("overflow")=="hidden")return false;
b=b&&b=="left"?"scrollLeft":"scrollTop";var d=false;if(a[b]>0)return true;a[b]=1;d=a[b]>0;a[b]=0;return d},isOverAxis:function(a,b,d){return a>b&&a<b+d},isOver:function(a,b,d,e,f,g){return c.ui.isOverAxis(a,d,f)&&c.ui.isOverAxis(b,e,g)},keyCode:{ALT:18,BACKSPACE:8,CAPS_LOCK:20,COMMA:188,CONTROL:17,DELETE:46,DOWN:40,END:35,ENTER:13,ESCAPE:27,HOME:36,INSERT:45,LEFT:37,NUMPAD_ADD:107,NUMPAD_DECIMAL:110,NUMPAD_DIVIDE:111,NUMPAD_ENTER:108,NUMPAD_MULTIPLY:106,NUMPAD_SUBTRACT:109,PAGE_DOWN:34,PAGE_UP:33,
PERIOD:190,RIGHT:39,SHIFT:16,SPACE:32,TAB:9,UP:38}};c.fn.extend({_focus:c.fn.focus,focus:function(a,b){return typeof a==="number"?this.each(function(){var d=this;setTimeout(function(){c(d).focus();b&&b.call(d)},a)}):this._focus.apply(this,arguments)},enableSelection:function(){return this.attr("unselectable","off").css("MozUserSelect","")},disableSelection:function(){return this.attr("unselectable","on").css("MozUserSelect","none")},scrollParent:function(){var a;a=c.browser.msie&&/(static|relative)/.test(this.css("position"))||
/absolute/.test(this.css("position"))?this.parents().filter(function(){return/(relative|absolute|fixed)/.test(c.curCSS(this,"position",1))&&/(auto|scroll)/.test(c.curCSS(this,"overflow",1)+c.curCSS(this,"overflow-y",1)+c.curCSS(this,"overflow-x",1))}).eq(0):this.parents().filter(function(){return/(auto|scroll)/.test(c.curCSS(this,"overflow",1)+c.curCSS(this,"overflow-y",1)+c.curCSS(this,"overflow-x",1))}).eq(0);return/fixed/.test(this.css("position"))||!a.length?c(document):a},zIndex:function(a){if(a!==
undefined)return this.css("zIndex",a);if(this.length){a=c(this[0]);for(var b;a.length&&a[0]!==document;){b=a.css("position");if(b=="absolute"||b=="relative"||b=="fixed"){b=parseInt(a.css("zIndex"));if(!isNaN(b)&&b!=0)return b}a=a.parent()}}return 0}});c.extend(c.expr[":"],{data:function(a,b,d){return!!c.data(a,d[3])},focusable:function(a){var b=a.nodeName.toLowerCase(),d=c.attr(a,"tabindex");return(/input|select|textarea|button|object/.test(b)?!a.disabled:"a"==b||"area"==b?a.href||!isNaN(d):!isNaN(d))&&
!c(a)["area"==b?"parents":"closest"](":hidden").length},tabbable:function(a){var b=c.attr(a,"tabindex");return(isNaN(b)||b>=0)&&c(a).is(":focusable")}})}(jQuery);
;/*
 * jQuery UI Datepicker 1.8.1
 *
 * Copyright (c) 2010 AUTHORS.txt (http://jqueryui.com/about)
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 *
 * http://docs.jquery.com/UI/Datepicker
 *
 * Depends:
 *	jquery.ui.core.js
 */
(function(d){function J(){this.debug=false;this._curInst=null;this._keyEvent=false;this._disabledInputs=[];this._inDialog=this._datepickerShowing=false;this._mainDivId="ui-datepicker-div";this._inlineClass="ui-datepicker-inline";this._appendClass="ui-datepicker-append";this._triggerClass="ui-datepicker-trigger";this._dialogClass="ui-datepicker-dialog";this._disableClass="ui-datepicker-disabled";this._unselectableClass="ui-datepicker-unselectable";this._currentClass="ui-datepicker-current-day";this._dayOverClass=
"ui-datepicker-days-cell-over";this.regional=[];this.regional[""]={closeText:"Done",prevText:"Prev",nextText:"Next",currentText:"Today",monthNames:["January","February","March","April","May","June","July","August","September","October","November","December"],monthNamesShort:["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],dayNames:["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"],dayNamesShort:["Sun","Mon","Tue","Wed","Thu","Fri","Sat"],dayNamesMin:["Su",
"Mo","Tu","We","Th","Fr","Sa"],weekHeader:"Wk",dateFormat:"mm/dd/yy",firstDay:0,isRTL:false,showMonthAfterYear:false,yearSuffix:""};this._defaults={showOn:"focus",showAnim:"show",showOptions:{},defaultDate:null,appendText:"",buttonText:"...",buttonImage:"",buttonImageOnly:false,hideIfNoPrevNext:false,navigationAsDateFormat:false,gotoCurrent:false,changeMonth:false,changeYear:false,yearRange:"c-10:c+10",showOtherMonths:false,selectOtherMonths:false,showWeek:false,calculateWeek:this.iso8601Week,shortYearCutoff:"+10",
minDate:null,maxDate:null,duration:"_default",beforeShowDay:null,beforeShow:null,onSelect:null,onChangeMonthYear:null,onClose:null,numberOfMonths:1,showCurrentAtPos:0,stepMonths:1,stepBigMonths:12,altField:"",altFormat:"",constrainInput:true,showButtonPanel:false,autoSize:false};d.extend(this._defaults,this.regional[""]);this.dpDiv=d('<div id="'+this._mainDivId+'" class="ui-datepicker ui-widget ui-widget-content ui-helper-clearfix ui-corner-all ui-helper-hidden-accessible"></div>')}function E(a,b){d.extend(a,
b);for(var c in b)if(b[c]==null||b[c]==undefined)a[c]=b[c];return a}d.extend(d.ui,{datepicker:{version:"1.8.1"}});var y=(new Date).getTime();d.extend(J.prototype,{markerClassName:"hasDatepicker",log:function(){this.debug&&console.log.apply("",arguments)},_widgetDatepicker:function(){return this.dpDiv},setDefaults:function(a){E(this._defaults,a||{});return this},_attachDatepicker:function(a,b){var c=null;for(var e in this._defaults){var f=a.getAttribute("date:"+e);if(f){c=c||{};try{c[e]=eval(f)}catch(h){c[e]=
f}}}e=a.nodeName.toLowerCase();f=e=="div"||e=="span";if(!a.id)a.id="dp"+ ++this.uuid;var i=this._newInst(d(a),f);i.settings=d.extend({},b||{},c||{});if(e=="input")this._connectDatepicker(a,i);else f&&this._inlineDatepicker(a,i)},_newInst:function(a,b){return{id:a[0].id.replace(/([^A-Za-z0-9_])/g,"\\\\$1"),input:a,selectedDay:0,selectedMonth:0,selectedYear:0,drawMonth:0,drawYear:0,inline:b,dpDiv:!b?this.dpDiv:d('<div class="'+this._inlineClass+' ui-datepicker ui-widget ui-widget-content ui-helper-clearfix ui-corner-all"></div>')}},
_connectDatepicker:function(a,b){var c=d(a);b.append=d([]);b.trigger=d([]);if(!c.hasClass(this.markerClassName)){this._attachments(c,b);c.addClass(this.markerClassName).keydown(this._doKeyDown).keypress(this._doKeyPress).keyup(this._doKeyUp).bind("setData.datepicker",function(e,f,h){b.settings[f]=h}).bind("getData.datepicker",function(e,f){return this._get(b,f)});this._autoSize(b);d.data(a,"datepicker",b)}},_attachments:function(a,b){var c=this._get(b,"appendText"),e=this._get(b,"isRTL");b.append&&
b.append.remove();if(c){b.append=d('<span class="'+this._appendClass+'">'+c+"</span>");a[e?"before":"after"](b.append)}a.unbind("focus",this._showDatepicker);b.trigger&&b.trigger.remove();c=this._get(b,"showOn");if(c=="focus"||c=="both")a.focus(this._showDatepicker);if(c=="button"||c=="both"){c=this._get(b,"buttonText");var f=this._get(b,"buttonImage");b.trigger=d(this._get(b,"buttonImageOnly")?d("<img/>").addClass(this._triggerClass).attr({src:f,alt:c,title:c}):d('<button type="button"></button>').addClass(this._triggerClass).html(f==
""?c:d("<img/>").attr({src:f,alt:c,title:c})));a[e?"before":"after"](b.trigger);b.trigger.click(function(){d.datepicker._datepickerShowing&&d.datepicker._lastInput==a[0]?d.datepicker._hideDatepicker():d.datepicker._showDatepicker(a[0]);return false})}},_autoSize:function(a){if(this._get(a,"autoSize")&&!a.inline){var b=new Date(2009,11,20),c=this._get(a,"dateFormat");if(c.match(/[DM]/)){var e=function(f){for(var h=0,i=0,g=0;g<f.length;g++)if(f[g].length>h){h=f[g].length;i=g}return i};b.setMonth(e(this._get(a,
c.match(/MM/)?"monthNames":"monthNamesShort")));b.setDate(e(this._get(a,c.match(/DD/)?"dayNames":"dayNamesShort"))+20-b.getDay())}a.input.attr("size",this._formatDate(a,b).length)}},_inlineDatepicker:function(a,b){var c=d(a);if(!c.hasClass(this.markerClassName)){c.addClass(this.markerClassName).append(b.dpDiv).bind("setData.datepicker",function(e,f,h){b.settings[f]=h}).bind("getData.datepicker",function(e,f){return this._get(b,f)});d.data(a,"datepicker",b);this._setDate(b,this._getDefaultDate(b),
true);this._updateDatepicker(b);this._updateAlternate(b)}},_dialogDatepicker:function(a,b,c,e,f){a=this._dialogInst;if(!a){a="dp"+ ++this.uuid;this._dialogInput=d('<input type="text" id="'+a+'" style="position: absolute; top: -100px; width: 0px; z-index: -10;"/>');this._dialogInput.keydown(this._doKeyDown);d("body").append(this._dialogInput);a=this._dialogInst=this._newInst(this._dialogInput,false);a.settings={};d.data(this._dialogInput[0],"datepicker",a)}E(a.settings,e||{});b=b&&b.constructor==Date?
this._formatDate(a,b):b;this._dialogInput.val(b);this._pos=f?f.length?f:[f.pageX,f.pageY]:null;if(!this._pos)this._pos=[document.documentElement.clientWidth/2-100+(document.documentElement.scrollLeft||document.body.scrollLeft),document.documentElement.clientHeight/2-150+(document.documentElement.scrollTop||document.body.scrollTop)];this._dialogInput.css("left",this._pos[0]+20+"px").css("top",this._pos[1]+"px");a.settings.onSelect=c;this._inDialog=true;this.dpDiv.addClass(this._dialogClass);this._showDatepicker(this._dialogInput[0]);
d.blockUI&&d.blockUI(this.dpDiv);d.data(this._dialogInput[0],"datepicker",a);return this},_destroyDatepicker:function(a){var b=d(a),c=d.data(a,"datepicker");if(b.hasClass(this.markerClassName)){var e=a.nodeName.toLowerCase();d.removeData(a,"datepicker");if(e=="input"){c.append.remove();c.trigger.remove();b.removeClass(this.markerClassName).unbind("focus",this._showDatepicker).unbind("keydown",this._doKeyDown).unbind("keypress",this._doKeyPress).unbind("keyup",this._doKeyUp)}else if(e=="div"||e=="span")b.removeClass(this.markerClassName).empty()}},
_enableDatepicker:function(a){var b=d(a),c=d.data(a,"datepicker");if(b.hasClass(this.markerClassName)){var e=a.nodeName.toLowerCase();if(e=="input"){a.disabled=false;c.trigger.filter("button").each(function(){this.disabled=false}).end().filter("img").css({opacity:"1.0",cursor:""})}else if(e=="div"||e=="span")b.children("."+this._inlineClass).children().removeClass("ui-state-disabled");this._disabledInputs=d.map(this._disabledInputs,function(f){return f==a?null:f})}},_disableDatepicker:function(a){var b=
d(a),c=d.data(a,"datepicker");if(b.hasClass(this.markerClassName)){var e=a.nodeName.toLowerCase();if(e=="input"){a.disabled=true;c.trigger.filter("button").each(function(){this.disabled=true}).end().filter("img").css({opacity:"0.5",cursor:"default"})}else if(e=="div"||e=="span")b.children("."+this._inlineClass).children().addClass("ui-state-disabled");this._disabledInputs=d.map(this._disabledInputs,function(f){return f==a?null:f});this._disabledInputs[this._disabledInputs.length]=a}},_isDisabledDatepicker:function(a){if(!a)return false;
for(var b=0;b<this._disabledInputs.length;b++)if(this._disabledInputs[b]==a)return true;return false},_getInst:function(a){try{return d.data(a,"datepicker")}catch(b){throw"Missing instance data for this datepicker";}},_optionDatepicker:function(a,b,c){var e=this._getInst(a);if(arguments.length==2&&typeof b=="string")return b=="defaults"?d.extend({},d.datepicker._defaults):e?b=="all"?d.extend({},e.settings):this._get(e,b):null;var f=b||{};if(typeof b=="string"){f={};f[b]=c}if(e){this._curInst==e&&
this._hideDatepicker();var h=this._getDateDatepicker(a,true);E(e.settings,f);this._attachments(d(a),e);this._autoSize(e);this._setDateDatepicker(a,h);this._updateDatepicker(e)}},_changeDatepicker:function(a,b,c){this._optionDatepicker(a,b,c)},_refreshDatepicker:function(a){(a=this._getInst(a))&&this._updateDatepicker(a)},_setDateDatepicker:function(a,b){if(a=this._getInst(a)){this._setDate(a,b);this._updateDatepicker(a);this._updateAlternate(a)}},_getDateDatepicker:function(a,b){(a=this._getInst(a))&&
!a.inline&&this._setDateFromField(a,b);return a?this._getDate(a):null},_doKeyDown:function(a){var b=d.datepicker._getInst(a.target),c=true,e=b.dpDiv.is(".ui-datepicker-rtl");b._keyEvent=true;if(d.datepicker._datepickerShowing)switch(a.keyCode){case 9:d.datepicker._hideDatepicker();c=false;break;case 13:c=d("td."+d.datepicker._dayOverClass,b.dpDiv).add(d("td."+d.datepicker._currentClass,b.dpDiv));c[0]?d.datepicker._selectDay(a.target,b.selectedMonth,b.selectedYear,c[0]):d.datepicker._hideDatepicker();
return false;case 27:d.datepicker._hideDatepicker();break;case 33:d.datepicker._adjustDate(a.target,a.ctrlKey?-d.datepicker._get(b,"stepBigMonths"):-d.datepicker._get(b,"stepMonths"),"M");break;case 34:d.datepicker._adjustDate(a.target,a.ctrlKey?+d.datepicker._get(b,"stepBigMonths"):+d.datepicker._get(b,"stepMonths"),"M");break;case 35:if(a.ctrlKey||a.metaKey)d.datepicker._clearDate(a.target);c=a.ctrlKey||a.metaKey;break;case 36:if(a.ctrlKey||a.metaKey)d.datepicker._gotoToday(a.target);c=a.ctrlKey||
a.metaKey;break;case 37:if(a.ctrlKey||a.metaKey)d.datepicker._adjustDate(a.target,e?+1:-1,"D");c=a.ctrlKey||a.metaKey;if(a.originalEvent.altKey)d.datepicker._adjustDate(a.target,a.ctrlKey?-d.datepicker._get(b,"stepBigMonths"):-d.datepicker._get(b,"stepMonths"),"M");break;case 38:if(a.ctrlKey||a.metaKey)d.datepicker._adjustDate(a.target,-7,"D");c=a.ctrlKey||a.metaKey;break;case 39:if(a.ctrlKey||a.metaKey)d.datepicker._adjustDate(a.target,e?-1:+1,"D");c=a.ctrlKey||a.metaKey;if(a.originalEvent.altKey)d.datepicker._adjustDate(a.target,
a.ctrlKey?+d.datepicker._get(b,"stepBigMonths"):+d.datepicker._get(b,"stepMonths"),"M");break;case 40:if(a.ctrlKey||a.metaKey)d.datepicker._adjustDate(a.target,+7,"D");c=a.ctrlKey||a.metaKey;break;default:c=false}else if(a.keyCode==36&&a.ctrlKey)d.datepicker._showDatepicker(this);else c=false;if(c){a.preventDefault();a.stopPropagation()}},_doKeyPress:function(a){var b=d.datepicker._getInst(a.target);if(d.datepicker._get(b,"constrainInput")){b=d.datepicker._possibleChars(d.datepicker._get(b,"dateFormat"));
var c=String.fromCharCode(a.charCode==undefined?a.keyCode:a.charCode);return a.ctrlKey||c<" "||!b||b.indexOf(c)>-1}},_doKeyUp:function(a){a=d.datepicker._getInst(a.target);if(a.input.val()!=a.lastVal)try{if(d.datepicker.parseDate(d.datepicker._get(a,"dateFormat"),a.input?a.input.val():null,d.datepicker._getFormatConfig(a))){d.datepicker._setDateFromField(a);d.datepicker._updateAlternate(a);d.datepicker._updateDatepicker(a)}}catch(b){d.datepicker.log(b)}return true},_showDatepicker:function(a){a=a.target||
a;if(a.nodeName.toLowerCase()!="input")a=d("input",a.parentNode)[0];if(!(d.datepicker._isDisabledDatepicker(a)||d.datepicker._lastInput==a)){var b=d.datepicker._getInst(a);d.datepicker._curInst&&d.datepicker._curInst!=b&&d.datepicker._curInst.dpDiv.stop(true,true);var c=d.datepicker._get(b,"beforeShow");E(b.settings,c?c.apply(a,[a,b]):{});b.lastVal=null;d.datepicker._lastInput=a;d.datepicker._setDateFromField(b);if(d.datepicker._inDialog)a.value="";if(!d.datepicker._pos){d.datepicker._pos=d.datepicker._findPos(a);
d.datepicker._pos[1]+=a.offsetHeight}var e=false;d(a).parents().each(function(){e|=d(this).css("position")=="fixed";return!e});if(e&&d.browser.opera){d.datepicker._pos[0]-=document.documentElement.scrollLeft;d.datepicker._pos[1]-=document.documentElement.scrollTop}c={left:d.datepicker._pos[0],top:d.datepicker._pos[1]};d.datepicker._pos=null;b.dpDiv.css({position:"absolute",display:"block",top:"-1000px"});d.datepicker._updateDatepicker(b);c=d.datepicker._checkOffset(b,c,e);b.dpDiv.css({position:d.datepicker._inDialog&&
d.blockUI?"static":e?"fixed":"absolute",display:"none",left:c.left+"px",top:c.top+"px"});if(!b.inline){c=d.datepicker._get(b,"showAnim");var f=d.datepicker._get(b,"duration"),h=function(){d.datepicker._datepickerShowing=true;var i=d.datepicker._getBorders(b.dpDiv);b.dpDiv.find("iframe.ui-datepicker-cover").css({left:-i[0],top:-i[1],width:b.dpDiv.outerWidth(),height:b.dpDiv.outerHeight()})};b.dpDiv.zIndex(d(a).zIndex()+1);d.effects&&d.effects[c]?b.dpDiv.show(c,d.datepicker._get(b,"showOptions"),f,
h):b.dpDiv[c||"show"](c?f:null,h);if(!c||!f)h();b.input.is(":visible")&&!b.input.is(":disabled")&&b.input.focus();d.datepicker._curInst=b}}},_updateDatepicker:function(a){var b=this,c=d.datepicker._getBorders(a.dpDiv);a.dpDiv.empty().append(this._generateHTML(a)).find("iframe.ui-datepicker-cover").css({left:-c[0],top:-c[1],width:a.dpDiv.outerWidth(),height:a.dpDiv.outerHeight()}).end().find("button, .ui-datepicker-prev, .ui-datepicker-next, .ui-datepicker-calendar td a").bind("mouseout",function(){d(this).removeClass("ui-state-hover");
this.className.indexOf("ui-datepicker-prev")!=-1&&d(this).removeClass("ui-datepicker-prev-hover");this.className.indexOf("ui-datepicker-next")!=-1&&d(this).removeClass("ui-datepicker-next-hover")}).bind("mouseover",function(){if(!b._isDisabledDatepicker(a.inline?a.dpDiv.parent()[0]:a.input[0])){d(this).parents(".ui-datepicker-calendar").find("a").removeClass("ui-state-hover");d(this).addClass("ui-state-hover");this.className.indexOf("ui-datepicker-prev")!=-1&&d(this).addClass("ui-datepicker-prev-hover");
this.className.indexOf("ui-datepicker-next")!=-1&&d(this).addClass("ui-datepicker-next-hover")}}).end().find("."+this._dayOverClass+" a").trigger("mouseover").end();c=this._getNumberOfMonths(a);var e=c[1];e>1?a.dpDiv.addClass("ui-datepicker-multi-"+e).css("width",17*e+"em"):a.dpDiv.removeClass("ui-datepicker-multi-2 ui-datepicker-multi-3 ui-datepicker-multi-4").width("");a.dpDiv[(c[0]!=1||c[1]!=1?"add":"remove")+"Class"]("ui-datepicker-multi");a.dpDiv[(this._get(a,"isRTL")?"add":"remove")+"Class"]("ui-datepicker-rtl");
a==d.datepicker._curInst&&d.datepicker._datepickerShowing&&a.input&&a.input.is(":visible")&&!a.input.is(":disabled")&&a.input.focus()},_getBorders:function(a){var b=function(c){return{thin:1,medium:2,thick:3}[c]||c};return[parseFloat(b(a.css("border-left-width"))),parseFloat(b(a.css("border-top-width")))]},_checkOffset:function(a,b,c){var e=a.dpDiv.outerWidth(),f=a.dpDiv.outerHeight(),h=a.input?a.input.outerWidth():0,i=a.input?a.input.outerHeight():0,g=document.documentElement.clientWidth+d(document).scrollLeft(),
k=document.documentElement.clientHeight+d(document).scrollTop();b.left-=this._get(a,"isRTL")?e-h:0;b.left-=c&&b.left==a.input.offset().left?d(document).scrollLeft():0;b.top-=c&&b.top==a.input.offset().top+i?d(document).scrollTop():0;b.left-=Math.min(b.left,b.left+e>g&&g>e?Math.abs(b.left+e-g):0);b.top-=Math.min(b.top,b.top+f>k&&k>f?Math.abs(f+i):0);return b},_findPos:function(a){for(var b=this._get(this._getInst(a),"isRTL");a&&(a.type=="hidden"||a.nodeType!=1);)a=a[b?"previousSibling":"nextSibling"];
a=d(a).offset();return[a.left,a.top]},_hideDatepicker:function(a){var b=this._curInst;if(!(!b||a&&b!=d.data(a,"datepicker")))if(this._datepickerShowing){a=this._get(b,"showAnim");var c=this._get(b,"duration"),e=function(){d.datepicker._tidyDialog(b);this._curInst=null};d.effects&&d.effects[a]?b.dpDiv.hide(a,d.datepicker._get(b,"showOptions"),c,e):b.dpDiv[a=="slideDown"?"slideUp":a=="fadeIn"?"fadeOut":"hide"](a?c:null,e);a||e();if(a=this._get(b,"onClose"))a.apply(b.input?b.input[0]:null,[b.input?b.input.val():
"",b]);this._datepickerShowing=false;this._lastInput=null;if(this._inDialog){this._dialogInput.css({position:"absolute",left:"0",top:"-100px"});if(d.blockUI){d.unblockUI();d("body").append(this.dpDiv)}}this._inDialog=false}},_tidyDialog:function(a){a.dpDiv.removeClass(this._dialogClass).unbind(".ui-datepicker-calendar")},_checkExternalClick:function(a){if(d.datepicker._curInst){a=d(a.target);a[0].id!=d.datepicker._mainDivId&&a.parents("#"+d.datepicker._mainDivId).length==0&&!a.hasClass(d.datepicker.markerClassName)&&
!a.hasClass(d.datepicker._triggerClass)&&d.datepicker._datepickerShowing&&!(d.datepicker._inDialog&&d.blockUI)&&d.datepicker._hideDatepicker()}},_adjustDate:function(a,b,c){a=d(a);var e=this._getInst(a[0]);if(!this._isDisabledDatepicker(a[0])){this._adjustInstDate(e,b+(c=="M"?this._get(e,"showCurrentAtPos"):0),c);this._updateDatepicker(e)}},_gotoToday:function(a){a=d(a);var b=this._getInst(a[0]);if(this._get(b,"gotoCurrent")&&b.currentDay){b.selectedDay=b.currentDay;b.drawMonth=b.selectedMonth=b.currentMonth;
b.drawYear=b.selectedYear=b.currentYear}else{var c=new Date;b.selectedDay=c.getDate();b.drawMonth=b.selectedMonth=c.getMonth();b.drawYear=b.selectedYear=c.getFullYear()}this._notifyChange(b);this._adjustDate(a)},_selectMonthYear:function(a,b,c){a=d(a);var e=this._getInst(a[0]);e._selectingMonthYear=false;e["selected"+(c=="M"?"Month":"Year")]=e["draw"+(c=="M"?"Month":"Year")]=parseInt(b.options[b.selectedIndex].value,10);this._notifyChange(e);this._adjustDate(a)},_clickMonthYear:function(a){a=this._getInst(d(a)[0]);
a.input&&a._selectingMonthYear&&!d.browser.msie&&a.input.focus();a._selectingMonthYear=!a._selectingMonthYear},_selectDay:function(a,b,c,e){var f=d(a);if(!(d(e).hasClass(this._unselectableClass)||this._isDisabledDatepicker(f[0]))){f=this._getInst(f[0]);f.selectedDay=f.currentDay=d("a",e).html();f.selectedMonth=f.currentMonth=b;f.selectedYear=f.currentYear=c;this._selectDate(a,this._formatDate(f,f.currentDay,f.currentMonth,f.currentYear))}},_clearDate:function(a){a=d(a);this._getInst(a[0]);this._selectDate(a,
"")},_selectDate:function(a,b){a=this._getInst(d(a)[0]);b=b!=null?b:this._formatDate(a);a.input&&a.input.val(b);this._updateAlternate(a);var c=this._get(a,"onSelect");if(c)c.apply(a.input?a.input[0]:null,[b,a]);else a.input&&a.input.trigger("change");if(a.inline)this._updateDatepicker(a);else{this._hideDatepicker();this._lastInput=a.input[0];typeof a.input[0]!="object"&&a.input.focus();this._lastInput=null}},_updateAlternate:function(a){var b=this._get(a,"altField");if(b){var c=this._get(a,"altFormat")||
this._get(a,"dateFormat"),e=this._getDate(a),f=this.formatDate(c,e,this._getFormatConfig(a));d(b).each(function(){d(this).val(f)})}},noWeekends:function(a){a=a.getDay();return[a>0&&a<6,""]},iso8601Week:function(a){a=new Date(a.getTime());a.setDate(a.getDate()+4-(a.getDay()||7));var b=a.getTime();a.setMonth(0);a.setDate(1);return Math.floor(Math.round((b-a)/864E5)/7)+1},parseDate:function(a,b,c){if(a==null||b==null)throw"Invalid arguments";b=typeof b=="object"?b.toString():b+"";if(b=="")return null;
for(var e=(c?c.shortYearCutoff:null)||this._defaults.shortYearCutoff,f=(c?c.dayNamesShort:null)||this._defaults.dayNamesShort,h=(c?c.dayNames:null)||this._defaults.dayNames,i=(c?c.monthNamesShort:null)||this._defaults.monthNamesShort,g=(c?c.monthNames:null)||this._defaults.monthNames,k=c=-1,l=-1,u=-1,j=false,o=function(p){(p=z+1<a.length&&a.charAt(z+1)==p)&&z++;return p},m=function(p){o(p);p=new RegExp("^\\d{1,"+(p=="@"?14:p=="!"?20:p=="y"?4:p=="o"?3:2)+"}");p=b.substring(s).match(p);if(!p)throw"Missing number at position "+
s;s+=p[0].length;return parseInt(p[0],10)},n=function(p,w,G){p=o(p)?G:w;for(w=0;w<p.length;w++)if(b.substr(s,p[w].length)==p[w]){s+=p[w].length;return w+1}throw"Unknown name at position "+s;},r=function(){if(b.charAt(s)!=a.charAt(z))throw"Unexpected literal at position "+s;s++},s=0,z=0;z<a.length;z++)if(j)if(a.charAt(z)=="'"&&!o("'"))j=false;else r();else switch(a.charAt(z)){case "d":l=m("d");break;case "D":n("D",f,h);break;case "o":u=m("o");break;case "m":k=m("m");break;case "M":k=n("M",i,g);break;
case "y":c=m("y");break;case "@":var v=new Date(m("@"));c=v.getFullYear();k=v.getMonth()+1;l=v.getDate();break;case "!":v=new Date((m("!")-this._ticksTo1970)/1E4);c=v.getFullYear();k=v.getMonth()+1;l=v.getDate();break;case "'":if(o("'"))r();else j=true;break;default:r()}if(c==-1)c=(new Date).getFullYear();else if(c<100)c+=(new Date).getFullYear()-(new Date).getFullYear()%100+(c<=e?0:-100);if(u>-1){k=1;l=u;do{e=this._getDaysInMonth(c,k-1);if(l<=e)break;k++;l-=e}while(1)}v=this._daylightSavingAdjust(new Date(c,
k-1,l));if(v.getFullYear()!=c||v.getMonth()+1!=k||v.getDate()!=l)throw"Invalid date";return v},ATOM:"yy-mm-dd",COOKIE:"D, dd M yy",ISO_8601:"yy-mm-dd",RFC_822:"D, d M y",RFC_850:"DD, dd-M-y",RFC_1036:"D, d M y",RFC_1123:"D, d M yy",RFC_2822:"D, d M yy",RSS:"D, d M y",TICKS:"!",TIMESTAMP:"@",W3C:"yy-mm-dd",_ticksTo1970:(718685+Math.floor(492.5)-Math.floor(19.7)+Math.floor(4.925))*24*60*60*1E7,formatDate:function(a,b,c){if(!b)return"";var e=(c?c.dayNamesShort:null)||this._defaults.dayNamesShort,f=(c?
c.dayNames:null)||this._defaults.dayNames,h=(c?c.monthNamesShort:null)||this._defaults.monthNamesShort;c=(c?c.monthNames:null)||this._defaults.monthNames;var i=function(o){(o=j+1<a.length&&a.charAt(j+1)==o)&&j++;return o},g=function(o,m,n){m=""+m;if(i(o))for(;m.length<n;)m="0"+m;return m},k=function(o,m,n,r){return i(o)?r[m]:n[m]},l="",u=false;if(b)for(var j=0;j<a.length;j++)if(u)if(a.charAt(j)=="'"&&!i("'"))u=false;else l+=a.charAt(j);else switch(a.charAt(j)){case "d":l+=g("d",b.getDate(),2);break;
case "D":l+=k("D",b.getDay(),e,f);break;case "o":l+=g("o",(b.getTime()-(new Date(b.getFullYear(),0,0)).getTime())/864E5,3);break;case "m":l+=g("m",b.getMonth()+1,2);break;case "M":l+=k("M",b.getMonth(),h,c);break;case "y":l+=i("y")?b.getFullYear():(b.getYear()%100<10?"0":"")+b.getYear()%100;break;case "@":l+=b.getTime();break;case "!":l+=b.getTime()*1E4+this._ticksTo1970;break;case "'":if(i("'"))l+="'";else u=true;break;default:l+=a.charAt(j)}return l},_possibleChars:function(a){for(var b="",c=false,
e=function(h){(h=f+1<a.length&&a.charAt(f+1)==h)&&f++;return h},f=0;f<a.length;f++)if(c)if(a.charAt(f)=="'"&&!e("'"))c=false;else b+=a.charAt(f);else switch(a.charAt(f)){case "d":case "m":case "y":case "@":b+="0123456789";break;case "D":case "M":return null;case "'":if(e("'"))b+="'";else c=true;break;default:b+=a.charAt(f)}return b},_get:function(a,b){return a.settings[b]!==undefined?a.settings[b]:this._defaults[b]},_setDateFromField:function(a,b){if(a.input.val()!=a.lastVal){var c=this._get(a,"dateFormat"),
e=a.lastVal=a.input?a.input.val():null,f,h;f=h=this._getDefaultDate(a);var i=this._getFormatConfig(a);try{f=this.parseDate(c,e,i)||h}catch(g){this.log(g);e=b?"":e}a.selectedDay=f.getDate();a.drawMonth=a.selectedMonth=f.getMonth();a.drawYear=a.selectedYear=f.getFullYear();a.currentDay=e?f.getDate():0;a.currentMonth=e?f.getMonth():0;a.currentYear=e?f.getFullYear():0;this._adjustInstDate(a)}},_getDefaultDate:function(a){return this._restrictMinMax(a,this._determineDate(a,this._get(a,"defaultDate"),new Date))},
_determineDate:function(a,b,c){var e=function(h){var i=new Date;i.setDate(i.getDate()+h);return i},f=function(h){try{return d.datepicker.parseDate(d.datepicker._get(a,"dateFormat"),h,d.datepicker._getFormatConfig(a))}catch(i){}var g=(h.toLowerCase().match(/^c/)?d.datepicker._getDate(a):null)||new Date,k=g.getFullYear(),l=g.getMonth();g=g.getDate();for(var u=/([+-]?[0-9]+)\s*(d|D|w|W|m|M|y|Y)?/g,j=u.exec(h);j;){switch(j[2]||"d"){case "d":case "D":g+=parseInt(j[1],10);break;case "w":case "W":g+=parseInt(j[1],
10)*7;break;case "m":case "M":l+=parseInt(j[1],10);g=Math.min(g,d.datepicker._getDaysInMonth(k,l));break;case "y":case "Y":k+=parseInt(j[1],10);g=Math.min(g,d.datepicker._getDaysInMonth(k,l));break}j=u.exec(h)}return new Date(k,l,g)};if(b=(b=b==null?c:typeof b=="string"?f(b):typeof b=="number"?isNaN(b)?c:e(b):b)&&b.toString()=="Invalid Date"?c:b){b.setHours(0);b.setMinutes(0);b.setSeconds(0);b.setMilliseconds(0)}return this._daylightSavingAdjust(b)},_daylightSavingAdjust:function(a){if(!a)return null;
a.setHours(a.getHours()>12?a.getHours()+2:0);return a},_setDate:function(a,b,c){var e=!b,f=a.selectedMonth,h=a.selectedYear;b=this._restrictMinMax(a,this._determineDate(a,b,new Date));a.selectedDay=a.currentDay=b.getDate();a.drawMonth=a.selectedMonth=a.currentMonth=b.getMonth();a.drawYear=a.selectedYear=a.currentYear=b.getFullYear();if((f!=a.selectedMonth||h!=a.selectedYear)&&!c)this._notifyChange(a);this._adjustInstDate(a);if(a.input)a.input.val(e?"":this._formatDate(a))},_getDate:function(a){return!a.currentYear||
a.input&&a.input.val()==""?null:this._daylightSavingAdjust(new Date(a.currentYear,a.currentMonth,a.currentDay))},_generateHTML:function(a){var b=new Date;b=this._daylightSavingAdjust(new Date(b.getFullYear(),b.getMonth(),b.getDate()));var c=this._get(a,"isRTL"),e=this._get(a,"showButtonPanel"),f=this._get(a,"hideIfNoPrevNext"),h=this._get(a,"navigationAsDateFormat"),i=this._getNumberOfMonths(a),g=this._get(a,"showCurrentAtPos"),k=this._get(a,"stepMonths"),l=i[0]!=1||i[1]!=1,u=this._daylightSavingAdjust(!a.currentDay?
new Date(9999,9,9):new Date(a.currentYear,a.currentMonth,a.currentDay)),j=this._getMinMaxDate(a,"min"),o=this._getMinMaxDate(a,"max");g=a.drawMonth-g;var m=a.drawYear;if(g<0){g+=12;m--}if(o){var n=this._daylightSavingAdjust(new Date(o.getFullYear(),o.getMonth()-i[0]*i[1]+1,o.getDate()));for(n=j&&n<j?j:n;this._daylightSavingAdjust(new Date(m,g,1))>n;){g--;if(g<0){g=11;m--}}}a.drawMonth=g;a.drawYear=m;n=this._get(a,"prevText");n=!h?n:this.formatDate(n,this._daylightSavingAdjust(new Date(m,g-k,1)),this._getFormatConfig(a));
n=this._canAdjustMonth(a,-1,m,g)?'<a class="ui-datepicker-prev ui-corner-all" onclick="DP_jQuery_'+y+".datepicker._adjustDate('#"+a.id+"', -"+k+", 'M');\" title=\""+n+'"><span class="ui-icon ui-icon-circle-triangle-'+(c?"e":"w")+'">'+n+"</span></a>":f?"":'<a class="ui-datepicker-prev ui-corner-all ui-state-disabled" title="'+n+'"><span class="ui-icon ui-icon-circle-triangle-'+(c?"e":"w")+'">'+n+"</span></a>";var r=this._get(a,"nextText");r=!h?r:this.formatDate(r,this._daylightSavingAdjust(new Date(m,
g+k,1)),this._getFormatConfig(a));f=this._canAdjustMonth(a,+1,m,g)?'<a class="ui-datepicker-next ui-corner-all" onclick="DP_jQuery_'+y+".datepicker._adjustDate('#"+a.id+"', +"+k+", 'M');\" title=\""+r+'"><span class="ui-icon ui-icon-circle-triangle-'+(c?"w":"e")+'">'+r+"</span></a>":f?"":'<a class="ui-datepicker-next ui-corner-all ui-state-disabled" title="'+r+'"><span class="ui-icon ui-icon-circle-triangle-'+(c?"w":"e")+'">'+r+"</span></a>";k=this._get(a,"currentText");r=this._get(a,"gotoCurrent")&&
a.currentDay?u:b;k=!h?k:this.formatDate(k,r,this._getFormatConfig(a));h=!a.inline?'<button type="button" class="ui-datepicker-close ui-state-default ui-priority-primary ui-corner-all" onclick="DP_jQuery_'+y+'.datepicker._hideDatepicker();">'+this._get(a,"closeText")+"</button>":"";e=e?'<div class="ui-datepicker-buttonpane ui-widget-content">'+(c?h:"")+(this._isInRange(a,r)?'<button type="button" class="ui-datepicker-current ui-state-default ui-priority-secondary ui-corner-all" onclick="DP_jQuery_'+
y+".datepicker._gotoToday('#"+a.id+"');\">"+k+"</button>":"")+(c?"":h)+"</div>":"";h=parseInt(this._get(a,"firstDay"),10);h=isNaN(h)?0:h;k=this._get(a,"showWeek");r=this._get(a,"dayNames");this._get(a,"dayNamesShort");var s=this._get(a,"dayNamesMin"),z=this._get(a,"monthNames"),v=this._get(a,"monthNamesShort"),p=this._get(a,"beforeShowDay"),w=this._get(a,"showOtherMonths"),G=this._get(a,"selectOtherMonths");this._get(a,"calculateWeek");for(var K=this._getDefaultDate(a),H="",C=0;C<i[0];C++){for(var L=
"",D=0;D<i[1];D++){var M=this._daylightSavingAdjust(new Date(m,g,a.selectedDay)),t=" ui-corner-all",x="";if(l){x+='<div class="ui-datepicker-group';if(i[1]>1)switch(D){case 0:x+=" ui-datepicker-group-first";t=" ui-corner-"+(c?"right":"left");break;case i[1]-1:x+=" ui-datepicker-group-last";t=" ui-corner-"+(c?"left":"right");break;default:x+=" ui-datepicker-group-middle";t="";break}x+='">'}x+='<div class="ui-datepicker-header ui-widget-header ui-helper-clearfix'+t+'">'+(/all|left/.test(t)&&C==0?c?
f:n:"")+(/all|right/.test(t)&&C==0?c?n:f:"")+this._generateMonthYearHeader(a,g,m,j,o,C>0||D>0,z,v)+'</div><table class="ui-datepicker-calendar"><thead><tr>';var A=k?'<th class="ui-datepicker-week-col">'+this._get(a,"weekHeader")+"</th>":"";for(t=0;t<7;t++){var q=(t+h)%7;A+="<th"+((t+h+6)%7>=5?' class="ui-datepicker-week-end"':"")+'><span title="'+r[q]+'">'+s[q]+"</span></th>"}x+=A+"</tr></thead><tbody>";A=this._getDaysInMonth(m,g);if(m==a.selectedYear&&g==a.selectedMonth)a.selectedDay=Math.min(a.selectedDay,
A);t=(this._getFirstDayOfMonth(m,g)-h+7)%7;A=l?6:Math.ceil((t+A)/7);q=this._daylightSavingAdjust(new Date(m,g,1-t));for(var N=0;N<A;N++){x+="<tr>";var O=!k?"":'<td class="ui-datepicker-week-col">'+this._get(a,"calculateWeek")(q)+"</td>";for(t=0;t<7;t++){var F=p?p.apply(a.input?a.input[0]:null,[q]):[true,""],B=q.getMonth()!=g,I=B&&!G||!F[0]||j&&q<j||o&&q>o;O+='<td class="'+((t+h+6)%7>=5?" ui-datepicker-week-end":"")+(B?" ui-datepicker-other-month":"")+(q.getTime()==M.getTime()&&g==a.selectedMonth&&
a._keyEvent||K.getTime()==q.getTime()&&K.getTime()==M.getTime()?" "+this._dayOverClass:"")+(I?" "+this._unselectableClass+" ui-state-disabled":"")+(B&&!w?"":" "+F[1]+(q.getTime()==u.getTime()?" "+this._currentClass:"")+(q.getTime()==b.getTime()?" ui-datepicker-today":""))+'"'+((!B||w)&&F[2]?' title="'+F[2]+'"':"")+(I?"":' onclick="DP_jQuery_'+y+".datepicker._selectDay('#"+a.id+"',"+q.getMonth()+","+q.getFullYear()+', this);return false;"')+">"+(B&&!w?"&#xa0;":I?'<span class="ui-state-default">'+q.getDate()+
"</span>":'<a class="ui-state-default'+(q.getTime()==b.getTime()?" ui-state-highlight":"")+(q.getTime()==u.getTime()?" ui-state-active":"")+(B?" ui-priority-secondary":"")+'" href="#">'+q.getDate()+"</a>")+"</td>";q.setDate(q.getDate()+1);q=this._daylightSavingAdjust(q)}x+=O+"</tr>"}g++;if(g>11){g=0;m++}x+="</tbody></table>"+(l?"</div>"+(i[0]>0&&D==i[1]-1?'<div class="ui-datepicker-row-break"></div>':""):"");L+=x}H+=L}H+=e+(d.browser.msie&&parseInt(d.browser.version,10)<7&&!a.inline?'<iframe src="javascript:false;" class="ui-datepicker-cover" frameborder="0"></iframe>':
"");a._keyEvent=false;return H},_generateMonthYearHeader:function(a,b,c,e,f,h,i,g){var k=this._get(a,"changeMonth"),l=this._get(a,"changeYear"),u=this._get(a,"showMonthAfterYear"),j='<div class="ui-datepicker-title">',o="";if(h||!k)o+='<span class="ui-datepicker-month">'+i[b]+"</span>";else{i=e&&e.getFullYear()==c;var m=f&&f.getFullYear()==c;o+='<select class="ui-datepicker-month" onchange="DP_jQuery_'+y+".datepicker._selectMonthYear('#"+a.id+"', this, 'M');\" onclick=\"DP_jQuery_"+y+".datepicker._clickMonthYear('#"+
a.id+"');\">";for(var n=0;n<12;n++)if((!i||n>=e.getMonth())&&(!m||n<=f.getMonth()))o+='<option value="'+n+'"'+(n==b?' selected="selected"':"")+">"+g[n]+"</option>";o+="</select>"}u||(j+=o+(h||!(k&&l)?"&#xa0;":""));if(h||!l)j+='<span class="ui-datepicker-year">'+c+"</span>";else{g=this._get(a,"yearRange").split(":");var r=(new Date).getFullYear();i=function(s){s=s.match(/c[+-].*/)?c+parseInt(s.substring(1),10):s.match(/[+-].*/)?r+parseInt(s,10):parseInt(s,10);return isNaN(s)?r:s};b=i(g[0]);g=Math.max(b,
i(g[1]||""));b=e?Math.max(b,e.getFullYear()):b;g=f?Math.min(g,f.getFullYear()):g;for(j+='<select class="ui-datepicker-year" onchange="DP_jQuery_'+y+".datepicker._selectMonthYear('#"+a.id+"', this, 'Y');\" onclick=\"DP_jQuery_"+y+".datepicker._clickMonthYear('#"+a.id+"');\">";b<=g;b++)j+='<option value="'+b+'"'+(b==c?' selected="selected"':"")+">"+b+"</option>";j+="</select>"}j+=this._get(a,"yearSuffix");if(u)j+=(h||!(k&&l)?"&#xa0;":"")+o;j+="</div>";return j},_adjustInstDate:function(a,b,c){var e=
a.drawYear+(c=="Y"?b:0),f=a.drawMonth+(c=="M"?b:0);b=Math.min(a.selectedDay,this._getDaysInMonth(e,f))+(c=="D"?b:0);e=this._restrictMinMax(a,this._daylightSavingAdjust(new Date(e,f,b)));a.selectedDay=e.getDate();a.drawMonth=a.selectedMonth=e.getMonth();a.drawYear=a.selectedYear=e.getFullYear();if(c=="M"||c=="Y")this._notifyChange(a)},_restrictMinMax:function(a,b){var c=this._getMinMaxDate(a,"min");a=this._getMinMaxDate(a,"max");b=c&&b<c?c:b;return b=a&&b>a?a:b},_notifyChange:function(a){var b=this._get(a,
"onChangeMonthYear");if(b)b.apply(a.input?a.input[0]:null,[a.selectedYear,a.selectedMonth+1,a])},_getNumberOfMonths:function(a){a=this._get(a,"numberOfMonths");return a==null?[1,1]:typeof a=="number"?[1,a]:a},_getMinMaxDate:function(a,b){return this._determineDate(a,this._get(a,b+"Date"),null)},_getDaysInMonth:function(a,b){return 32-(new Date(a,b,32)).getDate()},_getFirstDayOfMonth:function(a,b){return(new Date(a,b,1)).getDay()},_canAdjustMonth:function(a,b,c,e){var f=this._getNumberOfMonths(a);
c=this._daylightSavingAdjust(new Date(c,e+(b<0?b:f[0]*f[1]),1));b<0&&c.setDate(this._getDaysInMonth(c.getFullYear(),c.getMonth()));return this._isInRange(a,c)},_isInRange:function(a,b){var c=this._getMinMaxDate(a,"min");a=this._getMinMaxDate(a,"max");return(!c||b.getTime()>=c.getTime())&&(!a||b.getTime()<=a.getTime())},_getFormatConfig:function(a){var b=this._get(a,"shortYearCutoff");b=typeof b!="string"?b:(new Date).getFullYear()%100+parseInt(b,10);return{shortYearCutoff:b,dayNamesShort:this._get(a,
"dayNamesShort"),dayNames:this._get(a,"dayNames"),monthNamesShort:this._get(a,"monthNamesShort"),monthNames:this._get(a,"monthNames")}},_formatDate:function(a,b,c,e){if(!b){a.currentDay=a.selectedDay;a.currentMonth=a.selectedMonth;a.currentYear=a.selectedYear}b=b?typeof b=="object"?b:this._daylightSavingAdjust(new Date(e,c,b)):this._daylightSavingAdjust(new Date(a.currentYear,a.currentMonth,a.currentDay));return this.formatDate(this._get(a,"dateFormat"),b,this._getFormatConfig(a))}});d.fn.datepicker=
function(a){if(!d.datepicker.initialized){d(document).mousedown(d.datepicker._checkExternalClick).find("body").append(d.datepicker.dpDiv);d.datepicker.initialized=true}var b=Array.prototype.slice.call(arguments,1);if(typeof a=="string"&&(a=="isDisabled"||a=="getDate"||a=="widget"))return d.datepicker["_"+a+"Datepicker"].apply(d.datepicker,[this[0]].concat(b));if(a=="option"&&arguments.length==2&&typeof arguments[1]=="string")return d.datepicker["_"+a+"Datepicker"].apply(d.datepicker,[this[0]].concat(b));
return this.each(function(){typeof a=="string"?d.datepicker["_"+a+"Datepicker"].apply(d.datepicker,[this].concat(b)):d.datepicker._attachDatepicker(this,a)})};d.datepicker=new J;d.datepicker.initialized=false;d.datepicker.uuid=(new Date).getTime();d.datepicker.version="1.8.1";window["DP_jQuery_"+y]=d})(jQuery);
;
/**
 * jscolor, JavaScript Color Picker
 *
 * @version 1.3.1
 * @license GNU Lesser General Public License, http://www.gnu.org/copyleft/lesser.html
 * @author  Jan Odvarko, http://odvarko.cz
 * @created 2008-06-15
 * @updated 2010-01-23
 * @link    http://jscolor.com
 */


var jscolor = {


	dir : 'Scripts/External/jscolor/', // location of jscolor directory (leave empty to autodetect)
	bindClass : 'color', // class name
	binding : false, // automatic binding via <input class="...">
	preloading : true, // use image preloading?


	install : function() {
		jscolor.addEvent(window, 'load', jscolor.init);
	},


	init : function() {
		if(jscolor.binding) {
			jscolor.bind();
		}
		if(jscolor.preloading) {
			jscolor.preload();
		}
	},


	getDir : function() {
		if(!jscolor.dir) {
			var detected = jscolor.detectDir();
			jscolor.dir = detected!==false ? detected : 'jscolor/';
		}
		return jscolor.dir;
	},


	detectDir : function() {
		var base = location.href;

		var e = document.getElementsByTagName('base');
		for(var i=0; i<e.length; i+=1) {
			if(e[i].href) { base = e[i].href; }
		}

		var e = document.getElementsByTagName('script');
		for(var i=0; i<e.length; i+=1) {
			if(e[i].src && /(^|\/)jscolor\.js([?#].*)?$/i.test(e[i].src)) {
				var src = new jscolor.URI(e[i].src);
				var srcAbs = src.toAbsolute(base);
				srcAbs.path = srcAbs.path.replace(/[^\/]+$/, ''); // remove filename
				srcAbs.query = null;
				srcAbs.fragment = null;
				return srcAbs.toString();
			}
		}
		return false;
	},


	bind : function() {
		var matchClass = new RegExp('(^|\\s)('+jscolor.bindClass+')\\s*(\\{[^}]*\\})?', 'i');
		var e = document.getElementsByTagName('input');
		for(var i=0; i<e.length; i+=1) {
			var m;
			if(!e[i].color && e[i].className && (m = e[i].className.match(matchClass))) {
				var prop = {};
				if(m[3]) {
					try {
						eval('prop='+m[3]);
					} catch(eInvalidProp) {}
				}
				e[i].color = new jscolor.color(e[i], prop);
			}
		}
	},


	preload : function() {
		for(var fn in jscolor.imgRequire) {
			if(jscolor.imgRequire.hasOwnProperty(fn)) {
				jscolor.loadImage(fn);
			}
		}
	},


	images : {
		pad : [ 181, 101 ],
		sld : [ 16, 101 ],
		cross : [ 15, 15 ],
		arrow : [ 7, 11 ]
	},


	imgRequire : {},
	imgLoaded : {},


	requireImage : function(filename) {
		jscolor.imgRequire[filename] = true;
	},


	loadImage : function(filename) {
		if(!jscolor.imgLoaded[filename]) {
			jscolor.imgLoaded[filename] = new Image();
			jscolor.imgLoaded[filename].src = jscolor.getDir()+filename;
		}
	},


	fetchElement : function(mixed) {
		return typeof mixed === 'string' ? document.getElementById(mixed) : mixed;
	},


	addEvent : function(el, evnt, func) {
		if(el.addEventListener) {
			el.addEventListener(evnt, func, false);
		} else if(el.attachEvent) {
			el.attachEvent('on'+evnt, func);
		}
	},


	fireEvent : function(el, evnt) {
		if(!el) {
			return;
		}
		if(document.createEventObject) {
			var ev = document.createEventObject();
			el.fireEvent('on'+evnt, ev);
		} else if(document.createEvent) {
			var ev = document.createEvent('HTMLEvents');
			ev.initEvent(evnt, true, true);
			el.dispatchEvent(ev);
		} else if(el['on'+evnt]) { // alternatively use the traditional event model (IE5)
			el['on'+evnt]();
		}
	},


	getElementPos : function(e) {
		var e1=e, e2=e;
		var x=0, y=0;
		if(e1.offsetParent) {
			do {
				x += e1.offsetLeft;
				y += e1.offsetTop;
			} while(e1 = e1.offsetParent);
		}
		while((e2 = e2.parentNode) && e2.nodeName.toUpperCase() !== 'BODY') {
			x -= e2.scrollLeft;
			y -= e2.scrollTop;
		}
		return [x, y];
	},


	getElementSize : function(e) {
		return [e.offsetWidth, e.offsetHeight];
	},


	getMousePos : function(e) {
		if(!e) { e = window.event; }
		if(typeof e.pageX === 'number') {
			return [e.pageX, e.pageY];
		} else if(typeof e.clientX === 'number') {
			return [
				e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft,
				e.clientY + document.body.scrollTop + document.documentElement.scrollTop
			];
		}
	},


	getViewPos : function() {
		if(typeof window.pageYOffset === 'number') {
			return [window.pageXOffset, window.pageYOffset];
		} else if(document.body && (document.body.scrollLeft || document.body.scrollTop)) {
			return [document.body.scrollLeft, document.body.scrollTop];
		} else if(document.documentElement && (document.documentElement.scrollLeft || document.documentElement.scrollTop)) {
			return [document.documentElement.scrollLeft, document.documentElement.scrollTop];
		} else {
			return [0, 0];
		}
	},


	getViewSize : function() {
		if(typeof window.innerWidth === 'number') {
			return [window.innerWidth, window.innerHeight];
		} else if(document.body && (document.body.clientWidth || document.body.clientHeight)) {
			return [document.body.clientWidth, document.body.clientHeight];
		} else if(document.documentElement && (document.documentElement.clientWidth || document.documentElement.clientHeight)) {
			return [document.documentElement.clientWidth, document.documentElement.clientHeight];
		} else {
			return [0, 0];
		}
	},


	URI : function(uri) { // See RFC3986

		this.scheme = null;
		this.authority = null;
		this.path = '';
		this.query = null;
		this.fragment = null;

		this.parse = function(uri) {
			var m = uri.match(/^(([A-Za-z][0-9A-Za-z+.-]*)(:))?((\/\/)([^\/?#]*))?([^?#]*)((\?)([^#]*))?((#)(.*))?/);
			this.scheme = m[3] ? m[2] : null;
			this.authority = m[5] ? m[6] : null;
			this.path = m[7];
			this.query = m[9] ? m[10] : null;
			this.fragment = m[12] ? m[13] : null;
			return this;
		};

		this.toString = function() {
			var result = '';
			if(this.scheme !== null) { result = result + this.scheme + ':'; }
			if(this.authority !== null) { result = result + '//' + this.authority; }
			if(this.path !== null) { result = result + this.path; }
			if(this.query !== null) { result = result + '?' + this.query; }
			if(this.fragment !== null) { result = result + '#' + this.fragment; }
			return result;
		};

		this.toAbsolute = function(base) {
			var base = new jscolor.URI(base);
			var r = this;
			var t = new jscolor.URI;

			if(base.scheme === null) { return false; }

			if(r.scheme !== null && r.scheme.toLowerCase() === base.scheme.toLowerCase()) {
				r.scheme = null;
			}

			if(r.scheme !== null) {
				t.scheme = r.scheme;
				t.authority = r.authority;
				t.path = removeDotSegments(r.path);
				t.query = r.query;
			} else {
				if(r.authority !== null) {
					t.authority = r.authority;
					t.path = removeDotSegments(r.path);
					t.query = r.query;
				} else {
					if(r.path === '') { // TODO: == or === ?
						t.path = base.path;
						if(r.query !== null) {
							t.query = r.query;
						} else {
							t.query = base.query;
						}
					} else {
						if(r.path.substr(0,1) === '/') {
							t.path = removeDotSegments(r.path);
						} else {
							if(base.authority !== null && base.path === '') { // TODO: == or === ?
								t.path = '/'+r.path;
							} else {
								t.path = base.path.replace(/[^\/]+$/,'')+r.path;
							}
							t.path = removeDotSegments(t.path);
						}
						t.query = r.query;
					}
					t.authority = base.authority;
				}
				t.scheme = base.scheme;
			}
			t.fragment = r.fragment;

			return t;
		};

		function removeDotSegments(path) {
			var out = '';
			while(path) {
				if(path.substr(0,3)==='../' || path.substr(0,2)==='./') {
					path = path.replace(/^\.+/,'').substr(1);
				} else if(path.substr(0,3)==='/./' || path==='/.') {
					path = '/'+path.substr(3);
				} else if(path.substr(0,4)==='/../' || path==='/..') {
					path = '/'+path.substr(4);
					out = out.replace(/\/?[^\/]*$/, '');
				} else if(path==='.' || path==='..') {
					path = '';
				} else {
					var rm = path.match(/^\/?[^\/]*/)[0];
					path = path.substr(rm.length);
					out = out + rm;
				}
			}
			return out;
		}

		if(uri) {
			this.parse(uri);
		}

	},


	/*
	 * Usage example:
	 * var myColor = new jscolor.color(myInputElement)
	 */

	color : function(target, prop) {


		this.required = true; // refuse empty values?
		this.adjust = true; // adjust value to uniform notation?
		this.hash = false; // prefix color with # symbol?
		this.caps = true; // uppercase?
		this.valueElement = target; // value holder
		this.styleElement = target; // where to reflect current color
		this.hsv = [0, 0, 1]; // read-only  0-6, 0-1, 0-1
		this.rgb = [1, 1, 1]; // read-only  0-1, 0-1, 0-1

		this.pickerOnfocus = true; // display picker on focus?
		this.pickerMode = 'HSV'; // HSV | HVS
		this.pickerPosition = 'bottom'; // left | right | top | bottom
		this.pickerFace = 10; // px
		this.pickerFaceColor = 'ThreeDFace'; // CSS color
		this.pickerBorder = 1; // px
		this.pickerBorderColor = 'ThreeDHighlight ThreeDShadow ThreeDShadow ThreeDHighlight'; // CSS color
		this.pickerInset = 1; // px
		this.pickerInsetColor = 'ThreeDShadow ThreeDHighlight ThreeDHighlight ThreeDShadow'; // CSS color
		this.pickerZIndex = 10000;


		for(var p in prop) {
			if(prop.hasOwnProperty(p)) {
				this[p] = prop[p];
			}
		}


		this.hidePicker = function() {
			if(isPickerOwner()) {
				removePicker();
			}
		};


		this.showPicker = function() {
			if(!isPickerOwner()) {
				var tp = jscolor.getElementPos(target); // target pos
				var ts = jscolor.getElementSize(target); // target size
				var vp = jscolor.getViewPos(); // view pos
				var vs = jscolor.getViewSize(); // view size
				var ps = [ // picker size
					2*this.pickerBorder + 4*this.pickerInset + 2*this.pickerFace + jscolor.images.pad[0] + 2*jscolor.images.arrow[0] + jscolor.images.sld[0],
					2*this.pickerBorder + 2*this.pickerInset + 2*this.pickerFace + jscolor.images.pad[1]
				];
				var a, b, c;
				switch(this.pickerPosition.toLowerCase()) {
					case 'left': a=1; b=0; c=-1; break;
					case 'right':a=1; b=0; c=1; break;
					case 'top':  a=0; b=1; c=-1; break;
					default:     a=0; b=1; c=1; break;
				}
				var l = (ts[b]+ps[b])/2;
				var pp = [ // picker pos
					-vp[a]+tp[a]+ps[a] > vs[a] ?
						(-vp[a]+tp[a]+ts[a]/2 > vs[a]/2 && tp[a]+ts[a]-ps[a] >= 0 ? tp[a]+ts[a]-ps[a] : tp[a]) :
						tp[a],
					-vp[b]+tp[b]+ts[b]+ps[b]-l+l*c > vs[b] ?
						(-vp[b]+tp[b]+ts[b]/2 > vs[b]/2 && tp[b]+ts[b]-l-l*c >= 0 ? tp[b]+ts[b]-l-l*c : tp[b]+ts[b]-l+l*c) :
						(tp[b]+ts[b]-l+l*c >= 0 ? tp[b]+ts[b]-l+l*c : tp[b]+ts[b]-l-l*c)
				];
				drawPicker(pp[a], pp[b]);
			}
		};


		this.importColor = function() {
			if(!valueElement) {
				this.exportColor();
			} else {
				if(!this.adjust) {
					if(!this.fromString(valueElement.value, leaveValue)) {
						styleElement.style.backgroundColor = styleElement.jscStyle.backgroundColor;
						styleElement.style.color = styleElement.jscStyle.color;
						this.exportColor(leaveValue | leaveStyle);
					}
				} else if(!this.required && /^\s*$/.test(valueElement.value)) {
					valueElement.value = '';
					styleElement.style.backgroundColor = styleElement.jscStyle.backgroundColor;
					styleElement.style.color = styleElement.jscStyle.color;
					this.exportColor(leaveValue | leaveStyle);

				} else if(this.fromString(valueElement.value)) {
					// OK
				} else {
					this.exportColor();
				}
			}
		};


		this.exportColor = function(flags) {
			if(!(flags & leaveValue) && valueElement) {
				var value = this.toString();
				if(this.caps) { value = value.toUpperCase(); }
				if(this.hash) { value = '#'+value; }
				valueElement.value = value;
			}
			if(!(flags & leaveStyle) && styleElement) {
				styleElement.style.backgroundColor =
					'#'+this.toString();
				styleElement.style.color =
					0.213 * this.rgb[0] +
					0.715 * this.rgb[1] +
					0.072 * this.rgb[2]
					< 0.5 ? '#FFF' : '#000';
			}
			if(!(flags & leavePad) && isPickerOwner()) {
				redrawPad();
			}
			if(!(flags & leaveSld) && isPickerOwner()) {
				redrawSld();
			}
		};


		this.fromHSV = function(h, s, v, flags) { // null = don't change
			h<0 && (h=0) || h>6 && (h=6);
			s<0 && (s=0) || s>1 && (s=1);
			v<0 && (v=0) || v>1 && (v=1);
			this.rgb = HSV_RGB(
				h===null ? this.hsv[0] : (this.hsv[0]=h),
				s===null ? this.hsv[1] : (this.hsv[1]=s),
				v===null ? this.hsv[2] : (this.hsv[2]=v)
			);
			this.exportColor(flags);
		};


		this.fromRGB = function(r, g, b, flags) { // null = don't change
			r<0 && (r=0) || r>1 && (r=1);
			g<0 && (g=0) || g>1 && (g=1);
			b<0 && (b=0) || b>1 && (b=1);
			var hsv = RGB_HSV(
				r===null ? this.rgb[0] : (this.rgb[0]=r),
				g===null ? this.rgb[1] : (this.rgb[1]=g),
				b===null ? this.rgb[2] : (this.rgb[2]=b)
			);
			if(hsv[0] !== null) {
				this.hsv[0] = hsv[0];
			}
			if(hsv[2] !== 0) {
				this.hsv[1] = hsv[1];
			}
			this.hsv[2] = hsv[2];
			this.exportColor(flags);
		};


		this.fromString = function(hex, flags) {
			var m = hex.match(/^\W*([0-9A-F]{3}([0-9A-F]{3})?)\W*$/i);
			if(!m) {
				return false;
			} else {
				if(m[1].length === 6) { // 6-char notation
					this.fromRGB(
						parseInt(m[1].substr(0,2),16) / 255,
						parseInt(m[1].substr(2,2),16) / 255,
						parseInt(m[1].substr(4,2),16) / 255,
						flags
					);
				} else { // 3-char notation
					this.fromRGB(
						parseInt(m[1].charAt(0)+m[1].charAt(0),16) / 255,
						parseInt(m[1].charAt(1)+m[1].charAt(1),16) / 255,
						parseInt(m[1].charAt(2)+m[1].charAt(2),16) / 255,
						flags
					);
				}
				return true;
			}
		};


		this.toString = function() {
			return (
				(0x100 | Math.round(255*this.rgb[0])).toString(16).substr(1) +
				(0x100 | Math.round(255*this.rgb[1])).toString(16).substr(1) +
				(0x100 | Math.round(255*this.rgb[2])).toString(16).substr(1)
			);
		};


		function RGB_HSV(r, g, b) {
			var n = Math.min(Math.min(r,g),b);
			var v = Math.max(Math.max(r,g),b);
			var m = v - n;
			if(m === 0) { return [ null, 0, v ]; }
			var h = r===n ? 3+(b-g)/m : (g===n ? 5+(r-b)/m : 1+(g-r)/m);
			return [ h===6?0:h, m/v, v ];
		}


		function HSV_RGB(h, s, v) {
			if(h === null) { return [ v, v, v ]; }
			var i = Math.floor(h);
			var f = i%2 ? h-i : 1-(h-i);
			var m = v * (1 - s);
			var n = v * (1 - s*f);
			switch(i) {
				case 6:
				case 0: return [v,n,m];
				case 1: return [n,v,m];
				case 2: return [m,v,n];
				case 3: return [m,n,v];
				case 4: return [n,m,v];
				case 5: return [v,m,n];
			}
		}


		function removePicker() {
			delete jscolor.picker.owner;
			document.getElementsByTagName('body')[0].removeChild(jscolor.picker.boxB);
		}


		function drawPicker(x, y) {
			if(!jscolor.picker) {
				jscolor.picker = {
					box : document.createElement('div'),
					boxB : document.createElement('div'),
					pad : document.createElement('div'),
					padB : document.createElement('div'),
					padM : document.createElement('div'),
					sld : document.createElement('div'),
					sldB : document.createElement('div'),
					sldM : document.createElement('div')
				};
				for(var i=0,segSize=4; i<jscolor.images.sld[1]; i+=segSize) {
					var seg = document.createElement('div');
					seg.style.height = segSize+'px';
					seg.style.fontSize = '1px';
					seg.style.lineHeight = '0';
					jscolor.picker.sld.appendChild(seg);
				}
				jscolor.picker.sldB.appendChild(jscolor.picker.sld);
				jscolor.picker.box.appendChild(jscolor.picker.sldB);
				jscolor.picker.box.appendChild(jscolor.picker.sldM);
				jscolor.picker.padB.appendChild(jscolor.picker.pad);
				jscolor.picker.box.appendChild(jscolor.picker.padB);
				jscolor.picker.box.appendChild(jscolor.picker.padM);
				jscolor.picker.boxB.appendChild(jscolor.picker.box);
			}

			var p = jscolor.picker;

			// recompute controls positions
			posPad = [
				x+THIS.pickerBorder+THIS.pickerFace+THIS.pickerInset,
				y+THIS.pickerBorder+THIS.pickerFace+THIS.pickerInset ];
			posSld = [
				null,
				y+THIS.pickerBorder+THIS.pickerFace+THIS.pickerInset ];

			// controls interaction
			p.box.onmouseup =
			p.box.onmouseout = function() { target.focus(); };
			p.box.onmousedown = function() { abortBlur=true; };
			p.box.onmousemove = function(e) { holdPad && setPad(e); holdSld && setSld(e); };
			p.padM.onmouseup =
			p.padM.onmouseout = function() { if(holdPad) { holdPad=false; jscolor.fireEvent(valueElement,'change'); } };
			p.padM.onmousedown = function(e) { holdPad=true; setPad(e); };
			p.sldM.onmouseup =
			p.sldM.onmouseout = function() { if(holdSld) { holdSld=false; jscolor.fireEvent(valueElement,'change'); } };
			p.sldM.onmousedown = function(e) { holdSld=true; setSld(e); };

			// picker
			p.box.style.width = 4*THIS.pickerInset + 2*THIS.pickerFace + jscolor.images.pad[0] + 2*jscolor.images.arrow[0] + jscolor.images.sld[0] + 'px';
			p.box.style.height = 2*THIS.pickerInset + 2*THIS.pickerFace + jscolor.images.pad[1] + 'px';

			// picker border
			p.boxB.style.position = 'absolute';
			p.boxB.style.clear = 'both';
			p.boxB.style.left = x+'px';
			p.boxB.style.top = y+'px';
			p.boxB.style.zIndex = THIS.pickerZIndex;
			p.boxB.style.border = THIS.pickerBorder+'px solid';
			p.boxB.style.borderColor = THIS.pickerBorderColor;
			p.boxB.style.background = THIS.pickerFaceColor;

			// pad image
			p.pad.style.width = jscolor.images.pad[0]+'px';
			p.pad.style.height = jscolor.images.pad[1]+'px';

			// pad border
			p.padB.style.position = 'absolute';
			p.padB.style.left = THIS.pickerFace+'px';
			p.padB.style.top = THIS.pickerFace+'px';
			p.padB.style.border = THIS.pickerInset+'px solid';
			p.padB.style.borderColor = THIS.pickerInsetColor;

			// pad mouse area
			p.padM.style.position = 'absolute';
			p.padM.style.left = '0';
			p.padM.style.top = '0';
			p.padM.style.width = THIS.pickerFace + 2*THIS.pickerInset + jscolor.images.pad[0] + jscolor.images.arrow[0] + 'px';
			p.padM.style.height = p.box.style.height;
			p.padM.style.cursor = 'crosshair';

			// slider image
			p.sld.style.overflow = 'hidden';
			p.sld.style.width = jscolor.images.sld[0]+'px';
			p.sld.style.height = jscolor.images.sld[1]+'px';

			// slider border
			p.sldB.style.position = 'absolute';
			p.sldB.style.right = THIS.pickerFace+'px';
			p.sldB.style.top = THIS.pickerFace+'px';
			p.sldB.style.border = THIS.pickerInset+'px solid';
			p.sldB.style.borderColor = THIS.pickerInsetColor;

			// slider mouse area
			p.sldM.style.position = 'absolute';
			p.sldM.style.right = '0';
			p.sldM.style.top = '0';
			p.sldM.style.width = jscolor.images.sld[0] + jscolor.images.arrow[0] + THIS.pickerFace + 2*THIS.pickerInset + 'px';
			p.sldM.style.height = p.box.style.height;
			try {
				p.sldM.style.cursor = 'pointer';
			} catch(eOldIE) {
				p.sldM.style.cursor = 'hand';
			}

			// load images in optimal order
			switch(modeID) {
				case 0: var padImg = 'hs.png'; break;
				case 1: var padImg = 'hv.png'; break;
			}
			p.padM.style.background = "url('"+jscolor.getDir()+"cross.gif') no-repeat";
			p.sldM.style.background = "url('"+jscolor.getDir()+"arrow.gif') no-repeat";
			p.pad.style.background = "url('"+jscolor.getDir()+padImg+"') 0 0 no-repeat";

			// place pointers
			redrawPad();
			redrawSld();

			jscolor.picker.owner = THIS;
			document.getElementsByTagName('body')[0].appendChild(p.boxB);
		}


		function redrawPad() {
			// redraw the pad pointer
			switch(modeID) {
				case 0: var yComponent = 1; break;
				case 1: var yComponent = 2; break;
			}
			var x = Math.round((THIS.hsv[0]/6) * (jscolor.images.pad[0]-1));
			var y = Math.round((1-THIS.hsv[yComponent]) * (jscolor.images.pad[1]-1));
			jscolor.picker.padM.style.backgroundPosition =
				(THIS.pickerFace+THIS.pickerInset+x - Math.floor(jscolor.images.cross[0]/2)) + 'px ' +
				(THIS.pickerFace+THIS.pickerInset+y - Math.floor(jscolor.images.cross[1]/2)) + 'px';

			// redraw the slider image
			var seg = jscolor.picker.sld.childNodes;

			switch(modeID) {
				case 0:
					var rgb = HSV_RGB(THIS.hsv[0], THIS.hsv[1], 1);
					for(var i=0; i<seg.length; i+=1) {
						seg[i].style.backgroundColor = 'rgb('+
							(rgb[0]*(1-i/seg.length)*100)+'%,'+
							(rgb[1]*(1-i/seg.length)*100)+'%,'+
							(rgb[2]*(1-i/seg.length)*100)+'%)';
					}
					break;
				case 1:
					var rgb, s, c = [ THIS.hsv[2], 0, 0 ];
					var i = Math.floor(THIS.hsv[0]);
					var f = i%2 ? THIS.hsv[0]-i : 1-(THIS.hsv[0]-i);
					switch(i) {
						case 6:
						case 0: rgb=[0,1,2]; break;
						case 1: rgb=[1,0,2]; break;
						case 2: rgb=[2,0,1]; break;
						case 3: rgb=[2,1,0]; break;
						case 4: rgb=[1,2,0]; break;
						case 5: rgb=[0,2,1]; break;
					}
					for(var i=0; i<seg.length; i+=1) {
						s = 1 - 1/(seg.length-1)*i;
						c[1] = c[0] * (1 - s*f);
						c[2] = c[0] * (1 - s);
						seg[i].style.backgroundColor = 'rgb('+
							(c[rgb[0]]*100)+'%,'+
							(c[rgb[1]]*100)+'%,'+
							(c[rgb[2]]*100)+'%)';
					}
					break;
			}
		}


		function redrawSld() {
			// redraw the slider pointer
			switch(modeID) {
				case 0: var yComponent = 2; break;
				case 1: var yComponent = 1; break;
			}
			var y = Math.round((1-THIS.hsv[yComponent]) * (jscolor.images.sld[1]-1));
			jscolor.picker.sldM.style.backgroundPosition =
				'0 ' + (THIS.pickerFace+THIS.pickerInset+y - Math.floor(jscolor.images.arrow[1]/2)) + 'px';
		}


		function isPickerOwner() {
			return jscolor.picker && jscolor.picker.owner === THIS;
		}


		function blurTarget() {
			if(valueElement === target) {
				THIS.importColor();
			}
			if(THIS.pickerOnfocus) {
				THIS.hidePicker();
			}
		}


		function blurValue() {
			if(valueElement !== target) {
				THIS.importColor();
			}
		}


		function setPad(e) {
			var posM = jscolor.getMousePos(e);
			var x = posM[0]-posPad[0];
			var y = posM[1]-posPad[1];
			switch(modeID) {
				case 0: THIS.fromHSV(x*(6/(jscolor.images.pad[0]-1)), 1 - y/(jscolor.images.pad[1]-1), null, leaveSld); break;
				case 1: THIS.fromHSV(x*(6/(jscolor.images.pad[0]-1)), null, 1 - y/(jscolor.images.pad[1]-1), leaveSld); break;
			}
		}


		function setSld(e) {
			var posM = jscolor.getMousePos(e);
			var y = posM[1]-posPad[1];
			switch(modeID) {
				case 0: THIS.fromHSV(null, null, 1 - y/(jscolor.images.sld[1]-1), leavePad); break;
				case 1: THIS.fromHSV(null, 1 - y/(jscolor.images.sld[1]-1), null, leavePad); break;
			}
		}


		var THIS = this;
		var modeID = this.pickerMode.toLowerCase()==='hvs' ? 1 : 0;
		var abortBlur = false;
		var
			valueElement = jscolor.fetchElement(this.valueElement),
			styleElement = jscolor.fetchElement(this.styleElement);
		var
			holdPad = false,
			holdSld = false;
		var
			posPad,
			posSld;
		var
			leaveValue = 1<<0,
			leaveStyle = 1<<1,
			leavePad = 1<<2,
			leaveSld = 1<<3;

		// target
		jscolor.addEvent(target, 'focus', function() {
			if(THIS.pickerOnfocus) { THIS.showPicker(); }
		});
		jscolor.addEvent(target, 'blur', function() {
			if(!abortBlur) {
				window.setTimeout(function(){ abortBlur || blurTarget(); abortBlur=false; }, 0);
			} else {
				abortBlur = false;
			}
		});

		// valueElement
		if(valueElement) {
			var updateField = function() {
				THIS.fromString(valueElement.value, leaveValue);
			};
			jscolor.addEvent(valueElement, 'keyup', updateField);
			jscolor.addEvent(valueElement, 'input', updateField);
			jscolor.addEvent(valueElement, 'blur', blurValue);
			valueElement.setAttribute('autocomplete', 'off');
		}

		// styleElement
		if(styleElement) {
			styleElement.jscStyle = {
				backgroundColor : styleElement.style.backgroundColor,
				color : styleElement.style.color
			};
		}

		// require images
		switch(modeID) {
			case 0: jscolor.requireImage('hs.png'); break;
			case 1: jscolor.requireImage('hv.png'); break;
		}
		jscolor.requireImage('cross.gif');
		jscolor.requireImage('arrow.gif');

		this.importColor();
	}

};


jscolor.install();

/**
 * Copyright 2009 Tim Down.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * jshashtable
 *
 * jshashtable is a JavaScript implementation of a hash table. It creates a
 * single constructor function called Hashtable in the global scope.
 *
 * Author: Tim Down <tim@timdown.co.uk>
 * Version: 1.0
 * Build date: 5 February 2009
 * Website: http://www.timdown.co.uk/jshashtable
 */

var Hashtable = (function() {
	function isUndefined(obj) {
		return (typeof obj === "undefined");
	}

	function isFunction(obj) {
		return (typeof obj === "function");
	}

	function isString(obj) {
		return (typeof obj === "string");
	}

	function hasMethod(obj, methodName) {
		return isFunction(obj[methodName]);
	}
	
	function hasEquals(obj) {
		return hasMethod(obj, "equals");
	}

	function hasHashCode(obj) {
		return hasMethod(obj, "hashCode");
	}

	function keyForObject(obj) {
		if (isString(obj)) {
			return obj;
		} else if (hasHashCode(obj)) {
			// Check the hashCode method really has returned a string
			var hashCode = obj.hashCode();
			if (!isString(hashCode)) {
				return keyForObject(hashCode);
			}
			return hashCode;
		} else if (hasMethod(obj, "toString")) {
			return obj.toString();
		} else {
			return String(obj);
		}
	}

	function equals_fixedValueHasEquals(fixedValue, variableValue) {
		return fixedValue.equals(variableValue);
	}

	function equals_fixedValueNoEquals(fixedValue, variableValue) {
		if (hasEquals(variableValue)) {
			return variableValue.equals(fixedValue);
		} else {
			return fixedValue === variableValue;
		}
	}

	function equals_equivalence(o1, o2) {
		return o1 === o2;
	}

	function arraySearch(arr, value, arrayValueFunction, returnFoundItem, equalityFunction) {
		var currentValue;
		for (var i = 0, len = arr.length; i < len; i++) {
			currentValue = arr[i];
			if (equalityFunction(value, arrayValueFunction(currentValue))) {
				return returnFoundItem ? [i, currentValue] : true;
			}
		}
		return false;
	}

	function arrayRemoveAt(arr, idx) {
		if (hasMethod(arr, "splice")) {
			arr.splice(idx, 1);
		} else {
			if (idx === arr.length - 1) {
				arr.length = idx;
			} else {
				var itemsAfterDeleted = arr.slice(idx + 1);
				arr.length = idx;
				for (var i = 0, len = itemsAfterDeleted.length; i < len; i++) {
					arr[idx + i] = itemsAfterDeleted[i];
				}
			}
		}
	}

	function checkKeyOrValue(kv, kvStr) {
		if (kv === null) {
			throw new Error("null is not a valid " + kvStr);
		} else if (isUndefined(kv)) {
			throw new Error(kvStr + " must not be undefined");
		}
	}

	var keyStr = "key", valueStr = "value";	

	function checkKey(key) {
		checkKeyOrValue(key, keyStr);
	}

	function checkValue(value) {
		checkKeyOrValue(value, valueStr);
	}

	/*------------------------------------------------------------------------*/	

	function Bucket(firstKey, firstValue, equalityFunction) {
		this.entries = [];
		this.addEntry(firstKey, firstValue);
		
		if (equalityFunction !== null) {
			this.getEqualityFunction = function() {
				return equalityFunction;
			};
		}
	}

	function getBucketEntryKey(entry) {
		return entry[0];
	}

	function getBucketEntryValue(entry) {
		return entry[1];
	}

	Bucket.prototype = {
		getEqualityFunction: function(searchValue) {
			if (hasEquals(searchValue)) {
				return equals_fixedValueHasEquals;
			} else {
				return equals_fixedValueNoEquals;
			}
		},

		searchForEntry: function(key) {
			return arraySearch(this.entries, key, getBucketEntryKey, true, this.getEqualityFunction(key));
		},

		getEntryForKey: function(key) {
			return this.searchForEntry(key)[1];
		},

		getEntryIndexForKey: function(key) {
			return this.searchForEntry(key)[0];
		},
		
		removeEntryForKey: function(key) {
			var result = this.searchForEntry(key);
			if (result) {
				arrayRemoveAt(this.entries, result[0]);
				return true;
			}
			return false;
		},

		addEntry: function(key, value) {
			this.entries[this.entries.length] = [key, value];
		},

		size: function() {
			return this.entries.length;
		},

		keys: function(keys) {
			var startIndex = keys.length;
			for (var i = 0, len = this.entries.length; i < len; i++) {
				keys[startIndex + i] = this.entries[i][0];
			}
		},

		values: function(values) {
			var startIndex = values.length;
			for (var i = 0, len = this.entries.length; i < len; i++) {
				values[startIndex + i] = this.entries[i][1];
			}
		},

		containsKey: function(key) {
			return arraySearch(this.entries, key, getBucketEntryKey, false, this.getEqualityFunction(key));
		},

		containsValue: function(value) {
			return arraySearch(this.entries, value, getBucketEntryValue, false, equals_equivalence);
		}
	};

	/*------------------------------------------------------------------------*/	

	function BucketItem() {}
	BucketItem.prototype = [];

	// Supporting functions for searching hashtable bucket items

	function getBucketKeyFromBucketItem(bucketItem) {
		return bucketItem[0];
	}

	function searchBucketItems(bucketItems, bucketKey, equalityFunction) {
		return arraySearch(bucketItems, bucketKey, getBucketKeyFromBucketItem, true, equalityFunction);
	}

	function getBucketForBucketKey(bucketItemsByBucketKey, bucketKey) {
		var bucketItem = bucketItemsByBucketKey[bucketKey];

		// Check that this is a genuine bucket item and not something
		// inherited from prototype
		if (bucketItem && (bucketItem instanceof BucketItem)) {
			return bucketItem[1];
		}
		return null;
	}
	
	/*------------------------------------------------------------------------*/	
	
	function Hashtable(hashingFunction, equalityFunction) {
		var bucketItems = [];
		var bucketItemsByBucketKey = {};

		hashingFunction = isFunction(hashingFunction) ? hashingFunction : keyForObject;
		equalityFunction = isFunction(equalityFunction) ? equalityFunction : null;

		this.put = function(key, value) {
			checkKey(key);
			checkValue(value);
			var bucketKey = hashingFunction(key);

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);
			if (bucket) {
				// Check this bucket to see if it already contains this key
				var bucketEntry = bucket.getEntryForKey(key);
				if (bucketEntry) {
					// This bucket entry is the current mapping of key to value, so replace
					// old value and we're done.
					bucketEntry[1] = value;
				} else {
					// The bucket does not contain an entry for this key, so add one
					bucket.addEntry(key, value);
				}
			} else {
				// No bucket, so create one and put our key/value mapping in
				var bucketItem = new BucketItem();
				bucketItem[0] = bucketKey;
				bucketItem[1] = new Bucket(key, value, equalityFunction);
				bucketItems[bucketItems.length] = bucketItem;
				bucketItemsByBucketKey[bucketKey] = bucketItem;
			}
		};

		this.get = function(key) {
			checkKey(key);

			var bucketKey = hashingFunction(key);

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);
			if (bucket) {
				// Check this bucket to see if it contains this key
				var bucketEntry = bucket.getEntryForKey(key);
				if (bucketEntry) {
					// This bucket entry is the current mapping of key to value, so return
					// the value.
					return bucketEntry[1];
				}
			}
			return null;
		};

		this.containsKey = function(key) {
			checkKey(key);

			var bucketKey = hashingFunction(key);

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);
			if (bucket) {
				return bucket.containsKey(key);
			}

			return false;
		};

		this.containsValue = function(value) {
			checkValue(value);
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				if (bucketItems[i][1].containsValue(value)) {
					return true;
				}
			}
			return false;
		};

		this.clear = function() {
			bucketItems.length = 0;
			bucketItemsByBucketKey = {};
		};

		this.isEmpty = function() {
			return bucketItems.length === 0;
		};

		this.keys = function() {
			var keys = [];
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				bucketItems[i][1].keys(keys);
			}
			return keys;
		};

		this.values = function() {
			var values = [];
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				bucketItems[i][1].values(values);
			}
			return values;
		};

		this.remove = function(key) {
			checkKey(key);

			var bucketKey = hashingFunction(key);

			// Check if a bucket exists for the bucket key
			var bucket = getBucketForBucketKey(bucketItemsByBucketKey, bucketKey);

			if (bucket) {
				// Remove entry from this bucket for this key
				if (bucket.removeEntryForKey(key)) {
					// Entry was removed, so check if bucket is empty
					if (bucket.size() === 0) {
						// Bucket is empty, so remove it
						var result = searchBucketItems(bucketItems, bucketKey, bucket.getEqualityFunction(key));
						arrayRemoveAt(bucketItems, result[0]);
						delete bucketItemsByBucketKey[bucketKey];
					}
				}
			}
		};

		this.size = function() {
			var total = 0;
			for (var i = 0, len = bucketItems.length; i < len; i++) {
				total += bucketItems[i][1].size();
			}
			return total;
		};
	}

	return Hashtable;
})();
(function($) {
    $.toJSON = function(o, deepLevel) {
        var type = typeof (o);

        if (o === null)
            return "null";

        if (type == "undefined")
            return undefined;

        if (type == "number" || type == "boolean")
            return o + "";

        if (type == "string")
            return $.quoteString(o);

        if (type == 'object') {
            if (o.constructor === Date) {
                var month = o.getUTCMonth() + 1;
                if (month < 10) month = '0' + month;

                var day = o.getUTCDate();
                if (day < 10) day = '0' + day;

                var year = o.getUTCFullYear();

                var hours = o.getUTCHours();
                if (hours < 10) hours = '0' + hours;

                var minutes = o.getUTCMinutes();
                if (minutes < 10) minutes = '0' + minutes;

                var seconds = o.getUTCSeconds();
                if (seconds < 10) seconds = '0' + seconds;

                var milli = o.getUTCMilliseconds();
                if (milli < 100) milli = '0' + milli;
                if (milli < 10) milli = '0' + milli;

                return '"' + year + '-' + month + '-' + day + 'T' +
                             hours + ':' + minutes + ':' + seconds +
                             '.' + milli + 'Z"';
            }

            if (o.constructor === Array) {
                if (o.__tracking) {
                    if (isDefined(deepLevel) && deepLevel <= 0) {
                        return null;
                    }

                    return $.toJSON({
                        __type: o.__type,
                        added: o.added,
                        items: o.items,
                        removed: o.removed
                    }, isDefined(deepLevel) ? (deepLevel - 1) : undefined);
                }

                var ret = [];
                for (var i = 0; i < o.length; i++)
                    ret.push($.toJSON(o[i], deepLevel) || "null");

                return "[" + ret.join(",") + "]";
            }

            var pairs = [];
            var properties = [];

            for (propertyName in o) {
                if (propertyName != '__type') {
                    properties.add(propertyName);
                }
            }

            if (o['__type']) {
                properties.insert(0, '__type');
            }

            for (var i = 0; i < properties.length; i++) {
                var k = properties[i];
                if (k.startsWith('__') && k !== '__type')
                    continue;

                var name;
                var type = typeof k;

                if (type == "number")
                    name = '"' + k + '"';
                else if (type == "string")
                    name = $.quoteString(k);
                else
                    continue;  //skip non-string or number keys                

                if (typeof o[k] == "function")
                    continue;  //skip pairs where the value is a function.

                var val = $.toJSON(o[k], deepLevel);

                pairs.push(name + ":" + val);
            }

            return "{" + pairs.join(", ") + "}";
        }
    };

    $.evalJSON = function(src) {
        return eval("(" + src + ")");
    };

    $.quoteString = function(string) {
        if (string.startsWith("/Date")) {
            string = string.replace(new RegExp('/','g'), "\\/");
        }
        
        if (string.startsWith("\\/Date")) {
            return '"' + string + '"';
        }

        if (string.match(_escapeable)) {
            return '"' + string.replace(_escapeable, function(a) {
                var c = _meta[a];
                if (typeof c === 'string') return c;
                c = a.charCodeAt();
                return '\\u00' + Math.floor(c / 16).toString(16) + (c % 16).toString(16);
            }) + '"';
        }
        return '"' + string + '"';
    };

    var _escapeable = /["\\\x00-\x1f\x7f-\x9f]/g;

    var _meta = {
        '\b': '\\b',
        '\t': '\\t',
        '\n': '\\n',
        '\f': '\\f',
        '\r': '\\r',
        '"': '\\"',
        '\\': '\\\\'
    };
})(jQuery);
var Base64 = {
	// private property
	_keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",

	// public method for encoding
	encode : function (input) {
		var output = "";
		var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
		var i = 0;

		input = Base64._utf8_encode(input);

		while (i < input.length) {

			chr1 = input.charCodeAt(i++);
			chr2 = input.charCodeAt(i++);
			chr3 = input.charCodeAt(i++);

			enc1 = chr1 >> 2;
			enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
			enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
			enc4 = chr3 & 63;

			if (isNaN(chr2)) {
				enc3 = enc4 = 64;
			} else if (isNaN(chr3)) {
				enc4 = 64;
			}

			output = output +
			this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) +
			this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);

		}

		return output;
	},

	// public method for decoding
	decode : function (input) {
		var output = "";
		var chr1, chr2, chr3;
		var enc1, enc2, enc3, enc4;
		var i = 0;

		input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

		while (i < input.length) {

			enc1 = this._keyStr.indexOf(input.charAt(i++));
			enc2 = this._keyStr.indexOf(input.charAt(i++));
			enc3 = this._keyStr.indexOf(input.charAt(i++));
			enc4 = this._keyStr.indexOf(input.charAt(i++));

			chr1 = (enc1 << 2) | (enc2 >> 4);
			chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
			chr3 = ((enc3 & 3) << 6) | enc4;

			output = output + String.fromCharCode(chr1);

			if (enc3 != 64) {
				output = output + String.fromCharCode(chr2);
			}
			if (enc4 != 64) {
				output = output + String.fromCharCode(chr3);
			}

		}

		output = Base64._utf8_decode(output);

		return output;

	},

	// private method for UTF-8 encoding
	_utf8_encode : function (string) {
		string = string.replace(/\r\n/g,"\n");
		var utftext = "";

		for (var n = 0; n < string.length; n++) {

			var c = string.charCodeAt(n);

			if (c < 128) {
				utftext += String.fromCharCode(c);
			}
			else if((c > 127) && (c < 2048)) {
				utftext += String.fromCharCode((c >> 6) | 192);
				utftext += String.fromCharCode((c & 63) | 128);
			}
			else {
				utftext += String.fromCharCode((c >> 12) | 224);
				utftext += String.fromCharCode(((c >> 6) & 63) | 128);
				utftext += String.fromCharCode((c & 63) | 128);
			}

		}

		return utftext;
	},

	// private method for UTF-8 decoding
	_utf8_decode : function (utftext) {
		var string = "";
		var i = 0;
		var c = c1 = c2 = 0;

		while ( i < utftext.length ) {

			c = utftext.charCodeAt(i);

			if (c < 128) {
				string += String.fromCharCode(c);
				i++;
			}
			else if((c > 191) && (c < 224)) {
				c2 = utftext.charCodeAt(i+1);
				string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
				i += 2;
			}
			else {
				c2 = utftext.charCodeAt(i+1);
				c3 = utftext.charCodeAt(i+2);
				string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
				i += 3;
			}

		}

		return string;
	}
}
var Debug = {
    getCallStack: function() {
        var callstack = [];
        
        var isCallstackPopulated = false;
        
        try {
            i.dont.exist += 0; //doesn't exist- that's the point
        } catch (e) {
            if (e.stack) { //Firefox
                var lines = e.stack.split("\n");
                for (var i = 0, len = lines.length; i < len; i++) {
                    if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
                        callstack.push(lines[i]);
                    }
                }
                //Remove call to printStackTrace()
                callstack.shift();
                isCallstackPopulated = true;
            }
            else if (window.opera && e.message) { //Opera
                var lines = e.message.split("\n");
                for (var i = 0, len = lines.length; i < len; i++) {
                    if (lines[i].match(/^\s*[A-Za-z0-9\-_\$]+\(/)) {
                        var entry = lines[i];
                        //Append next line also since it has the file info
                        if (lines[i + 1]) {
                            entry += " at " + lines[i + 1];
                            i++;
                        }
                        callstack.push(entry);
                    }
                }
                //Remove call to printStackTrace()
                callstack.shift();
                isCallstackPopulated = true;
            }
        }
        if (!isCallstackPopulated) { //IE and Safari
            var currentFunction = arguments.callee.caller;
            while (currentFunction) {
                var fn = currentFunction.toString();
                var fname = fn.substring(fn.indexOf("function") + 8, fn.indexOf("(")) || "anonymous";
                callstack.push(fname);
                currentFunction = currentFunction.caller;
            }
        }
        
        return callstack;
    },
    
    getCallStackRoot: function() {
        return this.getCallStack().pop();
    }
};

Debug.startTracking = function(obj, breakFunctions, showArgsForFunctions) {
    var closure = function(p, breakpoint, showArgs) {
        return function() {
            var args = Array.prototype.slice.call(arguments || []);
            
            if (breakpoint) {
                console.log('Breakpoint for the function "' + p + '"');
                debugger;
            }
            
            var result = arguments.callee._old.apply(this, args);

            if (showArgs) {
                if (args.length > 0) {
                    console.log('Function "' + p + '" called for ', obj, 'with args: ', args, '. Result: ', result);
                } else {
                    console.log('Function "' + p + '" called for ', obj,' Result: ', result);
                }
            } else {
                    console.log('Function "' + p + '" called');
            }

            return result;
        }
    };

    for (var prop in obj) {
        if (typeof(obj[prop]) == 'function' && prop != 'toString') {
            var oldFunc = obj[prop];
            
            obj[prop] = closure(prop, breakFunctions && breakFunctions.contains(prop), showArgsForFunctions && showArgsForFunctions.contains(prop));
            obj[prop]._old = oldFunc;

            for (var oldProp in oldFunc) {
                obj[prop][oldProp] = oldFunc[oldProp];
            }
        }
    }
};

Debug.stopTracking = function(obj) {
    for (var prop in obj) {
        if (typeof(obj[prop]) == 'function' && prop != 'toString') {
            var oldFunc = obj[prop]._old;
            obj[prop] = oldFunc;
        }
    }
};

if (!window.console) {
    window.console = {
        log: function() {},
        error: function() {},
        info: function() {},
        warn: function() {},
        group: function() {},
        groupEnd: function() {}
    };
}

if (!console.info) {
    console.info = function() {};
}

if (!console.warn) {
    console.warn = function() {};
}

if (!console.error) {
    console.error = function() {};
}

Error.prototype.toString = function() {
    return this.name + '\r\n' + this.message + '\r\n' + (this.description || '');
}
var isFunction = function(o) {
    return typeof(o) === 'function';
};

Function.prototype.bind = function bind(obj) {
    var func = this;

    return function Function$bind() {
        var args = Array.prototype.slice.call(arguments || []);

        return func.apply(obj, args);
    };
};

Function.prototype.after = function(target) {
    var func = this;    
    
    return function() {
        var args = Array.prototype.slice.call(arguments || []);
    
        target.apply(arguments.caller, args);
        return func.apply(arguments.caller, args);
    };
};

Function.prototype.before = function(target) {
    var func = this;    
    
    return function() {
        var args = Array.prototype.slice.call(arguments || []);
    
        result = func.apply(arguments.caller, args);        
        target.apply(arguments.caller, args);
        
        return result;
    };
};

Function.prototype.delay = function(delay, thisObj) {
    delay = isUndefined(delay) ? 0 : delay;
    var func = this;

    window.setTimeout(function() {
        if (thisObj) {
            func.call(thisObj);
        } else {
            func();
        }
    }, delay);
};

Function.prototype.when = function(condition) {
    if(!condition || typeof(condition) !== 'function')
        throw new Error('wrong Function.when argument');

    var delay = 100;
    var func = this;

    var intervalId = window.setInterval(function() {
        if(!condition())
            return;
            
        window.clearInterval(intervalId);
        func();
    }, delay);
};
/// <reference path="Function.js" />

required = function(obj, params) {
  if (isNullOrUndefined(obj))
    throw new Error("Argument is null");

  if (params) {
    for (var i = 0; i < params.length; i++) {
        if (isNullOrUndefined(obj[params[i]]))
            throw new Error("Argument " + params[i] + " is null");
    }
  }
}

tryEval = function(text, defaulValue) {
    try {
        return eval(text);
    } catch(e) {
        console.warn("Cannot evaluate the expression: \"" + text + "\"", e);
        return isUndefined(defaulValue) ? text : defaulValue;
    }
};

isNumber = function(o) {
    return typeof(o) == 'number';
}

isBoolean = function(o) {
    return typeof(o) == 'boolean';
}

isArray = function(o) {
    return o instanceof Array;
}

isObject = function(o) {
    return typeof(o) == 'object';
}

isUndefined = function(o) {
    return typeof(o) == 'undefined';
}

isNullOrUndefined = function(o) {
    return o === null || isUndefined(o);
}

isDefined = function(o) {
    return !isNullOrUndefined(o);
}

Object.extend = function (target, src, deep) {
    if (!src)
        return target;

    for (var prop in src) {
        if (prop === 'constructor' || (typeof (target) == 'function' && Function.prototype[prop]))
            continue;

        if (!src.hasOwnProperty(prop))
            continue;

        if (typeof src[prop] === 'function') {
            target[prop] = src[prop];
        } else if (typeof (src[prop]) === 'object') {
            target[prop] = (deep && src[prop]) ? Object.copy(src[prop], true) : src[prop];
        } else {
            target[prop] = src[prop];
        }
    }

    //HACK: IE
    if (src.hasOwnProperty('toString')) {
        target.toString = src.toString;
    }

    return target;
};

Object.copy = function(src, deep) {
    if (Entity.isInstance(src)) {
        throw new Error('copying of entities is prohibited');
    }

    if (src.isEnum)  {
        return src;
    }

    var copy;
    
    if (src instanceof Array) {
        copy = [];
    } else {
        var clonedObject = function() {};
        clonedObject.prototype = src.constructor.prototype;
        copy = new clonedObject();
    }    
    
    Object.extend(copy, src, deep);
    
    if (typeof(src.constuctor) == 'function') {
        copy.constructor = src.constructor;
    }

    return copy;
};

Object.keysCount = function(obj) {
    var result = 0;

    for (var prop in obj) {
        if (obj.hasOwnProperty(prop)) {
            result++;
        }
    }

    return result;
};

Object.getPropertyValue = function(obj, propName) {
    if (obj[propName])
        return obj[propName];
    
    if (obj['get_' + propName])
        return obj['get_' + propName]();

    return false;
};

//Objec.prototype.hash = function() {
//    var hash = '{';

//    for (var prop in this) {
//        if (typeof this[prop] === 'function')
//            continue;

//        hash = hash.concat(
//            (typeof this[prop] === 'object') ?
//                this[prop].hash() :
//                this[prop].toString()
//        ).concat(',');
//    }

//    hash = hash.replace(/,$/, '');
//    return hash + '}';
//};

//Object.prototype.equals = function(right) {
//    if(typeof this === 'function' && typeof right === function)
//        return this.toString() === right.toString();
//        
//    return this.hash() === right.hash();
//};
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
/*
* Usage:
*     var methodInterceptor = new MethodInterceptor(<object>, function(originalFunc, arguments) {
*        originalFunc();
*    });
*    ...
*    methodInterceptor.restore();
*/

MethodInterceptor = function(target, funcName, methodInterceptorFunc) {
    this._target = target;
    this._oldFunc = {};
    this._funcName = funcName;
    this._methodInterceptorFunc = methodInterceptorFunc.bind(target);

    if (!isFunction(methodInterceptorFunc)) {
        throw new Error('You have to define a methodInterceptor function');
    }

    this.init();
};

MethodInterceptor.prototype = {
    init: function() {
        var target = this._target;
        var func = this._funcName;

        if (isFunction(target[func])) {
            this._oldFunc[func] = target[func];
            target[func] = (function(oldFunc, methodInterceptorFunc) {
                return function() {
                    var args = Array.prototype.slice.call(arguments || []);
                    return methodInterceptorFunc.call(this, oldFunc, args);
                }
            })(this._oldFunc[func], this._methodInterceptorFunc);
        } else {
            throw new Error('Cannot intercept function "' + func + '". That function is not exists.');
        }
    },
    
    restore: function() {
        var func = this._funcName;

        if (this._oldFunc.hasOwnProperty(func) && isFunction(this._oldFunc[func])) {
            this._target[func] = this._oldFunc[func];
        }

        this._oldFunc = {};
    }
};
/*
* Usage:
*     var proxyInterceptor = new ProxyInterceptor(<object>, function(methodName, arguments, originalFunc) {
*        sqlconsole.log(methodName);
*    });
*    ...
*    proxyInterceptor.restore();
*/

ProxyInterceptor = function(target, proxyInterceptorFunc) {
    this._target = target;
    this._oldFunc = {};
    this._proxyInterceptorFunc = proxyInterceptorFunc;

    if (!isFunction(proxyInterceptorFunc)) {
        throw new Error('You have to define a proxyInterceptor function');
    }

    this.init();
};

ProxyInterceptor.prototype = {
    init: function() {
        var target = this._target;

        for (var func in target) {
            if (isFunction(target[func])) {
                this._oldFunc[func] = target[func];
                target[func] = (function(func, oldFunc, proxyInterceptorFunc) {
                    return function() {
                        var args = Array.prototype.slice.call(arguments || []);
                        proxyInterceptorFunc(func, args, oldFunc);
                    }
                })(func, this._oldFunc[func], this._proxyInterceptorFunc);
            }
        }
    },
    
    restore: function() {
        for (var func in this._oldFunc) {
            if (this._oldFunc.hasOwnProperty(func) && isFunction(this._oldFunc[func])) {
                this._target[func] = this._oldFunc[func];
            }
        }

        this._oldFunc = {};
    }
};
Array.__typeName='Array';

Array.prototype.add=function Array$add(input) {
    if(!input || !(input instanceof Array)) {
        this[this.length]=input;
        return;
    }
    
    var length = input.length;

    for(var i=0; i<length; i++)
        this[this.length]=input[i];
        
    return input;
}

Array.prototype.addItem=function Array$addItem(input) {
    this[this.length]=input;
}

Array.prototype.ofType=function Array$ofType(type) {
    var result = [];

    if (isString(type)) {
        type = window[type];
    }

    for (var i = 0; i < this.length; i++) {
        if (!isNullOrUndefined(this[i]) && this[i].constructor && this[i].constructor === type) {
            result.add(this[i]);
        }
    }

    return result;
}

Array.prototype.aggregate=function Array$aggregate(seed,callback) {
    var length=this.length;

    for(var index=0;index<length;index++) {
        seed=callback(seed,this[index],index,this);
    }

    return seed;
}

Array.prototype.clear=function Array$clear() {
    if(this.length>0) {
        this.splice(0,this.length);
    }
}

Array.prototype.clone=function Array$clone() {
    var length=this.length;
    var array=new Array(length);

    for(var index=0;index<length;index++) {
        array[index]=this[index];
    }

    return array;
}

Array.prototype.dequeue=function Array$dequeue() {

    return this.shift();

}

Array.prototype.enqueue=function Array$enqueue(item) {

    // We record that this array instance is a queue, so we

    // can implement the right behavior in the peek method.

    this._queue=true;

    this.push(item);

}

Array.prototype.peek=function Array$peek() {

    if(this.length) {

        var index=this._queue?0:this.length-1;

        return this[index];

    }

    return null;

}

if(!Array.prototype.every) {

    Array.prototype.every=function Array$every(callback) {

        for(var i=this.length-1;i>=0;i--) {

            if(!callback(this[i],i,this)) {

                return false;

            }

        }

        return true;

    }

}

Array.prototype.extract=function Array$extract(index,count) {

    if(!count) {

        return this.slice(index);

    }

    return this.slice(index,index+count);

}

if (!Array.prototype.filter) {
    Array.prototype.filter = function Array$filter(callback) {
        var filtered = [];

        for (var i = 0; i < this.length; i++) {
            if (callback(this[i], i, this)) {
                filtered.add(this[i]);
            }
        }

        return filtered;
    }
}


Array.prototype.index=function Array$index(callback) {

    var length=this.length;

    var items={};

    for(var index=0;index<length;index++) {

        var key=callback(this[index],index);

        if(String.isNullOrEmpty(key)) {

            continue;

        }

        items[key]=this[index];

    }

    return items;

}

if (!Array.indexOf) {
    Array.prototype.indexOf = function Array$indexOf(item) {
        var length = this.length;
        
        if (length) {
            for (var index = 0;index < length; index++) {
                if (this[index] === item) {
                    return index;
                }
            }
        }
        
        return -1;
    }
}

Array.prototype.insert=function Array$insert(index, input) {
    if(input instanceof Array) {        
        for(var i = input.length - 1 ; i >= 0; i--)
            this.splice(index, 0 ,input[i]);
            
        return;
    }
    
    this.splice(index, 0 ,input);
}

Array.prototype.insertRange=function Array$insertRange(index,items) {

    this.splice(index,0,items);

}

if(!Array.prototype.map) {

    Array.prototype.map=function Array$map(callback) {

        var mapped=new Array(this.length);

        for(var i=this.length-1;i>=0;i--) {

            mapped[i]=callback(this[i],i,this);

        }

        return mapped;

    }

}

Array.parse=function Array$parse(s) {

    return eval('('+s+')');

}

Array.prototype.remove=function Array$remove(input) {
    if(!(input instanceof Array)) {
        var index=this.indexOf(input);
        
        if(index < 0)
            throw new Error('Item not found in array on remove.');

        this.splice(index,1);
            
        return;
    }

    for(var i=0; i < input.length; i++) {
        var index = this.indexOf(input[i]);
        
        if(index < 0)
            throw new Error('Item not found in array on remove.');
            
        this.splice(index,1);
    }
}

Array.prototype.exclude = function Array$exclude(input) {
    if (!(input instanceof Array)) {
        var index = this.indexOf(input);

        if (index < 0) {
            return [];
        }

        this.splice(index, 1);

        return [input];
    }

    var excluded = [];

    for (var i = 0; i < input.length; i++) {
        var index = this.indexOf(input[i]);

        if (index < 0) {
            continue;
        }

        excluded.add(input[i]);
        this.splice(index, 1);
    }

    return excluded;
}

Array.prototype.removeAt=function Array$removeAt(index) {
    return this.splice(index,1)[0];
}

Array.prototype.removeRange=function Array$removeRange(index,count) {
    return this.splice(index,count);
}

if(!Array.prototype.some) {
    Array.prototype.some=function Array$some(callback) {
        for(var i=this.length-1;i>=0;i--) {
            if(callback(this[i],i,this)) {
                return true;
            }
        }
        return false;
    }
} 

Array.prototype.synchronize = function Array$synchronize(source, comparer, args) {    
    var toAdd = [];
    var toRemove = [];

    for(var i = this.length-1; i >= 0; i--) {
        if(!source.contains(this[i], comparer))
            toRemove.add(this[i]);
    }
    
    for(var i = 0; i < source.length; i++) {
        if(!this.contains(source[i], comparer))
            toAdd.add(source[i]);
    }
    
    this.remove(toRemove, args);
    this.add(toAdd, args);
    
    return toAdd.length + toRemove.length;
};

Array.prototype.merge = function Array$merge(source, comparer, args) {
    var toAdd = source.where(function(item) {
        return !this.contains(item, comparer);
    }, this);
        
    this.add(toAdd, args);
};
Array.prototype.forEach = function(action, thisObject, inverse) {
    var func = !!thisObject ? action.bind(thisObject) : action;
    if(!inverse)    
        for (var i = 0; i < this.length; i++) {
            func(this[i], i);        
        }
    else
        for (var i = this.length - 1; i >= 0 ; i--) {
            func(this[i], i);        
        }
};

Array.prototype.select = function(selectFunc, thisObject) {
    if(typeof selectFunc === 'string') {
        var prop = selectFunc;
        
        selectFunc = function(item) {
            return item['get_' + prop] ?
                   item['get_' + prop]() :
                   item[prop];
        };
    }

    var func = !!thisObject ? selectFunc.bind(thisObject) : selectFunc;
    
    var result = [];

    this.forEach(function(item) {
        result.push(func(item));
    })

    return result;
};

Array.prototype.selectMany = function(selectFunc, thisObject) {
    var func = !!thisObject ? selectFunc.bind(thisObject) : selectFunc;

    var results = [];

    this.forEach(function(singleItem) {
        var singleResults = func(singleItem);

        singleResults.forEach(function(item) {
            results.push(item);
        });
    });

    return results;
};

Array.prototype.toDictionary = function(keySelector, valueSelector) {
    var result = {};

    this.forEach(function(item) {
        result[keySelector(item)] = valueSelector ? valueSelector(item) : item;
    });

    return result;
};

Array.prototype.where = function(filter, thisObject) {
    var filter = !!thisObject ? filter.bind(thisObject) : filter;

    var result = [];

    this.forEach(function(item) {
        if (filter(item)) {
            result.push(item);
        }
    });

    return result;
};

Array.prototype.first = function(filter, thisObject) {
    if (!filter)
        filter = function() { return true; }

    var filter = !!thisObject ? filter.bind(thisObject) : filter;

    for (var i = 0; i < this.length; i++) {
        if (filter(this[i]))
            return this[i];
    }

    return null;
};

Array.prototype.findByProperty = function(propertyName, value) {
    for (var i = 0; i < this.length; i++) {
        var item = this[i];
        var itemValue = item['get_' + propertyName] ? item['get_' + propertyName]() : item[propertyName];

        if (itemValue == value) {
            return item;
        }
    }
};

Array.prototype.last = function Array$last(filter, thisObject) {
    if (!filter)
        filter = function() { return true; }

    var filter = !!thisObject ? filter.bind(thisObject) : filter;    
    
    for (var i = this.length - 1; i >= 0 ; i--) {
        if (filter(this[i]))
            return this[i];
    }

    return null;
};

Array.prototype.single = function Array$single(filter, thisObject) {
    if (!filter)
        filter = function() { return true; }

    var founded = this.where(filter, thisObject);

    if (founded.length != 1)
        throw Error("array single exception");

    return founded[0];
};

Array.prototype.singleOrDefault = function Array$singleOrDefault(filter, thisObject) {
    var founded = this.where(filter, thisObject);

    if (founded.length > 1)
        throw Error("array single exception");    

    return founded.length == 1 ? founded[0] : null;
};

Array.prototype.contains = function Array$contains(input, comparer) {        
    /*if(input instanceof Array)
        return this._containsMany(input, comparer);*/
        
    if (!comparer) {
        return this.indexOf(input) >= 0;
    }

    for (var i = 0; i < this.length; i++) {
        if (comparer(input, this[i])) {
            return true;
        }
    }
    
    return false;
};

Array.prototype.containsMany = function Array$containsMany(items, comparer) {    
    for (var i = 0; i < items.length; i++) {
        if (!this.contains(items[i]), comparer) {
            return false;
        }
    }
        
    return true;
},

Array.prototype.distinct = function Array$distinct(comparer) {
    if(!comparer) {
        comparer = function(a, b) { return a === b };
    }
    
    var temp = [];

    for (var i = 0; i < this.length; i++) {
        var item = this[i];
        if (!temp.contains(item, comparer)) {
            temp.add(item);
        }
    }
    
    return temp;
};

Array.prototype.sum = function Array$sum(summator, thisObject) {
    var summator = !!thisObject ? summator.bind(thisObject) : summator;

    var result = 0;

    this.forEach(function(item) {
        result += summator(item);
    });

    return result;
};
if (!window.Auto)
    Auto = {};

Object.extend(Auto, {
    Event: function(object, eventName) {
        object['raise_' + eventName] = function Auto$Event$raise_event(args, context) {
            if(!this.__events ||
               !this.__events[eventName] || 
                this.__events[eventName].__handlers.length == 0)
                return;
        
            var sender = this;
            var handlers = this.__events[eventName].__handlers;
            
            for (var i = 0; i < handlers.length; i+=2) {
                var handler = handlers[i];
                var thisObj = handlers[i+1];
                
                var func = thisObj ? handler.bind(thisObj) : handler;
                func(sender, args, context);
            }
        };
        
        object['add_' + eventName] = function Auto$Event$add_event(handler, thisObj) {
            if (!this.__events) {
                this.__events = {};
            }

            if(!this.__events[eventName]) {
                this.__events[eventName] = {
                    __handlers: []
                };
            }
            
            var handlers = this.__events[eventName].__handlers;
            handlers.addItem(handler);
            handlers.addItem(thisObj);
        };

        object['remove_' + eventName] = function Auto$Event$remove_event(handler, thisObj) {
            if (!this.__events || !this.__events[eventName]) {
                return;
            }
            
            var handlers = this.__events[eventName].__handlers;
            var newHandlers = [];
            
            for (var i = 0; i < handlers.length; i+=2) {
                if (handlers[i] !== handler || (thisObj && handlers[i+1] !== thisObj)) {
                    newHandlers.addItem(handlers[i]);
                    newHandlers.addItem(handlers[i+1]);
                }
            }
            
            this.__events[eventName].__handlers = newHandlers;
        };
    },
    
    Events: function(object, events) {
        for(var i = 0; i < events.length; i ++)
            this.Event(object, events[i]);
    },
    
    ApplyEventDisposing: function ApplyEventDisposing(target) {
        if(target.dispose && target.dispose.__eventDisposing)
            return;
        
        Trait.Apply(target, {
            dispose: function() {
                var events = this.__events;
                if (events) {
                    for (var prop in events) {
                        if (events.hasOwnProperty(prop)) {
                            if (events[prop] && isArray(events[prop].__handlers)) {
                                events[prop].__handlers.clear();
                            }
                            delete events[prop];
                        }
                    }
                    delete this.__events;
                }
            },
            clearEvents: function() {
                if(this.__events)
                    delete this.__events;
            }
        });
        
        target.dispose.__eventDisposing = true;
    }
});
/// <reference path="Object.js" />
/// <reference path="Data/Array.js" />
/// <reference path="Data/Array.Linq.js" />
if (!window.Auto)
    Auto = {};

Object.extend(Auto, {
    Property: function(object, property) {
        // array used to avoid cicles when set_prop(abc) causes invoking set_prop(abc);

        if (property.autoEvent) {
            Auto.Event(object, property.name + 'Changed');
        }

        this._attachProperty(
            object,
            property);
    },

    Properties: function(object, properties) {
        properties.forEach(function(property) {
            this.Property(object, property);
        }, this);
    },

    _attachProperty: function(object, property) {
        var name = typeof(property) === 'string' ?
                   property : property.name;
                   
        if (!name) {
            throw new Error('[Auto.Property]: property name is undefined');
        }
                   
        var getterName = 'get_' + name;
        
        if(!object[getterName]) {
            var getter = !isUndefined(property.defaultValue) || isFunction(property.get_defaultValue) ?
                 function() {
                    if (!this.hasOwnProperty('_' + name)) {
                        this['_' + name] = arguments.callee.get_defaultValue();
                    }

                    return this['_' + name];
                 } :
                 function() {
                    return this['_' + name];
                 }
        
            object[getterName] = getter;
            object[getterName].autoGetter = true;

            if (!isUndefined(property.defaultValue)) {
                var defValue = property.defaultValue;

                if (defValue && isObject(defValue)) {
                    console.warn("[Auto.Property] Using of object as a default value of the property can cause unexpected behaviour. Property name: ", property.name, ". Definition: ", property);
                }

                object[getterName].get_defaultValue = function() {
                    if (defValue && isObject(defValue)) {
                        return Object.copy(defValue);
                    }

                    return defValue;
                };
            }

            if (isFunction(property.get_defaultValue)) {
                object[getterName].get_defaultValue = property.get_defaultValue;
            }
        }

        if(!object['set_' + name]) {        
            var setter = !property.autoEvent ?
                function Auto$Property$setter(value) {
                    if (this['_' + name] === value)
                        return;

                    this['_' + name] = value;
                } :
                function Auto$Property$setter$withEvent(value) {
                    var propertyName = '_' + name;
                    if (this[propertyName] === value)
                        return;
                        
                    var oldValue = this.hasOwnProperty(propertyName) ? this[propertyName] : this['get' + propertyName]();

                    this[propertyName] = value;

                    this['raise_' + name + 'Changed']({
                        newValue: value,
                        oldValue: oldValue
                    });
                };

            setter.autoSetter = true;
            object['set_' + name] = setter;
        }
        
        if (!object['set_' + name].override) {
            // override setter to avoid stack overflow
            // Warning: this code can cause to unexpected behaviour in some cases
            object['__$set_' + name] = object['set_' + name];
            object['set_' + name] = function Auto$Property$setter(value) {
                if (!this.__alreadyCalledWith) {
                    this.__alreadyCalledWith = {};
                }

                if (!this.__alreadyCalledWith[name]) {
                    this.__alreadyCalledWith[name] = [];
                }

                var alreadyCalledWith = this.__alreadyCalledWith[name];
                
                if (!alreadyCalledWith.contains(value)) {
                    alreadyCalledWith.add(value);
                    this['__$set_' + name].apply(this, arguments);
                    alreadyCalledWith.remove(value);
                }
            }
            object['set_' + name].override = true;
        }
    }
});

Auto.Property.CreateOneWayLink = function(obj1, obj2, property1, property2) {
    if (!property2) {
        property2 = property1;
    }

    var linkInfo = {
        _obj1: obj1,
        _obj2: obj2,
        _property1: property1,
        _property2: property2,
        _obj1Handler: function(sender, args) {
            this._obj2['set_' + this._property2](args.newValue);
        },
        dispose: function() {
            this._obj1['remove_' + this._property1 + 'Changed'](this._obj1Handler);
        }
    };

    var handler1 = obj1['add_' + property1 + 'Changed'];

    if (!isFunction(handler1)) {
        throw new Error("[Auto.Property.CreateLink] Cannot find the event handler for \"" + property1 + "\" property");
    }

    obj2['set_' + property2](obj1['get_' + property1]());
    obj1['add_' + property1 + 'Changed'](linkInfo._obj1Handler, linkInfo);

    return linkInfo;
}

Auto.Property.CreateLink = function(obj1, obj2, property1, property2) {
    if (!property2) {
        property2 = property1;
    }

    var handler1 = obj1['add_' + property1 + 'Changed'];
    var handler2 = obj2['add_' + property2 + 'Changed'];

    if (!isFunction(handler1)) {
        throw new Error("[Auto.Property.CreateLink] Cannot find the event handler for \"" + property1 + "\" property");
    }

    if (!isFunction(handler2)) {
        throw new Error("[Auto.Property.CreateLink] Cannot find the event handler for \"" + property2 + "\" property");
    }

    obj2['set_' + property2](obj1['get_' + property1]());

    var linkInfo = {
        _obj1: obj1,
        _obj2: obj2,
        _property1: property1,
        _property2: property2,
        _obj1Handler: function(sender, args) {
            this._obj2['set_' + this._property2](args.newValue);
        },
        _obj2Handler: function(sender, args) {
            this._obj1['set_' + this._property1](args.newValue);
        },
        dispose: function() {
            this._obj1['remove_' + this._property1 + 'Changed'](this._obj1Handler);
            this._obj2['remove_' + this._property2 + 'Changed'](this._obj2Handler);
        }
    };

    obj1['add_' + property1 + 'Changed'](linkInfo._obj1Handler, linkInfo);
    obj2['add_' + property2 + 'Changed'](linkInfo._obj2Handler, linkInfo);

    return linkInfo;
};
Trait = {
    Apply: function(target, source, options) {
        var cache = source.__$traitPropertiesCache;
        
        if (!cache) {
            cache = [];
            
            for (var prop in source) {
                cache.add(prop);
            }
            
            source.__$traitPropertiesCache = cache;
        }
        
        for (var i = 0; i < cache.length; i++) {
            Trait._applyForProperty(target, source, options, cache[i]);
        }
    },
    
    _applyForProperty: function(target, source, options, prop) {
        if (typeof(source[prop]) == 'function') {
            if(target[prop] && typeof(target[prop]) == 'function') {
                this._traitFunction(
                    target, prop, source[prop], 
                    options && options.asOriginal && options.asOriginal.contains(prop),
                    options && options.overrideAll && options.overrideAll.contains(prop));
            }
            else
                target[prop] = source[prop];
        }
        else {
            target[prop] = source[prop];
        }
    },
    
    _traitFunction: function(object, funcName, newFunc, asOriginal, overrideAll) {
        if(object[funcName].__overriden) {
            return;
        }
        
        if(overrideAll) {
            object[funcName] = newFunc;
            return;
        }
    
        if(!object[funcName].__traits) {
            var original = object[funcName];
            
            var traitedFunc = function() {
                var args = Array.prototype.slice.call(arguments || []);
                
                var originalResult = arguments.callee.__traits[0].apply(this, args);
                
                // There been issue when original function was called with lesser count of arguments
                // args[args.length] = originalResult;
                var originalResultArgsIndex = original.length;

                // If function was called with greater count of arguments we put original
                // result to the end of arguments list
                if (args.length >= originalResultArgsIndex) {
                    originalResultArgsIndex = args.length;
                }

                args[originalResultArgsIndex] = originalResult;
                
                for(var i = 1 ; i < arguments.callee.__traits.length; i++)
                    arguments.callee.__traits[i].apply(this, args);
                    
                return originalResult;
            };
            
            traitedFunc.__traits = [original];
            
            object[funcName] = traitedFunc;
        }
        
        if(asOriginal) {
            // TODO: Warning! The source function can already has a link to the original function at the next line!
            newFunc.original = object[funcName].__traits[0]; // TODO: Check behaviour of that code on a chain of traits
            object[funcName].__traits[0] = newFunc;
        }
        else        
            object[funcName].__traits.add(newFunc);
    }
};
Observable = {
    add: function(input, args) {
        var items = (input instanceof Array) ? input : [input];        
               
        if (items.length == 0) {
            return;
        }
        
        this.raise_added(this.__createAddedArguments(items));
        this.raise_changed({ 
                added: items,
                removed: []
            }, args);
    },
    
    insert: function(index, input, args) {
        var items = (input instanceof Array) ? input : [input];
        
        if (items.length == 0) {
            return;
        }

        this.raise_added(this.__createAddedArguments(items));
        this.raise_changed({ 
                added: items,
                removed: []
            }, args);
    },
    
    remove: function(input, args) {
        var items = (input instanceof Array) ? input : [input];
    
        if (items.length == 0) {
            return;
        }

        this.raise_removed({items: items});
        this.raise_changed({ 
                added: [],
                removed: items
            }, args);
    },
    
    exclude: function(input, args) {
        var items = args;
    
        if (items.length == 0) {
            return;
        }

        this.raise_removed({items: items});
        this.raise_changed({ 
                added: [],
                removed: items
            }, args);
    },

    clear: function(args) {
        if(this.length === 0)
            return;
    
        var items = [];
        
        for(var i=0; i < this.length; i++ )
            items.add(this[i]);

        this.splice(0, this.length);
        //arguments.callee.original();
        
        this.raise_removed({items: items});
        this.raise_changed({ 
                added: [],
                removed: items
            }, args);
    },

    /**
    * It used in attach/detach functions to observe another collection.
    */
    __attachDelegate: function(sender, args) {
        this.remove_changed(this.__attachDelegate, sender);

        if (args.added.length > 0) {
            this.add(args.added);
        }
        
        if (args.removed.length > 0) {
            this.remove(args.removed);
        }
        
        this.add_changed(this.__attachDelegate, sender);
    },

    /**
    * Synchronize with @observable collection and subscribe to changing event of target collection.
    * When target collection is updated, current collection will be updated too.
    * @param synchronize: it's true by default
    */
    attach: function(observable, synchronize) {
        if (!isDefined(synchronize) || synchronize) {
            this.synchronize(observable);
        }

        observable.add_changed(this.__attachDelegate, this);
        this.add_changed(this.__attachDelegate, observable);
    },
    
    /**
    * Unsubscribe from changing event of target collection.
    */
    detach: function(observable) {
        observable.remove_changed(this.__attachDelegate, this);
        this.remove_changed(this.__attachDelegate, observable);
    },

    __exclusiveAttachDelegate: function(sender, args) {
        this.remove_changed(this.__exclusiveAttachDelegate, sender);

        if (args.added.length > 0) {
            for (var i = 0; i < args.added.length; i++) {
                if (this.contains(args.added[i])) {
                    this.remove(args.added[i]);
                }
            }
        }
        
        if (args.removed.length > 0) {
            for (var i = 0; i < args.removed.length; i++) {
                if (!this.contains(args.removed[i])) {
                    this.add(args.removed[i]);
                }
            }
        }
        
        this.add_changed(this.__exclusiveAttachDelegate, sender);
    },

    /**
    * Works like the "attach" method but when one item removed from the first collection it moves to the second one
    */
    exclusiveAttach: function(observable) {
        for (var i = observable.length - 1; i >= 0; i--) {
            if (this.contains(observable[i])) {
                observable.remove(observable[i]);
            }
        }

        observable.add_changed(this.__exclusiveAttachDelegate, this);
        this.add_changed(this.__exclusiveAttachDelegate, observable);
    },

    /**
    * Unsubscribe from changing event of target collection.
    */
    exclusiveDetach: function(observable) {
        observable.remove_changed(this.__exclusiveAttachDelegate, this);
        this.remove_changed(this.__exclusiveAttachDelegate, observable);
    },

    __createAddedArguments: function(items) {
        var args = {
            items: [],
            after: []
        };
        
        var processed = [];

        for (var i = 0; i < items.length; i++) {            
            processed.add({item: items[i], index: this.indexOf(items[i])});
        }

        processed.sort(function(a, b) {
            return a.index == b.index ? 0 : (a.index > b.index ? 1 : -1);
        });
        
        for (var i = 0; i < processed.length; i++) {
            args.items.add(processed[i].item);
            args.after.add(processed[i].index === 0 ? null : this[processed[i].index - 1]);
        }
        
        return args;
    }
};

Auto.Events(Observable, [
    'added',
    'removed',
    'changed'
]);

Auto.ApplyEventDisposing(Observable);

Array.prototype.makeObservable = function() {
    if(this.__observable)
       return this; 
    
    Trait.Apply(this, Observable, { asOriginal: ['clear'] });
    this.__observable = true;
    
    return this;
};
SortedArray = {
    add: function (data, args) {        
        var items = data instanceof Array ? data : [data];

        for (var i = 0; i < items.length; i++)
            this.splice(this._findPosition(items[i]), 0, items[i]);
    },

    sort: function() {
        if (this.length <= 1)
            return;
    
        var initial = [];
        
        for (var i = 0; i< this.length; i ++)
            initial[i] = this[i];   

        var sorted = this.__mergeSort(this);
        var sortMap = this.__getSortMapSimple(sorted);

        for(var i = 0; i < sortMap.length; i++) {
            this.splice(sortMap[i].end, 1, sortMap[i].item);
        }

        var movedItems = sortMap.select(function(sm) {return sm.item});

        //TODO: get rid of removed event
        if (movedItems.length > 0) {
            this.raise_removed({items: movedItems});
            this.raise_added(this.__createAddedArguments(movedItems));
        }
    },

    __getSortMapSimple: function(sortResult) {
        var map = [];
        
        for(var i=0; i < sortResult.length; i++) {
            if(sortResult[i] === this[i])
                continue;
                
            map.add({
                item: sortResult[i],
                begin: this.indexOf(sortResult[i]),
                end: i
            });            
        }
        
        return map;
    },

    __mergeSort: function(arr) {
        if (arr.length == 1)
            return arr;
        
        var arrSorted = [];
        var middle = Math.floor(arr.length/2);
        
        var leftArray = arr.slice(0, middle);
        var rightArray = arr.slice(middle, arr.length);
        
        leftArray =  this.__mergeSort(leftArray);
        rightArray = this.__mergeSort(rightArray);            
        
        var left = 0;
        var right = 0;

        for (var i = 0; i < leftArray.length + rightArray.length; i++) {
            if (left == leftArray.length){
                arrSorted.add(rightArray[right]);
                right++;
            } else if (right == rightArray.length){
                arrSorted.add(leftArray[left]);
                left++;
            } else if(this.__compare(leftArray[left], rightArray[right]) <= 0) {
                arrSorted.add(leftArray[left]);
                left++;
            } else {
                arrSorted.add(rightArray[right]);
                right++;
            }
        }
        return arrSorted;
    },

    _findPosition: function(item) {   
        if (this.length == 0)
            return 0;
        
        var start = 0;
        var end = this.length - 1;
        
        while( start <= end) {        
            var mid = start + Math.floor((end - start) / 2);
            
            var previous = this[mid - 1];
            var current = this[mid];
            var next = this[mid + 1];
            
            var leftCompare = previous ? this.__compare(item, previous) : 1;
            var midCompare = this.__compare(item, current);
            var rightCompare = next ? this.__compare(item, next) : -1;

            if (isUndefined(leftCompare) || isUndefined(midCompare) || isUndefined(rightCompare)) {
                throw new Error('Comparer returns undefined');
            }
            
            if(midCompare > 0) {
                if(rightCompare > 0)
                    start = mid + 1;
                else
                    return mid + 1;
            }
            else if(midCompare < 0) {
                if(leftCompare < 0)
                    end = mid - 1;
                else
                    return mid;
            }
            else
                return mid + 1;
        }
    }
};

Array.prototype.makeSorted = function(comparer) {
    if (this.__sortedArray)
       return this;

    if (!this.__observable) {
        this.makeObservable();
    }

    if (!isFunction(comparer)) {
        if (isString(comparer)) {
            comparer = (function(prop) {
                return function(h1, h2) {
                    return h1['get_' + prop]() == h2['get_' + prop]() ? 0 : (h1['get_' + prop]() > h2['get_' + prop]() ? 1 : -1);
                }
            })(comparer);
        } else {
            comparer = function(a, b) {
                return a == b ? 0 : (a > b ? 1 : -1);
            }
        }
    }

    this.__compare = comparer;
    
    Trait.Apply(this, SortedArray, {
        asOriginal: ['add']
    });

    this.__sortedArray = true;
    this.sort();
    
    return this;
};
Tracking = {
    isTrackingDto: function(dto) {
        return dto.__type.startsWith('TrackingCollection_');
    },

    add: function(input, args) {
        this.items.add(input);
    
        if(args && args.notTrackable)
            return;
    
        var items = input instanceof Array ? input : [input];
        
        items.forEach(function(item) {
            if(this.removed.contains(item))
                this.removed.remove(item);
            else
                this.added.add(item);
        }, this);
    },    
    
    remove: function(input, args) {
        this.items.remove(input);
    
        if(args && args.notTrackable)
            return;
            
        var items = input instanceof Array ? input : [input];
    
        items.forEach(function(item) {
            if(this.added.contains(item))
                this.added.remove(item);
            else
                this.removed.add(item); 
        }, this);
    }
};

Array.prototype.makeTracking = function(entityType) {
    if(this.__tracking)
       return this;
    
    Trait.Apply(this, Tracking);
    
    this.added = [];
    this.items = [];
    this.removed = [];
    
    this.__type = entityType ? 'TrackingCollection_' + entityType + ':#Phoenix.Infrastructure.Collections' : 'TrackingCollection:#Phoenix.Infrastructure.Collections';
    
    for(var i = 0; i < this.length; i++)
        this.items.add(this[i]);
    
    this.__tracking = true;
    
    return this;
};

var Type = Function;
Type.__typeName = 'Type';

var __Namespace = function(name) {
    this.__typeName = name;
}
__Namespace.prototype = {
    __namespace: true,
    getName: function() {
        return this.__typeName;
    }
}

Type.createNamespace = function Type$createNamespace(name) {
    if (!window.__namespaces) {
        window.__namespaces = {};
    }
    if (!window.__rootNamespaces) {
        window.__rootNamespaces = [];
    }

    if (window.__namespaces[name]) {
        return;
    }

    var ns = window;
    var nameParts = name.split('.');

    for (var i = 0; i < nameParts.length; i++) {
        var part = nameParts[i];
        var nso = ns[part];
        if (!nso) {
            ns[part] = nso = new __Namespace(nameParts.slice(0, i + 1).join('.'));
            if (i == 0) {
                window.__rootNamespaces.add(nso);
            }
        }
        ns = nso;
    }

    window.__namespaces[name] = ns;
}

Type.prototype.createClass = function Type$createClass(name, baseType, interfaceType) {
    this.prototype.constructor = this;
    this.__typeName = name;
    this.__class = true;
    this.__baseType = baseType || Object;
    if (baseType) {
        this.__basePrototypePending = true;
    }

    if (interfaceType) {
        this.__interfaces = [];
        for (var i = 2; i < arguments.length; i++) {
            interfaceType = arguments[i];
            this.__interfaces.add(interfaceType);
        }
    }
}

Type.prototype.createInterface = function Type$createInterface(name) {
    this.__typeName = name;
    this.__interface = true;
}

Type.prototype.createEnum = function Type$createEnum(name, flags) {
    for (var field in this.prototype) {
         this[field] = this.prototype[field];
    }

    this.__typeName = name;
    this.__enum = true;
    if (flags) {
        this.__flags = true;
    }
}

Type.prototype.setupBase = function Type$setupBase() {
    if (this.__basePrototypePending) {
        var baseType = this.__baseType;
        if (baseType.__basePrototypePending) {
            baseType.setupBase();
        }

        for (var memberName in baseType.prototype) {
            var memberValue = baseType.prototype[memberName];
            if (!this.prototype[memberName]) {
                this.prototype[memberName] = memberValue;
            }
        }
        
        if (this.prototype.toString === Object.prototype.toString) {
            this.prototype.toString = baseType.prototype.toString;
        }

        delete this.__basePrototypePending;
    }
}

if (!Type.prototype.resolveInheritance) {
    Type.prototype.resolveInheritance = Type.prototype.setupBase;
}

Type.prototype.constructBase = function Type$constructBase(instance, args) {
    if (this.__basePrototypePending) {
        this.setupBase();
    }

    if (!args) {
        this.__baseType.apply(instance);
    }
    else {
        this.__baseType.apply(instance, args);
    }
}

Type.prototype.callBase = function Type$callBase(instance, name, args) {
    var baseMethod = this.__baseType.prototype[name];
    if (!args) {
        return baseMethod.apply(instance);
    }
    else {
        return baseMethod.apply(instance, args);
    }
}

Type.prototype.get_baseType = function Type$get_baseType() {
    return this.__baseType || null;
}

Type.prototype.get_fullName = function Type$get_fullName() {
    return this.__typeName;
}

Type.prototype.get_name = function Type$get_name() {
    var fullName = this.__typeName;
    var nsIndex = fullName.lastIndexOf('.');
    if (nsIndex > 0) {
        return fullName.substr(nsIndex + 1);
    }
    return fullName;
}

Type.prototype.isInstance = function Type$isInstance(instance) {
    if (isNullOrUndefined(instance)) {
        return false;
    }
    if ((this == Object) || (instance instanceof this)) {
        return true;
    }

    var type = Type.getInstanceType(instance);
    return this.isAssignableFrom(type);
}

Type.prototype.isAssignableFrom = function Type$isAssignableFrom(type) {
    if ((this == Object) || (this == type)) {
        return true;
    }
    if (this.__class) {
        var baseType = type.__baseType;
        while (baseType) {
            if (this == baseType) {
                return true;
            }
            baseType = baseType.__baseType;
        }
    }
    else if (this.__interface) {
        var interfaces = type.__interfaces;
        if (interfaces && interfaces.contains(this)) {
            return true;
        }

        var baseType = type.__baseType;
        while (baseType) {
            interfaces = baseType.__interfaces;
            if (interfaces && interfaces.contains(this)) {
                return true;
            }
            baseType = baseType.__baseType;
        }
    }
    return false;
}

Type.isClass = function Type$isClass(type) {
    return (type.__class == true);
}

Type.isEnum = function Type$isEnum(type) {
    return (type.__enum == true);
}

Type.isFlagsEnum = function Type$isFlagsEnum(type) {
    return ((type.__enum == true) && (type.__flags == true));
}

Type.isInterface = function Type$isInterface(type) {
    return (type.__interface == true);
}

Type.hasBaseMethod = function(obj, name) {
    var type = Type.getInstanceType(obj);
    return type.__baseType && isFunction(type.__baseType.prototype[name]);
};

Type.canCast = function Type$canCast(instance, type) {
    return type.isInstance(instance);
}

Type.safeCast = function Type$safeCast(instance, type) {
    if (type.isInstance(instance)) {
        return instance;
    }
    return null;
}

Type.getInstanceType = function Type$getInstanceType(instance) {
    var ctor = null;

    // NOTE: We have to catch exceptions because the constructor
    //       cannot be looked up on native COM objects
    try {
        ctor = instance.constructor;
    }
    catch (ex) {
    }
    if (!ctor || !ctor.__typeName) {
        ctor = Object;
    }
    return ctor;
}

Type.getType = function Type$getType(typeName) {
    if (!typeName) {
        return null;
    }

    if (!Type.__typeCache) {
        Type.__typeCache = {};
    }

    var type = Type.__typeCache[typeName];
    if (!type) {
        type = eval(typeName);
        Type.__typeCache[typeName] = type;
    }
    return type;
}
var StringBuilder = function StringBuilder$(s) {

    if ((s !== undefined) && (s !== null)) {
        this._parts = [ s ];
    }
    else {
        this._parts = [];
    }
}

StringBuilder.prototype = {
    get_isEmpty: function StringBuilder$get_isEmpty() {
        return (this._parts.length == 0);
    },

    append: function StringBuilder$append(s) {
        if ((s !== undefined) && (s !== null)) {
            this._parts.add(s);
        }
    },

    appendLine: function StringBuilder$appendLine(s) {
        this.append(s);
        this.append('\r\n');
    },

    clear: function StringBuilder$clear() {
        this._parts.clear();
    },

    toString: function StringBuilder$toString() {
        return this._parts.join('');
    }
};

StringBuilder.createClass('StringBuilder');
RegExp.parse = function(s) {
    if (s.startsWith('/')) {
        var endSlashIndex = s.lastIndexOf('/');
        if (endSlashIndex > 1) {
            var expression = s.substring(1, endSlashIndex);
            var flags = s.substr(endSlashIndex + 1);
            return new RegExp(expression, flags);
        }
    }

    return null;    
}

RegExp.escape = function(text) {
    if (!arguments.callee.sRE) {
        var specials = [
            '/', '.', '*', '+', '?', '|',
            '(', ')', '[', ']', '{', '}', '\\'
        ];
        arguments.callee.sRE = new RegExp(
            '(\\' + specials.join('|\\') + ')', 'g'
        );
    }
    
    return text.replace(arguments.callee.sRE, '\\$1');
}

///////////////////////////////////////////////////////////////////////////////
// String Extensions

/*function isNullOrUndefined(o) {
    return (o === null) || (o === undefined);
}*/

function isString(o){
    return typeof(o) === 'string';
}

Number.prototype.pluralize = function Number$pluralize(s1, s2) {
    if (this == 1) {
        return this + ' ' + s1;
    }
    
    return this + ' ' + s2;
}

String.__typeName = 'String';
String.Empty = '';

String.compare = function String$compare(s1, s2, ignoreCase) {
    if (ignoreCase) {
        if (s1) {
            s1 = s1.toUpperCase();
        }
        if (s2) {
            s2 = s2.toUpperCase();
        }
    }
    s1 = s1 || '';
    s2 = s2 || '';

    if (s1 == s2) {
        return 0;
    }
    if (s1 < s2) {
        return -1;
    }
    return 1;
}

String.prototype.compareTo = function String$compareTo(s, ignoreCase) {
    return String.compare(this, s, ignoreCase);
}

String.prototype.endsWith = function String$endsWith(suffix) {
    if (!suffix.length) {
        return true;
    }
    if (suffix.length > this.length) {
        return false;
    }
    return (this.substr(this.length - suffix.length) == suffix);
}

String.equals = function String$equals1(s1, s2, ignoreCase) {
    return String.compare(s1, s2, ignoreCase) == 0;
}

String._format = function String$_format(format, values, useLocale) {
    if (!String._formatRE) {
        String._formatRE = /(\{[^\}^\{]+\})/g;
    }

    return format.replace(String._formatRE,
                          function(str, m) {
                              var index = parseInt(m.substr(1));
                              var value = values[index + 1];
                              if (isNullOrUndefined(value)) {
                                  return '';
                              }
                              if (value.format) {
                                  var formatSpec = null;
                                  var formatIndex = m.indexOf(':');
                                  if (formatIndex > 0) {
                                      formatSpec = m.substring(formatIndex + 1, m.length - 1);
                                  }
                                  return value.format.call(value, formatSpec, useLocale);
                              }
                              else {
                                  if (useLocale) {
                                      return value.toLocaleString();
                                  }
                                  return value.toString();
                              }
                          });
}

String.format = function String$format(format) {
    return String._format(format, arguments, /* useLocale */ false);
}

String.prototype.format = function() {
    var args = [ this ];
    
    for (var i = 0; i < arguments.length; i++) {
        args.add(arguments[i]);
    }
    
    return String._format(this, args, false);
};

String.fromChar = function String$fromChar(ch, count) {
    var s = ch;
    for (var i = 1; i < count; i++) {
        s += ch;
    }
    return s;
}

String.prototype.htmlDecode = function String$htmlDecode() {
    if (!String._htmlDecRE) {
        String._htmlDecMap = { '&amp;': '&', '&lt;': '<', '&gt;': '>', '&quot;': '"' };
        String._htmlDecRE = /(&amp;|&lt;|&gt;|&quot;)/gi;
    }

    var s = this;
    s = s.replace(String._htmlDecRE,
                  function String$htmlDecode$replace(str, m) {
                      return String._htmlDecMap[m];
                  });
    return s;
}

String.prototype.htmlEncode = function String$htmlEncode() {
    if (!String._htmlEncRE) {
        String._htmlEncMap = { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' };
        String._htmlEncRE = /([&<>"])/g;
    }

    var s = this;
    if (String._htmlEncRE.test(s)) {
        s = s.replace(String._htmlEncRE,
                      function String$htmlEncode$replace(str, m) {
                          return String._htmlEncMap[m];
                      });
    }
    return s;
}

String.prototype.indexOfAny = function String$indexOfAny(chars, startIndex, count) {
    var length = this.length;
    if (!length) {
        return -1;
    }

    startIndex = startIndex || 0;
    count = count || length;

    var endIndex = startIndex + count - 1;
    if (endIndex >= length) {
        endIndex = length - 1;
    }

    for (var i = startIndex; i <= endIndex; i++) {
        if (chars.indexOf(this.charAt(i)) >= 0) {
            return i;
        }
    }
    return -1;
}

String.prototype.contains = function String$contains(string) {
    return this.indexOf(string) > -1;
};

String.prototype.insert = function String$insert(index, value) {
    if (!value) {
        return this;
    }
    if (!index) {
        return value + this;
    }
    var s1 = this.substr(0, index);
    var s2 = this.substr(index);
    return s1 + value + s2;
}

String.isNullOrEmpty = function String$isNullOrEmpty(s) {
    return !s || !s.length;
}

String.prototype.lastIndexOfAny = function String$lastIndexOfAny(chars, startIndex, count) {
    var length = this.length;
    if (!length) {
        return -1;
    }

    startIndex = startIndex || length - 1;
    count = count || length;

    var endIndex = startIndex - count + 1;
    if (endIndex < 0) {
        endIndex = 0;
    }

    for (var i = startIndex; i >= endIndex; i--) {
        if (chars.indexOf(this.charAt(i)) >= 0) {
            return i;
        }
    }
    return -1;
}

String.localeFormat = function String$localeFormat(format) {
    return String._format(format, arguments, /* useLocale */ true);
}

String.prototype.padLeft = function String$padLeft(totalWidth, ch) {
    if (this.length < totalWidth) {
        ch = ch || ' ';
        return String.fromChar(ch, totalWidth - this.length) + this;
    }
    return this;
}

String.prototype.padRight = function String$padRight(totalWidth, ch) {
    if (this.length < totalWidth) {
        ch = ch || ' ';
        return this + String.fromChar(ch, totalWidth - this.length);
    }
    return this;
}

String.prototype.quote = function String$quote() {
    if (!String._quoteMap) {
        String._quoteMap = { '\\' : '\\\\',
                             '\'' : '\\\'', '"' : '\\"',
                             '\r' : '\\r', '\n' : '\\n', '\t' : '\\t', '\f' : '\\f',
                             '\b' : '\\b' };
    }
    if (!String._quoteRE || !RegExp._cacheable) {
        String._quoteRE = new RegExp("([\'\"\\\\\x00-\x1F\x7F-\uFFFF])", "g");
    }

    var s = this;
    if (String._quoteSkipTest || String._quoteRE.test(s)) {
        s = this.replace(String._quoteRE,
                         function String$quote$replace(str, m) {
                             var c = String._quoteMap[m];
                             if (c) {
                                 return c;
                             }
                             c = m.charCodeAt(0);
                             return '\\u' + c.toString(16).toUpperCase().padLeft(4, '0');
                         });
    }
    return '"' + s + '"';
}

String.prototype.remove = function String$remove(index, count) {
    if (!count || ((index + count) > this.length)) {
        return this.substr(0, index);
    }
    return this.substr(0, index) + this.substr(index + count);
}

/*String.prototype._replace = String.prototype.replace;
String.prototype.replace = function String$replace(oldValue, newValue) {
    if (oldValue.constructor == String) {
        newValue = newValue || '';
        return this.split(oldValue).join(newValue);
    }
    return String.prototype._replace.call(this, oldValue, newValue);
}*/

String.prototype.replaceAll = function String$replaceAll(oldValue, newValue) {
    if (oldValue.constructor != String) {
        throw new Error('Argument exception');
    }
    
    return this.replace(new RegExp(RegExp.escape(oldValue), "g"), newValue);
}

String.prototype.startsWith = function String$startsWith(prefix) {
    if (!prefix.length) {
        return true;
    }
    if (prefix.length > this.length) {
        return false;
    }
    return (this.substr(0, prefix.length) == prefix);
}

String.prototype.trim = function String$trim() {
    return this.trimEnd().trimStart();
}

String.prototype.trimEnd = function String$trimEnd() {
    return this.replace(/\s*$/, '');
}

String.prototype.trimStart = function String$trimStart() {
    return this.replace(/^\s*/, '');
}

String.prototype.unquote = function String$unquote() {
    return eval('(' + this + ')');
}

String.prototype.toCamelCase = function String$toCamelCase() {
    return String.isNullOrEmpty(this) ?
           this :
           this.charAt(0).toLowerCase() + this.substring(1, this.length);
}

String.prototype.toPascalCase = function String$toPascalCase() {
    return String.isNullOrEmpty(this) ?
           this :
           this.charAt(0).toUpperCase() + this.substring(1, this.length);
};

String.prototype.toNumber = function() {
    return this * 1;
};

String.prototype.toBoolean = function() {
    if (this == 'true') 
        return true;
    if (this == 'false')
        return false;
    throw new Error('Cannot parse bool. Valid values are "true", "false"');
};

String.prototype.hashCode = function() {
    var hash = 0;

	if (this.length == 0) {
        return code;
    }

	for (i = 0; i < this.length; i++) {
		chr = this.charCodeAt(i);
		hash = 31*hash + chr;
		hash = hash & hash;
	}

	return hash;    
};

String.leftPad = function (val, size, ch) {
    var result = new String(val);
    
    if (ch === null || ch === undefined || ch === '') {
        ch = " ";
    }
    
    while (result.length < size) {
        result = ch + result;
    }
    
    return result;
};
///////////////////////////////////////////////////////////////////////////////
// Boolean Extensions

Boolean.parse = function(str) {
    if (isBoolean(str))
        return str;

    if (str === 'true') 
        return true;

    if (str === 'false')
        return false;
    
    throw new Error('Cannot parse bool. Valid values are "true", "false"');
};

Date.__typeName = 'Date';

Date.get_now = function Date$get_now() {
    return new Date();
}

Date.get_today = function Date$get_today() {
    var d = new Date();
    return new Date(d.getFullYear(), d.getMonth(), d.getDate());
}

Date.prototype.format = function Date$format(format, useLocale) {
    if (isNullOrUndefined(format) ||
        (format.length == 0) || (format == 'i')) {
        if (useLocale) {
            return this.toLocaleString();
        }
        else {
            return this.toString();
        }
    }
    if (format == 'id') {
        if (useLocale) {
            return this.toLocaleDateString();
        }
        else {
            return this.toDateString();
        }
    }
    if (format == 'it') {
        if (useLocale) {
            return this.toLocaleTimeString();
        }
        else {
            return this.toTimeString();
        }
    }

    return this._netFormat(format, useLocale);
}

Date.prototype._netFormat = function Date$_netFormat(format, useLocale) {
    var dtf = useLocale ? CultureInfo.Current.dateFormat : CultureInfo.Neutral.dateFormat;
    var useUTC = false;

    if (format.length == 1) {
        switch (format) {
            case 'f': format = dtf.longDatePattern + ' ' + dtf.shortTimePattern;
            case 'F': format = dtf.dateTimePattern; break;

            case 'd': format = dtf.shortDatePattern; break;
            case 'D': format = dtf.longDatePattern; break;

            case 't': format = dtf.shortTimePattern; break;
            case 'T': format = dtf.longTimePattern; break;

            case 'g': format = dtf.shortDatePattern + ' ' + dtf.shortTimePattern; break;
            case 'G': format = dtf.shortDatePattern + ' ' + dtf.longTimePattern; break;

            case 'R': case 'r': format = dtf.gmtDateTimePattern; useUTC = true; break;
            case 'u': format = dtf.universalDateTimePattern; useUTC = true; break;
            case 'U': format = dtf.dateTimePattern; useUTC = true; break;

            case 's': format = dtf.sortableDateTimePattern; break;
        }
    }

    if (format.charAt(0) == '%') {
        format = format.substr(1);
    }

    if (!Date._formatRE) {
        Date._formatRE = /dddd|ddd|dd|d|MMMM|MMM|MM|M|yyyy|yy|y|hh|h|HH|H|mm|m|ss|s|tt|t|fff|ff|f|zzz|zz|z/g;
    }

    var re = Date._formatRE;    
    var sb = new StringBuilder();
    var dt = this;
    if (useUTC) {
        dt = new Date(Date.UTC(dt.getUTCFullYear(), dt.getUTCMonth(), dt.getUTCDate(),
                               dt.getUTCHours(), dt.getUTCMinutes(), dt.getUTCSeconds(), dt.getUTCMilliseconds()));
    }

    re.lastIndex = 0;
    while (true) {
        var index = re.lastIndex;
        var match = re.exec(format);

        sb.append(format.slice(index, match ? match.index : format.length));
        if (!match) {
            break;
        }

        var fs = match[0];
        var part = fs;
        switch (fs) {
            case 'dddd':
                part = dtf.dayNames[dt.getDay()];
                break;
            case 'ddd':
                part = dtf.shortDayNames[dt.getDay()];
                break;
            case 'dd':
                part = dt.getDate().toString().padLeft(2, '0');
                break;
            case 'd':
                part = dt.getDate();
                break;
            case 'MMMM':
                part = dtf.monthNames[dt.getMonth()];
                break;
            case 'MMM':
                part = dtf.shortMonthNames[dt.getMonth()];
                break;
            case 'MM':
                part = (dt.getMonth() + 1).toString().padLeft(2, '0');
                break;
            case 'M':
                part = (dt.getMonth() + 1);
                break;
            case 'yyyy':
                part = dt.getFullYear();
                break;
            case 'yy':
                part = (dt.getFullYear() % 100).toString().padLeft(2, '0');
                break;
            case 'y':
                part = (dt.getFullYear() % 100);
                break;
            case 'h': case 'hh':
                part = dt.getHours() % 12;
                if (!part) {
                    part = '12';
                }
                else if (fs == 'hh') {
                    part = part.toString().padLeft(2, '0');
                }
                break;
            case 'HH':
                part = dt.getHours().toString().padLeft(2, '0');
                break;
            case 'H':
                part = dt.getHours();
                break;
            case 'mm':
                part = dt.getMinutes().toString().padLeft(2, '0');
                break;
            case 'm':
                part = dt.getMinutes();
                break;
            case 'ss':
                part = dt.getSeconds().toString().padLeft(2, '0');
                break;
            case 's':
                part = dt.getSeconds();
                break;
            case 't': case 'tt':
                part = (dt.getHours() < 12) ? dtf.amDesignator : dtf.pmDesignator;
                if (fs == 't') {
                    part = part.charAt(0);
                }
                break;
            case 'fff':
                part = dt.getMilliseconds().toString().padLeft(3, '0');
                break;
            case 'ff':
                part = dt.getMilliseconds().toString().padLeft(3).substr(0, 2);
                break;
            case 'f':
                part = dt.getMilliseconds().toString().padLeft(3).charAt(0);
                break;
            case 'z':
                part = dt.getTimezoneOffset() / 60;
                part = ((part >= 0) ? '-' : '+') + Math.floor(Math.abs(part));
                break;
            case 'zz': case 'zzz':
                part = dt.getTimezoneOffset() / 60;
                part = ((part >= 0) ? '-' : '+') + Math.floor(Math.abs(part)).toString().padLeft(2, '0');
                if (fs == 'zzz') {
                    part += dtf.timeSeparator + Math.abs(dt.getTimezoneOffset() % 60).toString().padLeft(2, '0');
                }
                break;
        }
        sb.append(part);
    }

    return sb.toString();
}

Date.$__parse = Date.parse;
Date.parse = function Date$parse(s) {
    // Date.parse by default returns the number of milliseconds
    // and not an actual Date instance
    return new Date(Date.$__parse(s));
}

Date.parseWcf = function Date$parseWcf(s) {
    if (isNullOrUndefined(s))
        return s; 

    var format = /.*?Date\((-?[0-9]+?)(?:[\-\+])([0-9]{2})([0-9]{2})\).*?/;
    var parsed = format.exec(s);
    
    if (!parsed[1] || !parsed[2] || !parsed[3]) {
        throw new Error('[Date] Cannot create Date object from "' + s + '".');
    }
    
    var d = new Date();
    
    /*var newOffset = parsed[2].toNumber() * 60 + parsed[3].toNumber();
    var currentOffset = -d.getTimezoneOffset();*/
    
    d.setTime(parsed[1].toNumber()/* + (currentOffset - newOffset) * 60 * 1000*/);
    
    return d;
}

Date.prototype.toWcfString = function Date$toWcfString() {
    return '\\/Date(' + this.getTime() + this.getGMTOffset() + ')\\/';
};


Date.prototype.getGMTOffset = function() {
    var offset = this.getTimezoneOffset();
    return (offset > 0 ? "-" : "+")
        + String.leftPad(Math.abs(Math.floor(offset / 60)), 2, "0")
        + String.leftPad(offset % 60, 2, "0");
};

Date.prototype.isValid = function Date$isValid() {
    return !isNaN(this.getTime());
};
/// <reference path="Inheritance.js" />

var CultureInfo = function(name, numberFormat, dateFormat) {
    this.name = name;
    this.numberFormat = numberFormat;
    this.dateFormat = dateFormat;
}
CultureInfo.createClass('CultureInfo');

CultureInfo.Neutral = new CultureInfo('en-US',
    {
        naNSymbol: 'NaN',
        negativeSign: '-',
        positiveSign: '+',
        negativeInfinityText: '-Infinity',
        positiveInfinityText: 'Infinity',
        
        percentSymbol: '%',
        percentGroupSizes: [3],
        percentDecimalDigits: 2,
        percentDecimalSeparator: '.',
        percentGroupSeparator: ',',
        percentPositivePattern: '{0} %',
        percentNegativePattern: '-{0} %',

        currencySymbol:'$',
        currencyGroupSizes: [3],
        currencyDecimalDigits: 2,
        currencyDecimalSeparator: '.',
        currencyGroupSeparator: ',',
        currencyNegativePattern: '(${0})',
        currencyPositivePattern: '${0}',

        numberGroupSizes: [3],
        numberDecimalDigits: 2,
        numberDecimalSeparator: '.',
        numberGroupSeparator: ','
    },
    {
        amDesignator: 'AM',
        pmDesignator: 'PM',

        dateSeparator: '/',
        timeSeparator: ':',

        gmtDateTimePattern: 'ddd, dd MMM yyyy HH:mm:ss \'GMT\'',
        universalDateTimePattern: 'yyyy-MM-dd HH:mm:ssZ',
        sortableDateTimePattern: 'yyyy-MM-ddTHH:mm:ss',
        dateTimePattern: 'dddd, MMMM dd, yyyy h:mm:ss tt',

        longDatePattern: 'dddd, MMMM dd, yyyy',
        shortDatePattern: 'dd-MMM-yy',

        longTimePattern: 'hh:mm:ss tt',
        shortTimePattern: 'hh:mm',

        firstDayOfWeek: 0,
        dayNames: ['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'],
        shortDayNames: ['Sun','Mon','Tue','Wed','Thu','Fri','Sat'],
        minimizedDayNames: ['Su','Mo','Tu','We','Th','Fr','Sa'],

        monthNames: ['January','February','March','April','May','June','July','August','September','October','November','December',''],
        shortMonthNames: ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','']
    });
    
CultureInfo.Current = CultureInfo.Neutral;
$get = function(id) {
    return document.getElementById(id);
};

var DOM = {
    _getComputedStyle: (document.defaultView && document.defaultView.getComputedStyle),
    _rdashAlpha: /-([a-z])/ig,
    _fcamelCase: function(all, letter) {
        return letter.toUpperCase();
    },
    
    jqGC: null,

    discardElement: function(element) {
        var jqGC = DOM.jqGC;

        if (!DOM.jqGC) {
            jqGC = document.createElement('div');
            jqGC.style.display = 'none';
            document.body.appendChild(jqGC);
            DOM.jqGC = jqGC;
        }
        
        var tagName = element.tagName;

        if (element.nodeType == 1 && tagName != 'TR' && tagName != 'TD') {            

            if (element.parentNode) {
                element.parentNode.removeChild(element);
            }
            
            element.innerHTML = '';
        } else {
            jqGC.appendChild(element);
            jqGC.innerHTML = '';
        }
    },

    remove: function(element) {
        if (!element) {
            return;
        }

        // Prevent memory leaks
        jQuery("*", element).each(function() {
            jQuery.event.remove(this);
            jQuery.removeData(this);
        });

        jQuery.event.remove(element);
        jQuery.removeData(element);
            
        // deleting of text nodes which been detached from a parent in IE may cause an error
        try {
            if (element.nodeType != 1 || element.parentNode)
                DOM.discardElement(element);
        }
        catch (e) { }
    },

    _nodeCaches: {},

    /**
    * Creates a new DOM node by tag. If parent node parameter is passed then created node will be appended to parent node.
    */
    create: function(tag, parentNode, params) {
        var time = new Date();
        var node;
        
        if (!params) {
            if (!DOM._nodeCaches[tag]) {
                DOM._nodeCaches[tag] = document.createElement(tag);
            }
            
            node = DOM._nodeCaches[tag].cloneNode(false);
        } else {
            node = document.createElement(tag);
        }
        
        if (parentNode) {
            parentNode.appendChild(node);
        }

        if (params) {
            for (var prop in params) {
                if (params.hasOwnProperty(prop)) {
                    node[prop] = params[prop];
                }
            }
        }

        return node;
    },
    
    appendChild: function(node, parent) {
        parent.appendChild(node);
    },

    /**
    * Removes a node from its parent
    */
    detachNode: function(node) {
        // if node is not detached yet
        if (node.parentNode) {
            node.parentNode.removeChild(node);
        }
    },

    /**
    * Creates a new text node in DOM and append it to parent node if it is passed.
    */
    createTextNode: function(value, parentNode) {
        var node = document.createTextNode(value || "");
        
        if (parentNode) {
            parentNode.appendChild(node);
        }
        
        return node;
    },

    disableSelection: function(target) {
        if (typeof (target.onselectstart) != "undefined") { //IE
            target.onselectstart = function() { return false; };
        }
        else if (typeof (target.style.MozUserSelect) != "undefined") { //Firefox route
            target.style.MozUserSelect = "none";
        }
        else { //All other route (ie: Opera)
            $("*", target).each(function() {
                this.unselectable = "on";
            });
        }
    },
    
    // a lite, a bit faster and a more buggy version of jquery curCss
    getCssProperty: function(elem, prop, camelCase) {
        var currentStyle = elem.currentStyle;
        
        if (currentStyle) {
            var camelCase = camelCase || prop.replace(DOM._rdashAlpha, DOM._fcamelCase);
            return currentStyle[camelCase] || currentStyle[prop];
        } else if (DOM._getComputedStyle) {
            var computedStyle = elem.ownerDocument.defaultView.getComputedStyle(elem, null);

            if (computedStyle) {
                return computedStyle.getPropertyValue(prop);
            }
        }
    },
    
    getPaddingHeight: function(elem) {
        var paddingTop = DOM.getCssProperty(elem, "padding-top", "paddingTop");
        var paddingBottom = DOM.getCssProperty(elem, "padding-bottom", "paddingBottom");

        return paddingTop.substr(0, paddingTop.length - 2) * 1 + paddingBottom.substr(0, paddingBottom.length - 2) * 1;
    },

    getPaddingWidth: function(elem) {
        var paddingLeft = DOM.getCssProperty(elem, "padding-left", "paddingLeft");
        var paddingRight = DOM.getCssProperty(elem, "padding-right", "paddingRight");

        return paddingLeft.substr(0, paddingLeft.length - 2) * 1 + paddingRight.substr(0, paddingRight.length - 2) * 1;
    },

    /**
    * Returns height of the margin. If margin height is negative then return zero.
    */
    getMarginHeight: function(elem) {
        var marginTop = DOM.getCssProperty(elem, "margin-top", "marginTop");
        var marginBottom = DOM.getCssProperty(elem, "margin-bottom", "marginBottom");

        return Math.max(marginTop.substr(0, marginTop.length - 2) * 1 + marginBottom.substr(0, marginBottom.length - 2) * 1, 0);
    },

    /**
    * Returns width of the margin. If margin width is negative then return zero.
    */
    getMarginWidth: function(elem) {
        var marginLeft = DOM.getCssProperty(elem, "margin-left", "marginLeft");
        var marginRight = DOM.getCssProperty(elem, "margin-right", "marginRight");

        return Math.max(marginLeft.substr(0, marginLeft.length - 2) * 1 + marginRight.substr(0, marginRight.length - 2) * 1, 0);
    },

    getBorderHeight: function(elem) {
        var borderTop = DOM.getCssProperty(elem, "border-top-width", "borderTopWidth");
        var borderBottom = DOM.getCssProperty(elem, "border-bottom-width", "borderBottomWidth");

        return borderTop.substr(0, borderTop.length - 2) * 1 + borderBottom.substr(0, borderBottom.length - 2) * 1;
    },

    getBorderWidth: function(elem) {
        var borderLeft = DOM.getCssProperty(elem, "border-left-width", "borderLeftWidth");
        var borderRight = DOM.getCssProperty(elem, "border-right-width", "borderRightWidth");

        return borderLeft.substr(0, borderLeft.length - 2) * 1 + borderRight.substr(0, borderRight.length - 2) * 1;
    },
    
    setPosition: function(node, pos) {
        var elStyle = node.style;

        elStyle.left = isNullOrUndefined(pos.x) ? "" : (pos.x + "px");
        elStyle.top = isNullOrUndefined(pos.y) ? "" : (pos.y + "px");
    },

    get_scrollHeight: function(item) {
        if ($.browser.opera) {
            return item.scrollHeight + DOM.getPaddingHeight(item);
        }

        return item.scrollHeight;
    },

    get_scrollWidth: function(item) {
        if ($.browser.opera) {
            return item.scrollWidth + DOM.getPaddingWidth(item);
        }

        return item.scrollWidth;
    },

    get_childsWidth: function(elem) {
        var childs = elem.childNodes;
        var result = 0;

        for (var i = 0; i < childs.length; i++) {
            result += childs[i].offsetWidth || 0;
        }

        return result;
    },

    get_childsHeight: function(elem) {
        var childs = elem.childNodes;
        var result = 0;

        for (var i = 0; i < childs.length; i++) {
            result += childs[i].offsetHeight || 0;
        }

        return result;
    },

    /**
    * Sets the bounding rectangle for some DOM node.
    * @param HTMLElement node
    * @param int width
    * @param int height
    */
    setBoundingRect: function(node, width, height) {
        var elStyle = node.style;

        elStyle.width = (isNullOrUndefined(width) || width < 0) ? "" : (width + "px");
        elStyle.height = (isNullOrUndefined(height) || height < 0) ? "" : (height + "px");
    },

    support: {
        cssFloat: jQuery.support.cssFloat
    },
    
    setFloat: function(style, floatValue) {
        var cssFloat = DOM.support.cssFloat;

        if (cssFloat) {
            style.cssFloat = floatValue;
        } else {
            style.styleFloat = floatValue;
        }
    },

    /**
    * Inserts the new node after the reference node
    * @param HTMLElement referenceNode
    * @param HTMLElement newNode
    */
    insertAfter: function(referenceNode, newNode) {
        referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
    },
    
    /**
    * Inserts the new node before the reference node
    * @param HTMLElement referenceNode
    * @param HTMLElement newNode
    */
    insertBefore: function(referenceNode, newNode) {
        referenceNode.parentNode.insertBefore(newNode, referenceNode);
    },
    
    /**
    * Inserts the new node after or before (determined by type) the reference node
    * @param String type
    * @param HTMLElement referenceNode
    * @param HTMLElement newNode
    */
    insert: function(type, referenceNode, newNode) {
        if (type == 'after') {
            DOM.insertAfter(referenceNode, newNode);
        } else if (type == 'before') {
            DOM.insertBefore(referenceNode, newNode);
        } else {
            throw new Error('Unknown type of insertion');
        }
    },

    isFocusable: function(node) {
        return isDefined(node.tabIndex) && node.tabIndex >= 0;
    },

    isTextNode: function(node) {
        return node.nodeType == 3 || (node.childNodes.length == 1 && node.childNodes[0].nodeType == 3);
    }
};
var QueueProcessor = function(callback) {
    if (callback) {
        this.add_processed(callback, this);
    }

    this._queueList = {};
    this._queueTimeouts = {};
};

QueueProcessor.prototype = {
    _queueList: null,
    _queueTimeouts: null,

    add: function(queueId, args) {
        if (isNullOrUndefined(args)) {
            throw new Error('Args is null or undefined');
        }

        if (!this._queueList[queueId]) {
            this._queueList[queueId] = [];
        }
        
        this._queueList[queueId].add(args);
        this._checkQueue(queueId);
    },

    clear: function() {
        for (var timeoutId in this._queueTimeouts) {
            if (this._queueTimeouts.hasOwnProperty(timeoutId)) {
                clearTimeout(this._queueTimeouts[timeoutId]);
            }
        }

        this._queueList = {};
        this._queueTimeouts = {};
    },

    force: function(queueId) {
        this._clearTimeout(queueId);
        var queue = this._queueList[queueId];
            
        if (!queue || queue.length === 0) {
            return;
        }
            
        this._queueList[queueId] = [];
            
        this.raise_processed({ queueId: queueId, data: queue });
    },

    _clearTimeout: function(queueId) {
        if (this._queueTimeouts[queueId]) {
            clearTimeout(this._queueTimeouts[queueId]);
            this._queueTimeouts[queueId] = null;
        }
    },

    __getTimeoutFunction: function(queueId) {
        return function() {
            this.force(queueId);
        }.bind(this);
    },
    
    _checkQueue: function(queueId) {
        this._clearTimeout(queueId);
        this._queueTimeouts[queueId] = setTimeout(this.__getTimeoutFunction(queueId), 0);
    }
};

Auto.Events(QueueProcessor.prototype, [
    'processed'
]);

QueueProcessor.createClass('QueueProcessor');
var DTOProcessor = function() {

};

Object.extend(DTOProcessor.prototype, {
    toEntity: function(dto, forceRewrite) {
        if (forceRewrite) {
            dto.$__forceRewrite = true;
        }
        
        return this._getEntityFrom(dto);
    },

    toEntityCollection: function(dto, forceRewrite) {
        var convertedEntities = [];        
        for (var i = 0; i < dto.length; i++) {
            convertedEntities.add(this.toEntity(dto[i]));
        }    

        return convertedEntities;
    },

    getEntityType: function(dto) {
        var match = dto.__type.match(/^(\w+):#/);
        if (match.length < 2)
            return null;

        return Type.getType(match[1]);
    },

    _getEntityFrom: function(dto, attachingInstance) {
        var type = this.getEntityType(dto);
        var entityTypeName = type.get_name();

        var entity;
        
        if (dto._id)
            entity = Repository.GetCached(entityTypeName, dto._id) || attachingInstance;

        if (!entity)
            entity = this._createEntity(dto);
        else if (entity._version !== dto._version || dto.$__forceRewrite)
            this.copyFromDTO(entity, dto);

        if (dto._id)
            Repository._addToCache(entity);

        return entity;
    },

    _createEntity: function(dto) {        
        var typeName = this.getEntityType(dto).get_name();
        var entity = eval('(new ' + typeName + '())');

        this.copyFromDTO(entity, dto);

        return entity;
    },

    copyFromDTO: function(entity, dto, attachToRepository) {
        for (var property in dto) {
            if (property === '__type' || property === '_guid' || property === '$__forceRewrite')
                continue;

            var dtoProperty = dto[property];

            var setterName = entity['set' + property] ? 'set' + property : '_set' + property;

            if (!entity[setterName] || typeof (entity[setterName]) !== 'function')
                throw new Error('Can\'t copy field ' + property + ' from DTO in class ' + entity.get_type().get_name() + '. Setter not found.');
            else if (dtoProperty && dtoProperty.__type && Tracking.isTrackingDto(dtoProperty)) {
                this._processTrackingCollection(entity['get' + property](null, false), dto[property], attachToRepository);
            }
            else if (dtoProperty instanceof Array) {
                var target = entity['get' + property]();
                if (!target)
                    throw new Error('Array ' + property + ' in ' + entity.get_type().get_name() + ' must be initialized in constructor.')

                this._processArray(target, dto[property]);
            }
            else if (dtoProperty && dtoProperty.__type) {
                var converted = this._getEntityFrom(dto[property], attachToRepository ? entity['get' + property]() : null);

                entity[setterName](converted);
            }
            else if (entity.__enums[property]) {
                entity[setterName](dto[property] !== null ? entity.__enums[property].withValue(dto[property]) : null);
            }
            else if (entity.__dates[property]) {
                entity[setterName](Date.parseWcf(dto[property]));
            }
            else if (dtoProperty == null && entity['get' + property](null, false) && entity['get' + property](null, false).__isEntitySet) {
            }
            else
                entity[setterName](dtoProperty);
        }
    },

    _processTrackingCollection: function(target, sourceDto, attachItems) {        
        var converted = [];
        
        if(target.__isDataSource || target.__isEntitySet) {
            converted = sourceDto.items.select(function(item) {
                var attachingInstance = null;
                if (attachItems) {
                    attachingInstance = target.items.singleOrDefault(function(targetItem) {
                        return targetItem._id === item._id || targetItem._guid === item._guid;
                    }) || target.added.singleOrDefault(function(targetItem) {
                        return targetItem._id === item._id;
                    });
                }

                return this._getEntityFrom(item, attachingInstance);
            }, this);
        } else if (target.__isEnumCollection) {
            converted = sourceDto.items.select(function(item) {
                return target.__enumType.withValue(item);
            });
        } else
            converted = sourceDto.items;
        
        converted.forEach(function(item) {
            if(!target.items.contains(item) && !target.removed.contains(item))
                target.add(item, {notTrackable: true});
                
            if(target.added.contains(item))
                target.added.remove(item);
        });
        
        target.removed.remove(target.removed.where(function(item) {
            return !converted.contains(item);
        }));
        
        target.remove(target.items.where(function(item) {
            return !converted.contains(item) && !target.added.contains(item);
        }), {notTrackable: true});
    },

    _processArray: function(target, sourceDto, attachItems, args) {    
        if (sourceDto.length === 0) {
            target.clear();
            return;
        }
        if (target.__isDataSource || target.__isEntitySet) {
            var converted = sourceDto.select(function(item) {
                var attachingInstance = null;
                if (attachItems) {
                    attachingInstance = target[sourceDto.indexOf(item)]
                }

                return this._getEntityFrom(item, attachingInstance);
            }, this);

            target.synchronize(converted, null, args);
        }
        else if (target.__isEnumCollection) {
            var converted = sourceDto.select(function(item) {
                return target.__enumType.withValue(item);
            });

            target.synchronize(converted, null, args);
        }
        else {
            target.synchronize(sourceDto, null, args);
        }
    }
}); 
Repository =
{
    registerEntityType: function (entityType) {
        this.__cache[entityType] = {};

        this['Load' + entityType] = (function (p) {
            return function (id, onSuccess, context) {
                this.Load(p, id, onSuccess, context);
            };
        })(entityType);

        this['Filter' + entityType] = (function (p) {
            return function (filter, onSuccess, context) {
                this.Filter(p, filter, onSuccess, context);
            };
        })(entityType);

        Auto.Events(Repository, [
            entityType.toCamelCase() + 'Saved',
            entityType.toCamelCase() + 'Loaded',
            entityType.toCamelCase() + 'Deleted'
        ]);
    },

    __cache: {},

    _addToCache: function(entity) {
        var entityTypeName = entity.get_type().get_name();
        var id = entity.get_id();

        Repository.__cache[entityTypeName][id] = entity;
    },

    Abort: function(rid) {
        if (Repository._dataService.abort(rid)) {
            Repository.raise_processed();
        }
    },

    Load: function (entityTypeName, id, onSuccess, context) {
        Repository.raise_processing();

        this._dataService.invoke(entityTypeName, 'Load',
            function (dto) {
                if (dto) {
                    var entity = this._dtoProcessor.toEntity(dto, true);
                    this._raiseEntityEvent(entityTypeName, [entity], 'Loaded', context);
                }

                if (onSuccess)
                    onSuccess(entity);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), { id: id });
    },

    _reloadQueue: new QueueProcessor(function (sender, args) {
        var data = {};
        var ids = [];
        var entityTypeName = args.queueId;

        for (var i = 0; i < args.data.length; i++) {
            var entityId = args.data[i].entity.get_id() * 1;
            data[entityId] = args.data[i];
            ids.add(entityId);
        }

        if (ids.length == 0) {
            return;
        }

        Repository._dataService.invoke(entityTypeName, 'Filter', function (dto) {
            var result = [];

            for (var i = 0; i < dto.length; i++) {
                var entity = data[dto[i]._id * 1].entity;
                var callback = data[dto[i]._id * 1].callback;

                this._dtoProcessor.copyFromDTO(entity, dto[i], true);

                if (callback)
                    callback(entity);
            }

            Repository.raise_processed();
        } .bind(Repository), function (error) {
            Repository.raise_processed();
            Repository.raise_error(error);
        }, {
            filter: "Id IN (" + ids.join(',') + ")",
            searchQuery: null,
            orderBy: null,
            page: null,
            pageSize: null
        });
    }),

    Reload: function (entity, onSuccess) {
        var entityTypeName = entity.rootType;
        var id = entity.get_id();

        this._reloadQueue.add(entityTypeName, { entity: entity, callback: onSuccess });
    },

    _deepLoadQueue: new QueueProcessor(function (sender, args) {
        var data = {};
        var ids = [];
        var entityTypeName = args.queueId;

        for (var i = 0; i < args.data.length; i++) {
            var entityId = args.data[i].entity.get_id() * 1;

            if (isNaN(entityId)) {
                throw new Error('Wrong identifier of entity');
            }

            if (!data[entityId]) {
                data[entityId] = [];
                ids.add(entityId);
            }

            data[entityId].add(args.data[i]);
        }

        if (ids.length == 0) {
            return;
        }

        Repository.raise_processing();

        Repository._dataService.invoke(entityTypeName, 'BatchDeepLoad', function (dto) {
            for (var i = 0; i < dto.length; i++) {
                var dtoId = dto[i]._id * 1;
                var entity = data[dtoId][0].entity;

                this._dtoProcessor.copyFromDTO(entity, dto[i], true);

                for (var j = 0; j < data[dtoId].length; j++) {
                    var callback = data[dtoId][j].callback;

                    if (callback)
                        callback(entity);
                }
            }

            Repository.raise_processed();
        } .bind(Repository), function (error) {
            Repository.raise_processed();
            Repository.raise_error(error);
        }, {
            ids: ids
        });
    }),

    DeepLoad: function (entity, onSuccess) {
        var entityTypeName = entity.rootType;
        var id = entity.get_id() * 1;

        if (isNaN(id)) {
            throw new Error('Wrong identifier of entity');
        }

        this._deepLoadQueue.add(entityTypeName, { entity: entity, callback: onSuccess });
    },

    Save: function (data, onSuccess, context, entityTypeName, deepLevel) {
        var batchSave = data instanceof Array;

        if (batchSave && data.length === 0)
            throw new Error('Trying to save empty collection');

        Repository.raise_processing();

        var entityTypeName = isNullOrUndefined(entityTypeName) ? (batchSave ? data[0].get_type().get_name() : data.get_type().get_name()) : entityTypeName; // TODO: there is may be problem when batch contains entities of different types

        this._dataService.invoke(entityTypeName, batchSave ? 'BatchSave' : 'Save',
            function (dto) {
                var result = [];

                var processFunc = function (entity, dto) {
                    this._dtoProcessor.copyFromDTO(entity, dto, true);
                    Repository._addToCache(entity);

                    result.add(entity);
                } .bind(this);

                if (batchSave) {
                    for (var i = 0; i < dto.length; i++)
                        processFunc(data[i], dto[i]);
                } else
                    processFunc(data, dto);

                this._raiseEntityEvent(entityTypeName, result, 'Saved', context);

                if (onSuccess)
                    onSuccess(data, context);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), batchSave ? { entities: data} : { entity: data }, isDefined(deepLevel) ? deepLevel : 1);
    },

    __raiseDeletedEvent: function (entity, context) {
        if (isFunction(entity.onDeleting)) {
            entity.onDeleting();
        }

        //++110310_Aykaev, raise deleted event for dependent sets
        if (entity.__dependentSets && entity.__dependentSets.length > 0) {
            for (var i = 0; i < entity.__dependentSets.length; i++) {
                var setName = entity.__dependentSets[i];
                var dependentEntities = entity['get_' + setName]();

                for (var j = dependentEntities.length - 1; j >= 0; j--) {
                    this.__raiseDeletedEvent(dependentEntities[j], context);
                }
            }
        }
        //--110310_Aykaev

        entity.dispose();
        var entityTypeName = entity.get_type().get_name();
        Repository._removeFromCache(entityTypeName, entity._id);

        this._raiseEntityEvent(entityTypeName, [entity], 'Deleted', context);
    },

    Delete: function (entity, onSuccess, context) {
        Repository.raise_processing();

        this._dataService.invoke(entity.get_type().get_name(), 'Delete',
            function () {
                this.__raiseDeletedEvent(entity, context);

                if (onSuccess)
                    onSuccess();

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), { entity: entity }, 0);
    },

    DeleteById: function (entityType, id, onSuccess, context) {
        Repository.raise_processing();

        this._dataService.invoke(entityType, 'DeleteById',
            function () {                
                if (onSuccess)
                    onSuccess();

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), { id: id }, 0);
    },

    Filter: function (entityTypeName, args, onSuccess, context) {        
        Repository.raise_processing();

        return this._dataService.invoke(entityTypeName, 'Filter',
            function (dto) {
                var result = [];

                for (var i = 0; i < dto.length; i++) {
                    var entity = this._dtoProcessor.toEntity(dto[i]);
                    result.add(entity);
                }

                this._raiseEntityEvent(entityTypeName, result, 'Loaded', context);

                if (onSuccess)
                    onSuccess(result, context);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this),
                typeof args === 'string' ?
                {
                    filter: args,
                    searchQuery: null,
                    orderBy: null,
                    page: null,
                    pageSize: null
                } :
                args
            );
    },

    Count: function (entityTypeName, args, onSuccess) {
        Repository.raise_processing();

        this._dataService.invoke(entityTypeName, 'Count',
            function (result) {
                if (onSuccess)
                    onSuccess(result);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), args);
    },

    GetCached: function (entityTypeName, id) {
        return this.__cache[entityTypeName][id];
    },

    _removeFromCache: function(entityTypeName, id) {
        delete this.__cache[entityTypeName][id];
    },

    FilterCached: function (entityTypeName, filter) {
        var result = [];

        var storage = this.__cache[entityTypeName];

        for (var prop in storage) {
            if (typeof storage[prop] === 'function')
                continue;

            if (filter(storage[prop]))
                result.add(storage[prop]);
        }

        return result;
    },

    _getQueue: new QueueProcessor(function (sender, args) {
        var queue = args.data;
        var entityTypeName = args.queueId;

        var notFounded = [];

        for (var i = 0; i < queue.length; i++) {
            notFounded.add(queue[i].notFounded);
        }

        if (notFounded.length === 0) {
            return;
        }

        Repository.Filter(entityTypeName, "Id In (" + notFounded.distinct().join(',') + ")", function (result) {
            for (var i = 0; i < queue.length; i++) {
                var callbackResult = [];

                for (var j = 0; j < result.length; j++) {
                    if (queue[i].notFounded.contains(result[j]._id)) {
                        callbackResult.add(result[j]);
                    }
                }

                if (queue[i].founded) {
                    callbackResult.add(queue[i].founded);
                }

                if (queue[i].callback) {
                    queue[i].callback(queue[i].singleMode
                                        ?
                                        (callbackResult.length > 0 ?
                                          callbackResult[0] : undefined)
                                        :
                                        callbackResult);
                }
            }
        });
    }),

    __Get: function (entityTypeName, id, callback, throwException) {
        if (isNullOrUndefined(id)) {
            throw new Error('Id is null or undefined');
        }

        var singleMode = !(id instanceof Array);

        if (singleMode) {
            id = [id];
        }

        var notFounded = [];
        var founded = [];

        for (var i = 0; i < id.length; i++) {
            var itemId = id[i]*1;
            var found = this.__cache[entityTypeName][itemId];

            if (found) {
                founded.add(found);
            } else {
                notFounded.add(itemId);
            }
        }

        if (notFounded.length == 0) {
            callback(singleMode ? founded[0] : founded);
            return;
        }

        var callbackWrapper = callback;
        
        if(throwException) {
            callbackWrapper = function(result) 
            {  
                if(result != null) {
                    callback(result);
                } else {
                    throw new Error('Cannot find \'' + entityTypeName + '\' with id: \'' + id + '\'');
                }
            };
        }

        this._getQueue.add(entityTypeName, {
            singleMode: singleMode,
            notFounded: notFounded.distinct(),
            founded: founded,
            callback: callbackWrapper
        });
    },

    Get: function(entityTypeName, id, callback) {
        this.__Get(entityTypeName, id, callback, true);
    },

    GetOrDefault: function(entityTypeName, id, callback) {
        this.__Get(entityTypeName, id, callback, false);
    },

    Clear: function () {
        for (var prop in this.__cache) {
            if (typeof this.__cache[prop] === 'function')
                continue;

            for (var p in this.__cache[prop]) {
                if (typeof this.__cache[prop][p] === 'function')
                    continue;

                delete this.__cache[prop][p];
            }
        }
    },

    Validate: function (data, onSuccess, onError, deepLevel) {
        Repository.raise_processing();

        var isBatch = data instanceof Array;

        if (isBatch) {
            throw new Error("Batch validation isn't supported.");
        }

        var entityTypeName = data.get_type().get_name();

        this._dataService.invoke(entityTypeName, "Validate",
            function (result) {
                if (result && result.length > 0 && onError) {
                    var processedResult = [];

                    for (var i = 0; i < result.length; i++) {
                        processedResult.add(result[i].__type ? this._dtoProcessor.toEntity(result[i]) : result[i]);
                    }

                    onError(processedResult);
                }

                if ((!result || !result.length) && onSuccess)
                    onSuccess();

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);

                if (onError)
                    onError(error);
            } .bind(this), { data: data }, isDefined(deepLevel) ? deepLevel : 1);
    },

    ValidateAndSave: function (data, onSuccess, onError, context) {
        Repository.Validate(data, function () {
            Repository.Save(data, onSuccess, context);
        }, function (error) {
            if (onError) {
                onError(error)
            }
        });
    },

    Method: function (entityTypeName, methodName, params, onSuccess) {
        Repository.raise_processing();

        this._dataService.invoke(entityTypeName, methodName,
            function (result) {
                if (result && result.__type) {
                    result = this._dtoProcessor.toEntity(result);
                    this._raiseEntityEvent(entityTypeName, [result], 'Loaded');
                }
                else if (result instanceof Array && result.length > 0 && result.first().__type) {
                    result = result.select(function (item) { return this._dtoProcessor.toEntity(item); }, this);
                    this._raiseEntityEvent(entityTypeName, result, 'Loaded');
                }

                if (onSuccess)
                    onSuccess(result);

                Repository.raise_processed();
            } .bind(this),
            function (error) {
                Repository.raise_processed();
                Repository.raise_error(error);
            } .bind(this), params);
    },

    _raiseEntityEvent: function (entityTypeName, entities, eventName, context) {
        if (entities.length > 0)
            this['raise_' + entityTypeName.toCamelCase() + eventName]({ entities: entities }, context);
    }
};

Auto.Properties(Repository, [
    'dtoProcessor',
    'dataService'
]);

Auto.Events(Repository, [
    'processing',
    'processed',
    'error'
]);

Repository.set_dtoProcessor(new DTOProcessor());
Repository.set_dataService(Services.DataService);
var Entity = function() {
    this.__type = this.get_type().get_name() + ':#Phoenix.WebServices.Contracts.Data';
    this._guid = Guid.New();
};

Entity.prototype = {
    set_id: function(value) {
        if(this._id === value)
            return;
        
        var oldValue = this._id;
        this._id = value;
        
        for(var i = 0; i < this.__dependentSets.length; i++) {            
            this['get_' + this.__dependentSets[i]](null, false).set_keyValue(value);
        }
        
        this.raise_idChanged({
                        newValue: value,
                        oldValue: oldValue
                    });
    },

    deepLoad: function(callback) {
        if (!this.__fullyLoaded) {
            Repository.DeepLoad(this, function() {
			    this.__fullyLoaded = true;

                if (callback)
                    callback.bind(this)();
		    }.bind(this));
        } else if (callback) {
            if (callback)
                callback.bind(this)();
        }
    },
    
    copyFrom: function(entity) {
        for (var property in entity) {
            if (this[property] && this[property].autoSetter && entity[property].autoSetter) {
                var getter = property.replace(property.startsWith('_set_') ? '_set_' : 'set_', 'get_');

                this[property](entity[getter]());
            }
        }
    },
    
    __dependentSets: [],
    
    isNew: function() {
        return !this._id;
    },

    get_type: function() {
        return Type.getInstanceType(this);
    },
    
    dispose: function() { },

    hashCode: function() {
        if (!this.__hashCode) {
            if (this._id) {
                this.__hashCode = this.__type + ":#" + this._id;
            } else {
                this.__hashCode = this.__type + ":#" + Math.random();
            }
        }
        
        return this.__hashCode;
    }
};

Auto.Properties(Entity.prototype, [
	{ name: 'id', autoEvent: true, defaultValue: 0 },
	{ name: 'version', autoEvent: true, defaultValue: '' },
	{ name: 'guid' }
]);

Entity.createClass('Entity');
EntitySet = {
    init: function() {
        if (this.get_keyProperty()) {
            Repository['add_' + this._entityType.toCamelCase() + 'Saved'](this.__repository_entitySaved, this);
            Repository['add_' + this._entityType.toCamelCase() + 'Loaded'](this.__repository_entityLoaded, this);
        }

        Repository['add_' + this._entityType.toCamelCase() + 'Deleted'](this.__repository_entityDeleted, this);
    },
    
    __repository_entitySaved: function(sender, args) {
        this._synchronizeEntities(args.entities);
    },
    
    __repository_entityLoaded: function(sender, args) {
        this._synchronizeEntities(args.entities);
    },
    
    __repository_entityDeleted: function(sender, args) {
        var found = [];

        for (var i = 0; i < this.length; i++) {
            for (var j = 0; j < args.entities.length; j++) {
                if (args.entities[j] === this[i]) {
                    found.add(this[i]);
                    break;
                }
            }
        }

        if(found.length > 0)
            this.remove(found);
    },
    
    set_keyValue: function(value) {
        if(!this._keyProperty)
            return;
    
        if(this._keyValue === value)
            return;
            
        this._keyValue = value;
        
        if(!value)
            this.clear();
        
        var filtered = Repository.FilterCached(this._entityType, this.__entityFilter.bind(this));
        
        this.synchronize(filtered,  {notTrackable: true});
    },
    
    __entityFilter: function(entity) {
        return entity['get_' + this._keyProperty]() === this._keyValue;
    },
    
    _synchronizeEntities: function(entities) {
        var toRemove = entities.where(function(entity) {
            return !this.__entityFilter(entity) && this.contains(entity);
        }, this);
        
        this.remove(toRemove,  {notTrackable: true});
        
        var toAdd = entities.where(function(entity) {
            return this.__entityFilter(entity) && !this.contains(entity);
        }, this);
                
        this.add(toAdd, {notTrackable: true});
    },

    dispose: function() {
        if(!this._keyProperty)
            return;
    
        Repository['remove_' + this._entityType.toCamelCase() + 'Saved'](this.__repository_entitySaved, this);
        Repository['remove_' + this._entityType.toCamelCase() + 'Loaded'](this.__repository_entityLoaded, this);
        Repository['remove_' + this._entityType.toCamelCase() + 'Deleted'](this.__repository_entityDeleted, this);
    }
};

Auto.Properties(EntitySet, [
    'entityType',
    'keyProperty',
    'keyValue'
]);

Array.prototype.makeEntitySet = function(owner, entityType, keyProperty, keyValue) {
    if(this.__isEntitySet)
        return;

    var type = Type.getType(entityType);
    if(!type)
        throw new Error('Valid entity type name is required for EntitySet');
        
    this.makeObservable();
    this.makeTracking(entityType);

    Trait.Apply(this, EntitySet);
    
    this.set_entityType(entityType);
    this.__isEntitySet = true;
    
    this.set_keyProperty(keyProperty);
    
    this.init();
    
    if(keyValue)
        this.set_keyValue(keyValue);
        
    return this;
};
var CustomFiltersList = function() {
    this._filters = {};
};

CustomFiltersList.prototype = {
    clear: function() {
        this._filters = {};
    },

    get: function(key) {
        return this._filters[key];
    },

    set: function(key, value) {
        var oldValue = this._filters[key];
        this._filters[key] = value;
        this.raise_filterChanged({ key: key, newValue: value, oldValue: oldValue });
    },

    remove: function(key) {
        delete this._filters[key];
        this.raise_filterChanged({ key: key, removed: true });
    },

    count: function() {
        return Object.keysCount(this._filters);
    },

    init: function(parts) {
        for (var i = 0; i < parts.length; i++) {
            var regex = /^(\S+)\(([^)]*?)\)$/i;
            var matches = regex.exec(parts[i]);

            if (!matches || matches.length != 3) {
                this.set(parts[i], undefined);
            } else {
                var args = matches[2].split(',').select(function(obj) { return obj.trim(); });
                this.set(matches[1], args);
            }
        }
    },

    toString: function() {
        var filter = "",
            customFilters = this._filters;

        for (var prop in customFilters) {
            if (customFilters.hasOwnProperty(prop)) {
                var value = customFilters[prop];

                if (isUndefined(value)) {
                    filter += prop + ";";
                } else {
                    filter += prop + "(" + customFilters[prop] + ");"
                }
            }
        }

        return filter;
    }
};

Auto.Events(CustomFiltersList.prototype, [
    'filterChanged'
]);

var DataFilter = function(stringFilter) {
    this._query = "";
    this._customFilters = new CustomFiltersList();
    
    if (stringFilter) {
        this.initFromString(stringFilter);
    }

    this._customFilters.add_filterChanged(function() {
        this._updateFilter();
    }, this);
};

DataFilter.prototype = {
    get_customFilters: function() {
        return this._customFilters;
    },

    get_query: function() {
        return this._query;
    },

    set_query: function(newQuery) {
        var newQuery = newQuery || '';

        if (newQuery === this._query) {
            return;
        }

        this._query = newQuery;
        this._updateFilter();
    },

    _clear: function() {
        this._query = "";
        this._customFilters.clear();
        this._updateFilter();
    },

    initFromString: function(filter) {
        this.__initializing = true;
        this._clear();
        var parts = filter.split(';');
        this._query = parts.pop().trim();
        this._customFilters.init(parts);
        this.__initializing = false;
        this._updateFilter();
    },

    _updateFilter: function() {
        if (this.__initializing) {
            return;
        }

        var oldFilter = this._oldFilter,
            newFilter = this.toString();

        if (newFilter === oldFilter) {
            return;
        }

        this._oldFilter = newFilter;
        this.raise_filterChanged({ newValue: newFilter, oldValue: oldFilter });
    },

    toString: function() {
        return this._customFilters.toString() + this._query;
    }
};

Auto.Events(DataFilter.prototype, [
    'filterChanged'
]);

DataFilter.createClass('DataFilter');
DataSource = {
    init: function (entityType, childTypes) {
        if (this.__initialized)
            return;

        if (!entityType)
            throw new Error('Entity type for DataSource not selected');

        this._entityType = entityType;
        this._childTypes = childTypes;

        this.__initialized = true;
        this.__loaded = false;
    },

    get_filter: function() {
        return this._filter;
    },

    get_filterString: function() {
        var filter = this.get_filter();
        return (filter ? filter.toString() : null) || '*';
    },

    set_filterString: function(newFilter) {
        var filter = this.get_filter();

        if (!filter) {
            throw new Error('Filter is not instantiated');
        }

        filter.initFromString(newFilter);
    },

    _onPreLoad: function() {
    },

    overallLoad: function(onSuccessFilter, isSubsidiary) {
        this._onPreLoad();
        var filterValue = isSubsidiary ? this._tempFilter : this.get_filterString();

        if (this.__rid) {
            Repository.Abort(this.__rid);
            this.__rid = null;
        }

        this.__rid = Repository.Filter(            
            this._entityType.get_name(),
            {
                filter: filterValue,
                searchQuery: this._searchQuery || '',
                orderBy: this._orderBy || null,
                sortDirection: this._sortDirection || null,
                page: this._page || null,
                pageSize: this._pageSize || null
            },
            function (result, context) {
                this.__rid = null;
                onSuccessFilter(result, context);
            }.bind(this),
            {
                requestCaller: this,
                subsidiary: isSubsidiary
            }
        );
    },

    load: function (onSuccess) {
        if (this.__loadTimeout) {
            clearTimeout(this.__loadTimeout);
        }

        this.__loadTimeout = setTimeout(function() {
            this.overallLoad (
                function(result, context) {
                    if (!this.__loaded) {
                        this.__attachHandlers();
                        this.__loaded = true;
                    }

                    this._onLoadSuccess(result, context, onSuccess);                
                }.bind(this),
                false // subsidiary
            );
        }.bind(this), 0);
        
        return this;        
    },

    isLoaded: function () {
        return this.__loaded;
    },

    updateCount: function (onSuccess) {
        Repository.Count(
            this._entityType.get_name(),
            {
                filter: this.get_filterString(),
                searchQuery: this._searchQuery || ''
            },
            function (result, context) {
                if (onSuccess)
                    onSuccess(result);

                this.set_count(result);
            } .bind(this)
        );
    },

    add: function (data) {        
        var items = data instanceof Array ? data : [data];

        for (var i = 0; i < items.length; i++)
            this.splice(this._findPosition(items[i]), 0, items[i]);
    },

    updateFrom: function (entities) {
        this._sift(entities, function (sifted) {
            this._applySifted(entities, sifted);
        } .bind(this));
    },

    // overridable by dataSource traits

    _findPosition: function (item) {
        return this.length;
    },

    _sift: function (items, callback) {
        callback(items);
    },

    _onLoadSuccess: function (result, context, onSuccess) {        
        this.synchronize(result, null, { notTrackable: true });

        if (onSuccess)
            onSuccess(result, context);
    },

    _applySifted: function (entities, sifted) {
        var toRemove = entities.where(function (entity) {
            return this.contains(entity) && !sifted.contains(entity);
        }, this);

        if (toRemove.length > 0)
            this.remove(toRemove, { notTrackable: true });

        var toAdd = sifted.where(function (item) { return !this.contains(item); }, this);

        if (toAdd.length > 0)
            this.add(toAdd, { notTrackable: true });
    },

    // ********************************

    get_entityType: function () {
        return this._entityType;
    },

    ensureTypeIs: function (entityType) {
        if (this._entityType !== entityType) {
            throw new Error('[DataSource] Type check assertion. Expected type is "' + entityType + '". Actual type is "' + this._entityType + '".');
        }
    },

    __attachHandlers: function () {
        Repository['add_' + this._entityType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
        Repository['add_' + this._entityType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
        Repository['add_' + this._entityType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);

        if (this._childTypes && this._childTypes instanceof Array) {
            for (var i = 0; i < this._childTypes.length; i++) {
                var childType = this._childTypes[i];
                Repository['add_' + childType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
                Repository['add_' + childType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
                Repository['add_' + childType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);
            }
        }
    },

    __detachHandlers: function () {
        Repository['remove_' + this._entityType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
        Repository['remove_' + this._entityType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
        Repository['remove_' + this._entityType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);

        if (this._childTypes) {
            for (var i = 0; i < this._childTypes.length; i++) {
                var entityType = this._childTypes[i];
                Repository['remove_' + entityType.get_name().toCamelCase() + 'Saved'](this.__entitySavedHandler, this);
                Repository['remove_' + entityType.get_name().toCamelCase() + 'Loaded'](this.__entityLoadedHandler, this);
                Repository['remove_' + entityType.get_name().toCamelCase() + 'Deleted'](this.__entityDeletedHandler, this);
            }
        }
    },

    __entitySavedHandler: function (sender, args, context) {
        this._onEntitySave(sender, args, context);
    },
    __entityLoadedHandler: function (sender, args, context) {
        if (!context || (context.requestCaller !== this && !context.subsidiary))
            this._onEntityLoad(sender, args, context);
    },
    __entityDeletedHandler: function (sender, args, context) {
        this._onEntityDelete(sender, args, context);
    },

    _onEntitySave: function (sender, args) {
        this.updateFrom(args.entities);
    },

    _onEntityLoad: function (sender, args) {        
        this.updateFrom(args.entities);
    },

    _onEntityDelete: function (sender, args) {
        for (var i = 0; i < args.entities.length; i++) {
            if (this.contains(args.entities[i]))
                this.remove(args.entities[i]);
        }
    },

    dispose: function () {
        if (this.__rid) {
            Repository.Abort(this.__rid);
            this.__rid = null;
        }

        if(this.__loaded) {
            this.__detachHandlers();
        }

        if (this.__events) {
            delete this.__events;
        }
    }
};

Array.prototype.makeDataSource = function(entityTypeName, callback, childTypes) {
    if(this.__isDataSource)
        return;
        
    var type = Type.getType(entityTypeName);
    
    if (!type) {
        throw new Error('valid entity type name is required for dataSource');
    }

    for (var i = 0; i < this.length; i++) {
        if (this[i].get_type() !== type) {
            throw new Error('Datasource type mistmatch in source array.')
        }
    }
        
    this.makeObservable();
    
    Trait.Apply(this, DataSource, {
        asOriginal: ['add']
    });
    
    this.init(type, childTypes);
    this.__isDataSource = true;
    
    if(callback)
        this.load(callback);
    
    return this;
};

Auto.Properties(DataSource, [
    { name: 'count', autoEvent: true }
]);
FilteredDataSource = {
    set_filter: function (filter) {
        if (isString(filter)) {
            filter = new DataFilter(filter);
        }

        if (!filter instanceof DataFilter) {
            throw new Error('Filter must be of DataFilter type');
        }

        var oldValue = this._filter;
        this._filter = filter;
        this.raise_filterChanged({ newValue: filter, oldValue: oldValue });
    },

    set_searchQuery: function (searchQuery, update) {
        if (typeof searchQuery !== 'string') {
            throw new Error('Search query should be a string');
        }

        var oldSearchQuery = this._searchQuery;
        this._searchQuery = searchQuery;
        this.raise_searchQueryChanged({ newValue: searchQuery, oldValue: oldSearchQuery });

        if (update) {
            this.load();
        }
    },

    _addIdsToFilter: function (filter, items) {
        filter = filter + ' AND Id In (';

        for (var i = 0; i < items.length; i++) {
            filter += (i > 0 ? ',' : '') + items[i]._id;
        }

        filter += ')';

        return filter;
    },

    _isServerFilter: function (filter) {
        filter = filter.trim();
        return filter.endsWith(';');
    },

    _isAnyFilter: function (filter) {
        return filter === '*' || filter.trim() === '';
    },

    _sift: function (items, callback) {        
        var filter = this.get_filterString();

        if (!this._isAnyFilter(filter)) {
            if (!this._isServerFilter(filter)) {
                filter = this._addIdsToFilter(filter, items);
            }
        }
        else { // 'Any' filter
            if ( !this._searchQuery || this._searchQuery === "") {
                callback(items);
                return;
            }
        }

        this._tempFilter = filter;

        this.overallLoad (
            function(result, context) {
                this._tempFilter = null;
                callback(result);               
            }.bind(this),
            true // subsidiary
        );   
    }
};

Auto.Property(FilteredDataSource, { name: 'filter', autoEvent: true });
Auto.Property(FilteredDataSource, { name: 'searchQuery', autoEvent: true });

DataSource.filterBy = function(filter, onSuccess) {
    if(this.__isFiltered)
        return;

    Trait.Apply(this, FilteredDataSource, {
            asOriginal: [ '_sift' ]
        }
    );
    
    if (filter) {
        this.set_filter(filter);
    } else {
        this.set_filter(new DataFilter());
    }

    this._tempFilter = null;
    
    if(onSuccess)
        this.load(onSuccess);
    
    this.__isFiltered = true;
    
    return this;
};
PaginatedDataSource = {
    set_page: function(value, onSuccess) {
        if(!this.__paginateType)
            throw new Error('Pagination type not selected');
        
        if (this._page === value) {
            return;
        }

        var oldValue = this._page;
        this._page = value;
        this.__dropPageOnLoad = false;

        if (this.__loaded) {
            this.load(onSuccess);
        }

        this.raise_pageChanged({ oldValue: oldValue, newValue: value });
    },
    
    set_pageSize: function(value, onSuccess) {
        if(!this.__paginateType)
            throw new Error('Pagination type not selected');
    
        this._pageSize = value;
        
        if(this.__paginateType === 'simple')            
            this.load(onSuccess);
    },

    _onPreLoad: function() {
        if (this.__dropPageOnLoad) {
            this._page = 1;
        }
    },
    
    _onLoadSuccess: function(result, context, onSuccess) {
         if(!this.__paginateType)
            throw new Error('Pagination type not selected');

        if(this.__paginateType === 'simple') {
            this.updateCount();
            arguments.callee.original.apply(this, [result, context, onSuccess]);
            return;
        }

        this.add(result, { notTrackable : true });

        if(onSuccess)
            onSuccess(result, context);
    },
    
    _applySifted: function(entities, sifted) {
//TODO: Research possibilities of normal imlementing

//        debugger;

//        if(this.__paginateType === 'simple') {
//            arguments.callee.original.apply(this, [entities, sifted]);
//            return;
//        }
//    
        var toRemove = entities.where(function(entity) {
            return this.contains(entity) && !sifted.contains(entity);                
        }, this);
        
        if(toRemove.length > 0) {
            this.load();                
            return;
        }
    },

    nextPage: function(onSuccess) {
        this.set_page(this._page + 1, onSuccess);
    },
    
    prevPage: function(onSuccess) {
        this.set_page(this._page - 1, onSuccess);
    }
};

Auto.Properties(PaginatedDataSource, [
    { name: 'page', autoEvent: true },
    'pageSize'        
]);

DataSource.paginate = function(pageSize, onSuccess) {
    if(this.__isPaginated)
        return;

    Trait.Apply(this, PaginatedDataSource, {
        asOriginal: ['_onLoadSuccess', '_applySifted']
    });
    
    this._page = 1;
    this._pageSize = pageSize || 10;
    
    this.simple = function() {
        this.__paginateType = 'simple';
    
        delete this.progressive;
        delete this.simple;
        
        if(onSuccess)
            this.load(onSuccess);
            
        return this;
    }.bind(this);
    this.progressive = function() {
        this.__paginateType = 'progressive';
     
        delete this.progressive;
        delete this.simple;
        
        if(onSuccess)
            this.load(onSuccess);
            
        return this;
    }.bind(this);

    this.__isPaginated = true;

    if (this._filter instanceof DataFilter) {
        this._filter.add_filterChanged(function(sender, args) {
            this.__dropPageOnLoad = true;
        }, this);
    }
    
    if (this.add_searchQueryChanged) {
        this.add_searchQueryChanged(function(sender, args) {
            this.__dropPageOnLoad = true;
        }, this);
    }
    
    return this;
};
OrderedDataSource = {
    set_orderBy: function(value) {        
        if (!value || !(typeof(value) === 'string') || value === '') {
            throw new Error('incorrect order property name');
        }

        this._orderByAndSortDirection = new Array();
        var oldOrderBy = this._orderBy;

        if (!isArray(value)) {
            value = value.split(',');
            
            for (var i = 0; i < value.length; i++) {
                value[i] = value[i].trim();
                valueWithOrder = value[i].split(' ');
                this._orderByAndSortDirection[i] = {
                    value: '_' + valueWithOrder[0].toCamelCase(),
                    order: valueWithOrder[1] || 'asc'
                };
            }
        }

        if (oldOrderBy === value) {
            return;
        }

        this._orderBy = value;
        this.raise_orderByChanged({ newValue: value, oldValue: oldOrderBy });
    },

    ascending: function() {
        this.set_sortDirection('asc');
        return this;
    },
    
    descending: function() {
        this.set_sortDirection('desc');
        return this;
    },

    _onLoadSuccess: function (result, context, onSuccess) {        
        this.sort();
    },

    set_sortDirection: function(direction) {
    },

    _onEntitySave: function(sender, args) {
        var toProcess = args.entities.where(function(item) { return this.contains(item);}, this );
        
        if(toProcess.length > 0)
            this.sort();
    },
    
    _onEntityLoad: function(sender, args) {
        var toProcess = args.entities.where(function(item) { return this.contains(item);}, this );
        
        if (toProcess.length > 0)
            this.sort();
    },
    
    sort: function() {
        if(this.length <= 1)
            return;
    
        var initial = [];
        
        for (var i = 0; i< this.length; i ++) {
            initial[i] = this[i];
        }

        var sorted = this.__mergeSort(this);
        var sortMap = this.__getSortMapSimple(sorted);

        for(var i = 0; i < sortMap.length; i++) {
            this.splice(sortMap[i].end, 1, sortMap[i].item);
        }

        var movedItems = sortMap.select(function(sm) {return sm.item});

        //TODO: get rid of removed event
        if (movedItems.length > 0) {
            this.raise_removed({items: movedItems});
            this.raise_added(this.__createAddedArguments(movedItems));
        }
    },
    
    __getSortMapSimple: function(sortResult) {
        var map = [];
        
        for(var i=0; i < sortResult.length; i++) {
            if(sortResult[i] === this[i])
                continue;
                
            map.add({
                item: sortResult[i],
                begin: this.indexOf(sortResult[i]),
                end: i
            });            
        }
        
        return map;
    },
    
    __getSortMap: function(result) {
        var map = [];
    
        var resultPtr = 0;
        var thisPtr = 0;
        
        var seen = [];
        
        while(resultPtr < result.length && thisPtr < this.length) {
            var resItem = result[resultPtr];
            var thisItem = this[thisPtr];
            
            if(resItem === thisItem) {
                resultPtr++;
                thisPtr++;
            }
            else if(seen.contains(thisItem)) {
                thisPtr++;
            }
            else {
                map.add({
                    item: resItem,
                    begin: this.indexOf(resItem),
                    end: resultPtr
                });
                
                seen.add(resItem);
                
                resultPtr++;
            }
        }
        
        return map;
    },
   
    __mergeSort: function(arr) {
        if (arr.length == 1)
            return arr;
        
        var arrSorted = [];
        var middle = Math.floor(arr.length/2);
        
        var leftArray = arr.slice(0, middle);
        var rightArray = arr.slice(middle, arr.length);
        
        leftArray =  this.__mergeSort(leftArray);
        rightArray = this.__mergeSort(rightArray);            
        
        var left = 0;
        var right = 0;

        for (var i = 0; i < leftArray.length + rightArray.length; i++) {
            if (left == leftArray.length){
                arrSorted.add(rightArray[right]);
                right++;
            } else if (right == rightArray.length){
                arrSorted.add(leftArray[left]);
                left++;
            } else if(this.__compare(leftArray[left], rightArray[right]) <= 0) {
                arrSorted.add(leftArray[left]);
                left++;
            } else {
                arrSorted.add(rightArray[right]);
                right++;
            }
        }
        return arrSorted;
    },
    
    _findPosition: function(item) {   
        if (this.length == 0)
            return 0;
        
        var start = 0;
        var end = this.length - 1;
        
        while( start <= end) {        
            var mid = start + Math.floor((end - start) / 2);
            
            var previous = this[mid - 1];
            var current = this[mid];
            var next = this[mid + 1];
            
            var leftCompare = previous ? this.__compare(item, previous) : 1;
            var midCompare = this.__compare(item, current);
            var rightCompare = next ? this.__compare(item, next) : -1;
            
            if(midCompare > 0) {
                if(rightCompare > 0)
                    start = mid + 1;
                else
                    return mid + 1;
            }
            else if(midCompare < 0) {
                if(leftCompare < 0)
                    end = mid - 1;
                else
                    return mid;
            }
            else
                return mid + 1;
        }
    },
    
    __switch: function(left, right) {
        var lObj = this[left];
        var rObj = this[right];
                    
        this[left] = rObj;
        this[right] = lObj;
    },

    __compare: function(left, right) {
        var res = 0;
        
        for (var i = 0; i < this._orderByAndSortDirection.length; i++) { 
            var getter = "get" + this._orderByAndSortDirection[i].value;

            if (this._orderByAndSortDirection[i].order == "desc") {
                res = this.__descCompare(getter, left, right);
            } else {
                res = this.__ascCompare(getter, left, right);
            }
                
            if (res != 0) {
                return res;
            }
        }

        return res;
    },
        
    __ascCompare: function(getter, left, right) {
        var l = left[getter](),
            r = right[getter]();

        var isDate = (l instanceof Date) || (r instanceof Date);
        var isNumber = typeof(l) == "number" || typeof(r) == "number";
        
        if (l != null && r != null && (!isNumber && !isDate)) {
            l = l.toString().toUpperCase();
            r = r.toString().toUpperCase();
        }
        
        if (l > r)
            return 1;

        if (l < r)
            return -1;
        
        return 0;
    },

    __descCompare: function(getter, left, right) {
        return -1 * this.__ascCompare(getter, left, right);
    }
};

Auto.Properties(OrderedDataSource, [
    { name: 'orderBy', autoEvent: true },
    { name: 'sortDirection', autoEvent: true }
]);

DataSource.orderBy = function(orderBy) {
    if (this.__isOrdered)
        return;

    this.__isOrdered = true;

    Trait.Apply(this, OrderedDataSource, {
        asOriginal: ['sort', '_findPosition']
    });

    this.set_orderBy(orderBy);
    this.sort();
        
    return this;
};
Type.createNamespace('History');

History.Observer = {
    _types: [],

    registerObserver: function(observer) {
        History.Observer._types.add(observer);
    },

    _getObserverForValue: function(value, itemType) {
        var types = History.Observer._types;
        
        for (var i = 0; i < types.length; i++) {
            if (types[i].canObserve(value, itemType)) {
                return types[i];
            }
        }

        return History.DefaultObserver;
    },

    init: function(itemName, itemType, relatedObject, page) {
        var value = relatedObject['get_' + itemName]();
        var page = isDefined(page) ? page : Application.get_currentPage();

        return new (this._getObserverForValue(value, itemType))(itemName, relatedObject, page);
    }
};

Type.createNamespace('History');

History.BaseObserver = function(itemName, relatedObject, page) {
    if (!page) {
        throw new Error('Page is null');
    }

    this._page = page;
    this._itemName = itemName;
    this._data = relatedObject;
    
    page.add_onParamChanged(function(sender, args) {       
        if (args.key == this._itemName) {
            this.paramChanged(this, { value: args.newValue });
        }
    }.bind(this));

    this.init();
};

History.BaseObserver.canObserve = function(obj, type) {
    return true;
};

History.BaseObserver.prototype = {
    init: function() {
        var window = this._page,
            itemName = this._itemName,
            data = this._data;

        data['add_' + itemName + 'Changed'](function(sender, args) {                                                
            window.set_param(itemName, args.newValue);
        }, this);

        var paramValue = window.get_param(itemName);

        if (paramValue) {
            data['set_' + itemName](paramValue);
        }
    },

    paramChanged: function(sender, args) {
        this._data['set_' + this._itemName](args.value);
    }
};

History.BaseObserver.createClass('History.BaseObserver');
Type.createNamespace('History');

History.DefaultObserver = function(itemName, relatedObject, page) {
    History.DefaultObserver.constructBase(this, [itemName, relatedObject, page]);
};

History.DefaultObserver.canObserve = function(obj, type) {
    return true;
};

History.DefaultObserver.prototype = {
};

History.DefaultObserver.createClass('History.DefaultObserver', History.BaseObserver);
Type.createNamespace('History');

History.DataSource = function(itemName, relatedObject, page) {
    History.DataSource.constructBase(this, [itemName, relatedObject, page]);
};

History.DataSource.canObserve = function(obj, type) {
    return (isDefined(obj) && obj.__isDataSource) || type === 'DataSource';
};

History.DataSource.prototype = {
    _ds: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__dsChanged, this);

        if (isDefined(curValue)) {
            this._ds = curValue;
            this._attachHandlersForDs(curValue);
        }
        
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    _attachHandlersForDs: function(ds) {
        if (isDefined(ds)) {
            var isPaginated = !!ds.__isPaginated;
            var isFiltered = !!ds.__isFiltered;
            var isOrdered = !!ds.__isOrdered;

            if (isFiltered) {
                ds.get_filter().add_filterChanged(this._updatePageParam, this);
                ds.add_searchQueryChanged(this._updatePageParam, this);
            }
        
            if (isPaginated) {
                ds.add_pageChanged(this._updatePageParam, this);
            }

            if (isOrdered) {
                ds.add_orderByChanged(this._updatePageParam, this);
                ds.add_sortDirectionChanged(this._updatePageParam, this);
            }
        }
    },

    _detachHandlersFromDs: function(ds) {
        if (isDefined(ds)) {
            var isPaginated = !!ds.__isPaginated;
            var isFiltered = !!ds.__isFiltered;
            var isOrdered = !!ds.__isOrdered;

            if (isFiltered) {
                ds.remove_filterChanged(this._updatePageParam, this);
                ds.remove_searchQueryChanged(this._updatePageParam, this);
            }
        
            if (isPaginated) {
                ds.remove_pageChanged(this._updatePageParam, this);
            }

            if (isOrdered) {
                ds.remove_orderByChanged(this._updatePageParam, this);
                ds.remove_sortDirectionChanged(this._updatePageParam, this);
            }
        }
    },

    __dsChanged: function(sender, args) {
        var ds = args.newValue;
        this._ds = ds;
        this._detachHandlersFromDs(args.oldValue);
        this._attachHandlersForDs(ds);

        this._updatePageParam();
    },

    paramChanged: function(sender, args) {
        this._loadFromHistory(args.value);
    },

    _parseParams: function(paramValue) {
        var paramValues = Base64.decode(paramValue).split('&');
        paramValues.shift();

        var params = {};

        for (var i = 0; i < paramValues.length; i++) {
            var item = paramValues[i];
            var keyValue = item.split('=');
            var key = keyValue[0];
            var value = keyValue[1];

            params[key] = value;
        }

        return params;
    },

    _loadFromHistory: function(paramValue) {
        var oldValue = this._getParamValue();

        if (oldValue === paramValue) {
            return;
        }

        var params = this._parseParams(paramValue);

        var decode = function(value) {
            return value.replaceAll('&eq;', '=').replaceAll('&amp;', '&');
        };

        if (!params['et']) {
            return;
        }

        var ds = [].makeDataSource(params['et']);

        if (params['if'] == 1) {
            var filter = params['f'] || '';
            ds = ds.filterBy(decode(filter));
            
            if (filter) {
            }

            var searchQuery = params['sq'];

            if (searchQuery) {
                ds.set_searchQuery(decode(searchQuery));
            }
        }

        if (params['ip'] == 1) {
            ds = ds.paginate(15).simple();
            var page = params['p'];

            if (page) {
                ds.set_page(page*1);
            }
        }

        if (params['io'] == 1 && params['ob']) {
            ds = ds.orderBy(params['ob']);
            ds.set_sortDirection(params['sd']);
        }

        this._data['set_' + this._itemName](ds);
    },

    _getParamValue: function() {
        var ds = this._ds;
        var paramValue = '';

        if (isNullOrUndefined(ds)) {
            return null;
        }

        var encode = function(value) {
            return value.replaceAll('&', '&amp;').replaceAll('=', '&eq;'); // TODO: global search
        }

        var entityType = ds._entityType.get_name();
        var isPaginated = !!ds.__isPaginated;
        var isFiltered = !!ds.__isFiltered;
        var isOrdered = !!ds.__isOrdered;

        paramValue += '&et=' + encode(entityType);

        if (isFiltered) {
            paramValue += '&if=1';
            var filter = ds.get_filterString();

            if (filter) {
                paramValue += '&f=' + encode(filter);
            }

            var searchQuery = ds.get_searchQuery();

            if (searchQuery) {
                paramValue += '&sq=' + encode(searchQuery);
            }
        }

        if (isPaginated) {
            paramValue += '&ip=1';
            var page = ds.get_page()*1;
            paramValue += '&p=' + page;
        }
        
        if (isOrdered) {
            paramValue += '&io=1';
            var orderBy = ds.get_orderBy().join(',');
            var sortDirection = ds.get_sortDirection();
            paramValue += '&ob=' + orderBy;
            paramValue += '&sd=' + sortDirection;
        }

        return Base64.encode(paramValue);
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.DataSource);
History.DataSource.createClass('History.DataSource', History.BaseObserver);
Type.createNamespace('History');

History.ObservableList = function(itemName, relatedObject, page) {
    History.ObservableList.constructBase(this, [itemName, relatedObject, page]);
};

History.ObservableList.canObserve = function(obj, type) {
    return (isDefined(obj) && obj.__observable) || type === 'Observable';
};

History.ObservableList.prototype = {
    _item: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__itemChanged, this);

        if (isDefined(curValue)) {
            this._item = curValue;
            this._attachHandlersForItem(curValue);
        }
        
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    _attachHandlersForItem: function(item) {
        if (isDefined(item)) {
            item.add_changed(this._updatePageParam, this);
        }
    },

    _detachHandlersFromItem: function(item) {
        if (isDefined(item)) {
            item.remove_changed(this._updatePageParam, this);
        }
    },

    __itemChanged: function(sender, args) {
        var item = args.newValue;
        this._item = item;
        this._detachHandlersFromItem(args.oldValue);
        this._attachHandlersForItem(item);

        this._updatePageParam();
    },

    paramChanged: function(sender, args) {
        this._loadFromHistory(args.value);
    },

    _loadFromHistory: function(paramValue) {
        var oldValue = this._getParamValue();

        if (oldValue === paramValue) {
            return;
        }

        var decode = function(value) {
            return value.replaceAll('&z;', ',').replaceAll('&amp;', '&');
        };

        var params = paramValue.split(',');

        for (var i = 0; i < params.length; i++) {
            params[i] = decode(params[i]);
        }

        var item = [].makeObservable();
        this._data['set_' + this._itemName](item);
        item.add(params);
    },

    _getParamValue: function() {
        var item = this._item;
        var paramValue = '';

        if (isNullOrUndefined(item)) {
            return null;
        }

        var encode = function(value) {
            return value.replaceAll('&', '&amp;').replaceAll(',', '&z;');
        }

        var params = [];

        for (var i = 0; i < item.length; i++) {
            params.add(encode(item[i].toString()));
        }

        return params.join(',');
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.ObservableList);
History.ObservableList.createClass('History.ObservableList', History.BaseObserver);
Type.createNamespace('History');

History.Boolean = function(itemName, relatedObject, page) {
    History.Boolean.constructBase(this, [itemName, relatedObject, page]);
};

History.Boolean.canObserve = function(obj, type) {
    return (isDefined(obj) && (typeof(obj) == 'boolean')) || type === 'Boolean';
};

History.Boolean.prototype = {
    _value: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__dsChanged, this);
        this._value = curValue;
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    __dsChanged: function(sender, args) {
        this._value = args.newValue;
        this._updatePageParam();
    },

    paramChanged: function(sender, args) {
        this._loadFromHistory(args.value);
    },

    _loadFromHistory: function(paramValue) {
        var oldValue = this._getParamValue();

        if (oldValue === paramValue) {
            return;
        }

        var value = isDefined(paramValue) ? Boolean.parse(paramValue) : false;
        this._data['set_' + this._itemName](value);
    },

    _getParamValue: function() {
        var value = this._value;

        // value on this step can be not only boolean
        // because of this we use that strange convertation to string
        return isDefined(value) ? (value ? 'true' : 'false') : '';
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.Boolean);
History.Boolean.createClass('History.Boolean', History.BaseObserver);
Type.createNamespace('History');

History.Date = function(itemName, relatedObject, page) {
    History.Date.constructBase(this, [itemName, relatedObject, page]);
};

History.Date.canObserve = function(obj, type) {
    return (isDefined(obj) && (obj instanceof Date)) || type === 'Date';
};

History.Date.prototype = {
    _value: null,

    init: function() {
        var itemName = this._itemName,
            relatedObject = this._data,
            curValue = this._data['get_' + this._itemName]();

        this._data['add_' + this._itemName + 'Changed'](this.__dsChanged, this);
        this._value = curValue;
        var paramValue = this._page.get_param(this._itemName);

        if (paramValue) {
            this._loadFromHistory(paramValue);
        }
    },

    __dsChanged: function(sender, args) {
        this._value = args.newValue;
        this._updatePageParam();
    },

    paramChanged: function(sender, args) {
        this._loadFromHistory(args.value);
    },

    _loadFromHistory: function(paramValue) {
        var oldValue = this._getParamValue();

        if (oldValue === paramValue) {
            return;
        }

        var value = new Date(paramValue);

        if (!value) {
            value = null;
        }

        this._data['set_' + this._itemName](value);
    },

    _getParamValue: function() {
        var value = this._value;
        return value ? value.format('d') : '';
    },

    _updatePageParam: function() {
        this._page.set_param(this._itemName, this._getParamValue());
    }
};

History.Observer.registerObserver(History.Date);
History.Date.createClass('History.Date', History.BaseObserver);
var Keys = {
    Esc: 27,
    PgUp: 33,
    PgDown: 34,
    End: 35,
    Home: 36,
    Left: 37,
    Up: 38,
    Right: 39,
    Down: 40
}
TextMeasurer = {
    init: function(onInit) {
        var browser = $.browser;
        if (/*true || */!Silverlight.isInstalled() || browser.mozilla || (browser.msie && browser.versionNumber >= 9)) {
            onInit();
            return;
        }
        
        var source = Application.resolveUrl('silverlight:Phoenix.TextMeasureService.xap');
        TextMeasurer.__onInit = onInit;

        TextMeasurer.domElement = DOM.create("div", document.body);
        TextMeasurer.domElement.style.cssText = 'position: absolute; left: -1000px; top: -1000px; z-index: 5000; width: 1px; height: 1px;';

        Silverlight.createObject(
            source,
            TextMeasurer.domElement,
            null,
            { version: "4.0.50401.0",
              width: '100%',
              height: '100%',
              isWindowless: 'true',
              background: '#00FFFFFF'
            },
            {
                onError: TextMeasurer.__onError
            }, this._initialParams
        );

        TextMeasurer.__slObj = TextMeasurer.domElement.firstChild;
    },

    __onError: function() {
        throw new Error("TextMeasurer unhandled error");
    },

    __onLoad: function(service) {
        TextMeasurer.__service = service;

        Application.getTextWidth = function(text, font, size, lineHeight, widthLimit) {
            if (size == 0 || !font) {
                return 0;
            }

            var result = TextMeasurer.__service.GetTextWidth(text, font, size, lineHeight || 0, widthLimit || 0);
            return result;
        };
                    
        Application.getTextHeight = function(text, font, size, lineHeight, widthLimit) {
            if (size == 0 || !font) {
                return 0;
            }

            var result = TextMeasurer.__service.GetTextHeight(text, font, size, lineHeight || 0, widthLimit || 0);
            return result;
        };

        TextMeasurer.__onInit();
    }
};
var CSSRules = function() {
    var styleElement = DOM.create("style");
    styleElement.type = "text/css";

    var add = function() {
        if (styleElement.styleSheet) {
            return function(selector, rule) {
                if (styleElement.styleSheet.cssText == '') {
                    styleElement.styleSheet.cssText = '';
                }
                styleElement.styleSheet.cssText += selector + " { " + rule + " }";
            }
        } else {
            return function(selector, rule) {
                styleElement.appendChild(document.createTextNode(selector + " { " + rule + " }"));
            }
        }
    } ();

    var init = function() {
        var headElement = document.getElementsByTagName("head")[0];
        headElement.insertBefore(styleElement, headElement.firstChild);
    };

    var dispose = function() {
        var headElement = document.getElementsByTagName("head")[0];
        headElement.removeChild(styleElement);
    };

    return {
        add: add,
        init: init,
        dispose: dispose
    }
} ();
var DimensionUnit = function(s, s2) {
    this._autoSize = false;
    this._stretchLevel = 0;
    this._isStretched = false;

    // percent, pixels
    if (!isNullOrUndefined(s) && !isNullOrUndefined(s2)) {
        this.percent = s * 1;
        this.pixels = s2 * 1;
    }
    else {
        // string
        if (isString(s)) {
            this.parse(s);
        }
        else if (!isNullOrUndefined(s)) {
            this.pixels = s * 1;
            this.percent = 0;
        }
    }
};

DimensionUnit.Zero = new DimensionUnit(0);
DimensionUnit.__parseRegex = /^([0-9]+?(?:\.[0-9]+?)?\%)?((?:\s?(?:\+|\-)\s?)?[0-9]+?(?:px)?)?$/;

Object.extend(DimensionUnit.prototype, {
    percent: null,
    pixels: null,
    _autoSize: null,
    _stretchLevel: null,
    _isStretched: null,
    
    add: function(du) {
        return new DimensionUnit(this.percent + (du.percent || 0), this.pixels + (du.pixels || 0));
    },
    
    subtract: function(du) {
        return new DimensionUnit(this.percent - (du.percent || 0), this.pixels - (du.pixels || 0));
    },
    
    divide: function(n) {
        return new DimensionUnit(this.percent / n, Math.round(this.pixels / n));
    },

    parse: function(s) {
        if (isObject(s) && (s instanceof DimensionUnit)) {
            return s;
        }

        this.percent = 0;
        this.pixels = 0;
            
        if (isString(s)) {
            if (s.match(/^([0-9]*?)\*$/)) { // stretch mode
                var args = /^([0-9]*?)\*$/.exec(s);
                this.percent = null;
                this.pixels = null;
                this._stretchLevel = args[1] * 1 || 1;
                this._isStretched = this._stretchLevel > 0;
                return;
            }
        
            if (s === '?') { // auto-size mode (depends on childs)
                this.percent = null;
                this.pixels = null;
                this._autoSize = true;
                return;
            }

            var args = DimensionUnit.__parseRegex.exec(s);
            
            if (isNullOrUndefined(args)) {
                throw new Error('Cannot convert \"' + s + '\" to DimensionUnit.');
            }
            
            for (var i = 1; i < args.length; i++) {
                if (isNullOrUndefined(args[i]))
                    continue;
            
                if (args[i].endsWith('%')) {
                    this.percent = args[i].substr(0, args[i].length - 1) / 100;
                }
                else {
                    var pixels = args[i].replace(/\s/g, '');
                    
                    if (pixels.endsWith('px')) {
                        pixels = pixels.substr(0, pixels.length - 2);
                    }
                        
                    this.pixels = pixels.toNumber();
                }
            }
            
            if (isNaN(this.pixels) || isNaN(this.percent)) {
                throw new Error('Cannot convert \"' + s + '\" to DimensionUnit.');
            }
        }
    },
    
    isNull: function() {
        return this.percent === null && this.pixels === null && !this.isStretched() && !this.isAutoSize();
    },

    getStretchLevel: function() {
        return this._stretchLevel;
    },
    
    isStretched: function() {
        return this._isStretched;
    },
    
    isAutoSize: function() {
        return this._autoSize;
    },

    isFixed: function() {
        return !this.percent && this.pixels > 0;
    },
    
    toString: function() {
        var result = '';
        
        if (this.percent) {
            result += Math.round(this.percent * 100) + '%';
        }
        
        if (this.pixels) {
            result += (this.pixels > 0 ? '+' : '') + this.pixels + 'px';
        }
           
        if (!result) {
            result = '0';
        }
            
        return result;
    }
});
var Rect = function(s) {
    this.left = 0;
    this.top = 0;
    this.right = 0;
    this.bottom = 0;

    if (isDefined(s) && isString(s)) {
        this.parse(s);
    }
};

Object.extend(Rect.prototype, {
    left: null,
    top: null,
    right: null,
    bottom: null,

    get_width: function() {
        return this.right + this.left;
    },
    
    get_height: function() {
        return this.top + this.bottom;
    },

    parse: function(value) {
        var reg = /(?:(\-?[0-9]{1,2})(?:px)?)(?:\s|$)/ig;

        var arr = [];

        while (args = reg.exec(value)) {
            arr.add(args[1]);
        }

        if (arr.length == 1) {
            arr[3] = arr[2] = arr[1] = arr[0];
        } else if (arr.length == 2) {
            arr[3] = arr[1];
            arr[2] = arr[0];
        } else if (arr.length == 3) {
            arr[3] = arr[1];
        }

        if (arr.length == 4) {
            this.left = arr[0]*1, this.top = arr[1]*1, this.right = arr[2]*1, this.bottom = arr[3]*1;
        }
    },
    
    toString: function() {
        return "{0}px {1}px {2}px {3}px".format(this.top, this.right, this.bottom, this.left);
    }
});
PageUri = {
    parseUri: function(pageUri) {
        var uriParts = pageUri.split('|');
        var uri = uriParts[0];

        var params = {};
        var paramsIdx = {};
        uriParts.removeAt(0);

        if (uriParts.length > 0) {
            paramsParts = uriParts;

            if (paramsParts) {
                for (var i = 0; i < paramsParts.length; i++) {
                    var paramPair = paramsParts[i].split('='),
                        key = PageUri.urlPartDecode(paramPair[0]),
                        value = isUndefined(paramPair[1]) ? null : PageUri.urlPartDecode(paramPair[1]);
                    
                    params[key] = value;
                    paramsIdx[key] = i;
                }
            }
        }

        return { uri: uri, params: params, paramsIdx: paramsIdx };
    },

    buildUri: function(uri, params, paramsOrderFunc) {
        var names = [];
        
        for (var paramName in params) {
            if (params.hasOwnProperty(paramName)) {
                names.add(paramName);
            }
        }

        if (paramsOrderFunc) {
            names.sort(paramsOrderFunc);
        }

        var newUri = uri;
        for (var i = 0; i < names.length; i++) {
            var paramName = names[i],
                paramValue;
            
            if (!isNullOrUndefined(params[paramName])) {
                paramValue = params[paramName].toString();
            } else {
                paramValue = '';
            }
                
            newUri += "|" + PageUri.urlPartEncode(paramName) + "=" + PageUri.urlPartEncode(paramValue);
        }

        return newUri;
    },

    urlPartEncode: function(value) {
        return value.replaceAll('&', '&amp;').replaceAll('=', '&eq;').replaceAll('|', '&sp;').replaceAll(' ', '&n;');
    },

    urlPartDecode: function(value) {
        return value.replaceAll('&n;', ' ').replaceAll('&sp;', '|').replaceAll('&eq;', '=').replaceAll('&amp;', '&');
    },

    getBasePart: function(pageUri) {
        return PageUri.parseUri(pageUri).uri;
    },

    getParamsPart: function(pageUri) {
        return PageUri.parseUri(pageUri).params;
    }
};
var ApplicationContext = function() {
    this._items = {};
};

ApplicationContext.prototype = {
    _items: null,
    
    register: function(key, callback) {
        this._items[key] = callback;
    },
    
    get: function(key) {
        var item = this._items[key];
        
        if (item && isFunction(item)) {
            this._items[key] = item();
            return this._items[key];
        }
        
        return item;
    },
    
    add: function(key, value) {
        this._items[key] = value;
    }
};

ApplicationContext.createClass('ApplicationContext');
// public static Application
Application =
{
    _configuration: null,
    _context: new ApplicationContext(),
    _masterPage: null,

    _currentPage: null,

    _onError: null,

    _resizeTimeout: null,

    __updatesCount: 0,

    _resources: {},

    get_resource: function(resourceName) {
        var resourceInfo = resourceName.split(':');

        if (resourceInfo.length != 2) {
            throw new Error('The "resource" args should have a "<source>:<resourceName>" format');
        }
        
        if (resourceInfo[0] == 'page') {
            return Application.get_currentPage().get_resource(resourceInfo[1]);
        }
    },

    set_resource: function(resourceName, value) {
        
    },

    // ======== Properties ========
    /**
    * Returns configuration of the application
    * @return Configuration
    */
    get_configuration: function () {
        return Application._configuration;
    },

    get_context: function () {
        return Application._context;
    },

    get_currentUser: function () {
        return Application._context.get('currentUser');
    },

    isDebugMode: function() {
        return Application.get_configuration().get_isDebug();
    },

    /**
    * Returns information about system and environment
    */
    get_systemInfo: function () {
        var info = "UserAgent: " + navigator.userAgent;
        info += "\r\nPage: " + Application.get_currentPage().get_uri();

        return info;
    },

    /**
    * Set configuration of the application
    */
    set_configuration: function (value) {
        if (!Application._configuration) {
            Application._configuration = new Configuration();
        }

        Application._configuration.init(value);
    },

    get_siteMap: function () {
        return Application._siteMap.clone();
    },

    getPagesForMenu: function (items) {
        var siteMap = [].makeObservable();
        var items = items || Application._siteMap;

        for (var i = 0; i < items.length; i++) {
            var menuItem = items[i];

            if (menuItem.showInMenu) {
                siteMap.add({
                    href: menuItem.href,
                    title: menuItem.title,
                    childs: Application.getPagesForMenu(menuItem.childs)
                });
            }
        }

        return siteMap;
    },

    getPageInfoByUrl: function (href) {
        var items = Application._siteMap.clone();

        while (items.length > 0) {
            var menuItem = items.pop();

            if (menuItem.childs) {
                items.add(menuItem.childs);
            }

            if (menuItem.showInMenu && menuItem.href == href) {
                return menuItem;
            }
        }

        return null;
    },

    /**
    * Returns master page
    */
    get_masterPage: function () {
        return Application._masterPage;
    },

    /**
    * Returns current page
    */
    get_currentPage: function () {
        return Application._currentPage;
    },

    get_error403Uri: function () {
        return Application.get_configuration().get_error403Uri();
    },

    get_error404Uri: function () {
        return Application.get_configuration().get_error404Uri();
    },

    load404: function() {
        Application.loadPage(Application.get_error404Uri());
    },

    // ============ Methods ============
    loadPage: function (pageUri) {
        $.history.load(pageUri);
    },

    pageIsAvailable: function (pageUri) {
        var tempUri = "page:" + pageUri;

        var stack = this._siteMap.clone();

        while (stack.length > 0) {
            var item = stack.pop();

            if (item.href === tempUri) {
                return true;
            }

            if (item.childs && item.childs.length > 0) {
                stack.add(item.childs);
            }
        }

        return false;
    },

    reloadPage: function () {
        Application._loadPage(Application._currentPage.get_fullUri(), true);
    },

    _unloadCurrentPage: function() {
        if (Application._currentPage) {
            Application._clearUpdatingEvent();
            var container = Application._masterPage['page_container'],
                containerStyle = container.domElement.style,
                curPage = Application._currentPage;

            containerStyle.position = "absolute";
            containerStyle.left = "-5000px";
            containerStyle.top = "-5000px";
            containerStyle.display = "none";

            Application._addUpdatingEvent();
            curPage.free();

            if (container.controls.contains(curPage))
                container.controls.remove(curPage);

            Application.raise_onPageUnload();
            Application._removeUpdatingEvent();
        }
    },

    _loadNewPage: function(pageUri) {
        Application._loadingPageUri = pageUri;
        var newPage = new Page();
        Application._currentPage = newPage;

        newPage.add_fullUriChanged(function(sender, args) {
            $.history.load(args.newValue);
        });
        newPage.add_initComplete(Application._onPageLoaded);

        Application._addUpdatingEvent();
        Application.raise_onPageLoading();
        newPage.initFromUri(pageUri);
    },

    /**
    * Function which will be invoked when page is loaded from server and initialized
    */
    _onPageLoaded: function() {
        Application._loadingPageUri = null;

        var container = Application._masterPage['page_container'];
        var containerStyle = container.domElement.style;
        container.controls.add(Application._currentPage);
        Application.raise_onPageLoad();
        Application._removeUpdatingEvent();

        window.setTimeout(function () {
            containerStyle.position = "";
            containerStyle.left = "";
            containerStyle.top = "";
            containerStyle.display = "";
        }, 0);
    },

    /**
    * Load page by uri
    */
    _loadPage: function (pageUri, forceReload) {
        var args = { pageUri: pageUri, cancel: false };
        Application.raise_onPagePreLoading(args);

        // support for cancelation
        if (args.cancel) {
            return;
        }        

        // pageUri can be changed in page pre loading
        pageUri = args.pageUri || Application.get_configuration().get_startPageUri();

        var curUri = Application._currentPage ? Application._currentPage.get_fullUri() : null;

        // if uri is the same
        if (!forceReload) {
            if (Application._loadingPageUri == pageUri || curUri == pageUri) {
                return;
            }

            // if a base parts of uri are equal
            if (Application._currentPage && Application._currentPage.isSameUri(pageUri)) {
                Application._currentPage.set_fullUri(pageUri);
                return;
            }
        }

        Application._unloadCurrentPage();
        Application._loadNewPage(pageUri);
    },

    _addUpdatingEvent: function () {
        Application.__updatesCount++;

        if (Application.__updatesCount == 1) {
            if (!Application.get_silentMode()) {
                Application.raise_onUpdating();
            }
        }
    },

    _removeUpdatingEvent: function () {
        if (Application.__updatesCount == 0) {
            return;
        }

        Application.__updatesCount--;

        if (Application.__updatesCount == 0) {
            if (!Application.get_silentMode()) {
                Application.raise_onUpdated();
            }
        }
    },

    _clearUpdatingEvent: function () {
        if (Application.__updatesCount == 0) {
            return;
        }

        Application.__updatesCount = 0;
        Application.raise_onUpdated();
    },

    _initPages: function () {
        Application._masterPage = new Page();
        //Application._currentPage = new Page();

        Application._masterPage.add_initComplete(function () {
            var domElement = Application.get_configuration().get_domElement();
            Application._masterPage.instantiateInDom(domElement);
            Application._masterPage.update();
            Application._initHistoryHanders();

            Application.raise_onInitialized();
            Application._removeUpdatingEvent();
        });

        Application._addUpdatingEvent();
        Application._masterPage.initFromUri(Application.get_configuration().get_masterPage());
    },

    /**
    * Initializes the application
    */
    init: function (onSuccess) {
        // get available pages
        Services.PagesService.getPagesList(function (pages) {
            Application._siteMap = pages;
            Application.raise_onSiteMapInitialized();

            Application._initPages();
            Application._initHandlers();
            Application._initCss();

        }.bind(this), function () {
            Application.throwError("Access is denied");
        }.bind(this));

        if (Application.get_configuration().get_globalErrorHandling()) {
            window.onerror = function(errorMsg, url, lineNumber) {
                Application.throwError("Error at line " + lineNumber + ": " + errorMsg);
            };
        }
    },

    _initCss: function () {
        CSSRules.init();
        CSSRules.add("div, a, span, li, ul, ol", "overflow: hidden; border: 0 solid #000; word-wrap: break-word;");
        //CSSRules.add("div, a, span, li, ul, ol", "overflow: hidden; border-color: #000; border-style: solid;");
    },

    _disposeCss: function() {
        CSSRules.dispose();
    },

    _initHandlers: function () {
        $(window).bind("resize", function () {
            if (Application._resizeTimeout) {
                clearTimeout(Application._resizeTimeout);
                Application._resizeTimeout = null;
            }

            Application._resizeTimeout = setTimeout(function () {
                Application._clientBoundingRect = null;
                Application._masterPage.update();
            }, 50);
        });

        $(document).bind('keydown', function (event) {
            Application.raise_onKeyDown({ keyCode: event.keyCode });
        });
    },

    _initHistoryHanders: function () {
        $(document).ready(function () {
            function pageload(hash) {
                Application._loadPage(hash);
            }

            $.history.init(pageload, {
                unescape: true
            });
        });
    },

    /**
    * Throw an error
    */
    throwError: function (error) {
        Application.raise_onError(error);
        Application._clearUpdatingEvent();
    },

    _urlFilters: {},

    registerUrlFilter: function (protocol, filter) {
        Application._urlFilters[protocol] = filter;
    },

    /**
    * Converts url from it virtual representation to absolute
    */
    resolveUrl: function (virtualUrl) {
        virtualUrl = virtualUrl || '';

        var rurl = /^(\w+:)?(?:\/\/)?(.*)/;

        var parts = rurl.exec(virtualUrl);

        if (parts && parts[1]) {
            var protocol = parts[1];

            if (Application._urlFilters[protocol]) {
                virtualUrl = Application._urlFilters[protocol](virtualUrl, parts[2]);
            }
        }

        return virtualUrl.replace('~/', Application.get_configuration().get_rootUrl());
    },

    showConfirm: function (title, text, callback) {
        var confirmPopup = Application._masterPage['confirmPopup'];

        if (!isFunction(callback)) {
            throw new Error('You cannot show confirm without callback function!');
        }

        if (confirmPopup) {
            var onCommand = function (sender, args) {
                confirmPopup.remove_onCommand(onCommand);
                callback(args.button == 'Yes');
                confirmPopup.close();
            };

            confirmPopup.add_onCommand(onCommand);

            confirmPopup.set_title(title);
            confirmPopup.container['popupText'].set_text(text);
            confirmPopup.open();
        }
    },

    showError: function (title, text, callback) {
        var errorPopup = null;

        if (Application._masterPage && Application._masterPage.controls) {
            errorPopup = Application._masterPage['errorPopup'];
        }

        if (errorPopup) {
            var onCommand = function (sender, args) {
                errorPopup.remove_onCommand(onCommand);

                if (callback) {
                    callback();
                }

                errorPopup.close();
            };

            errorPopup.add_onCommand(onCommand);

            errorPopup.set_title(title);
            errorPopup.container['popupText'].set_text(text);
            errorPopup.open();
        } else {
            alert(text);
        }
    },

    _clientBoundingRect: null,

    _get_boundingRect: function () {
        $appDom = $(this.get_configuration().get_domElement());
        $appDom.children('*').hide(); // we must hide the content of dom element to properly handle the size

        Application._clientBoundingRect = {
            width: $appDom.width(),
            height: $appDom.height()
        }

        $appDom.children('*').show();

        return Application._clientBoundingRect;
    },

    /**
    * Returns the width of the application
    */
    get_clientWidth: function () {
        if (!Application._clientBoundingRect) {
            Application._clientBoundingRect = Application._get_boundingRect();
        }

        return Application._clientBoundingRect.width;
    },

    /**
    * Returns the height of the application
    */
    get_clientHeight: function () {
        if (!Application._clientBoundingRect) {
            Application._clientBoundingRect = Application._get_boundingRect();
        }

        return Application._clientBoundingRect.height;
    }
};

Application.registerUrlFilter("javascript:", function(fullUrl, part) {
    return fullUrl;
});

Application.registerUrlFilter("page:", function(fullUrl, part) {
    return String.format("javascript:Application.loadPage(\"{0}\");", part);
});

Auto.Properties(Application, [
    'silentMode'
]);

Auto.Events(Application, [
    'onInitialized',
    'onPagePreLoading',
    'onPageLoading',
    'onPageLoad',
    'onPageUnload',
    'onError',
    'onKeyDown',
    'onUpdating',
    'onUpdated',
    'onSiteMapInitialized'
]);
Configuration = function() { };

Configuration.prototype = {
    _configuration: null,
    
    // ======== Properties ========
    get_pagesServiceUrl: function() {
        return (this._configuration['pagesServiceUrl'] || '');
    },
    
    set_pagesServiceUrl: function(value) {
        this._configuration['pagesServiceUrl'] = value;
        return value;
    },
    
    get_rootUrl: function() {
        return (this._configuration['rootUrl'] || '');
    },

    get_error403Uri: function() {
        return (this._configuration['error403Uri'] || 'error403');
    },
    
    get_error404Uri: function() {
        return (this._configuration['error404Uri'] || 'error404');
    },

    get_startPageUri: function() {
        return (this._configuration['startPageUri'] || '');
    },
    
    set_startPageUri: function(value) {
        this._configuration['startPageUri'] = value;
    },
    
    set_rootUrl: function(value) {
        this._configuration['rootUrl'] = value;
        return value;
    },
    
    get_masterPage: function() {
        return (this._configuration['masterPage'] || '');
    },
    
    get_isDebug: function() {
        return !!this._configuration['isDebug'];
    },

    get_globalErrorHandling: function() {
        return !!this._configuration['globalErrorHandling'];
    },

    set_masterPage: function(value) {
        this._configuration['masterPage'] = value;
    },
    
    get_blankImageUrl: function() {
        return Application.resolveUrl(this._configuration['blankImageUrl'] || '~/Images/0.gif');
    },
    
    get_domElement: function() { return this._configuration['domElement']; },
    
    // ======== Methods ========
    init: function(configuration) {
        this._configuration = configuration;
    }
};

Configuration.createClass('Configuration');
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
var DepthManager = {
    __lastZIndex: 10000,

    getNewZIndex: function() {
        DepthManager.__lastZIndex++;

        return DepthManager.__lastZIndex;
    }
};
var Color = {
    GetForeground: function (backgroundColor) {
        var 
            re=/^\#(\w{2})(\w{2})(\w{2})$/,
            rgb=re.exec(backgroundColor);

        var 
            r=parseInt(rgb[1],16),
            g=parseInt(rgb[2],16),
            b=parseInt(rgb[3],16);

        var brightness=(r*299+g*587+b*114)/1000;

        if(brightness>125) {
            return "#000000";
        } else {
            return "#FFFFFF";
        }
    },

    GetRandom: function () {
        var rint = Math.round(0xffffff * (Math.random() * 0.7 + 0.3));
        return ('#0'+rint.toString(16)).replace(/^#0([0-9a-f]{6})$/i,'#$1');
    }
};
var logBuffer = {};
var layoutTime = 0;
var layoutCount = 0;

Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.BaseLayout = function (control) {
    Nimble.Core.LayoutEngines.BaseLayout.constructBase(this);
    this._control = control;
};

var ___baseLayout = Nimble.Core.LayoutEngines.BaseLayout;

Nimble.Core.LayoutEngines.BaseLayout.__getDomTester = function(width, height) {
    var domTester = Nimble.Core.LayoutEngines.BaseLayout.__domTester;
    
    if (!domTester) {
        domTester = Nimble.Core.LayoutEngines.BaseLayout.__domTester = DOM.create('div', document.body);
    } else {
        domTester.innerHTML = '';
    }
    
    domTester.style.cssText = 'position: absolute; left: 0; top: 0; visibility: hidden; width: ' + (width ? (width + 'px') : '""') + '; height: ' + (height ? (height + 'px') : '""') + ';';

    return domTester;
};

Nimble.Core.LayoutEngines.BaseLayout.prototype = {
    _control: null,

    // #region Abstract methods
    
    _getChildsWidth: function() {
        throw new Error('Abstract method');
    },

    _getChildsHeight: function() {
        throw new Error('Abstract method');
    },

    _getChildStretchedMeasure: function(child, measure) {
        throw new Error('Abstract method');
    },

    get_childPosition: function(childControl) {
        throw new Error('Abstract method');
    },

    get_domWidth: function() {
        throw new Error('Abstract method');
    },

    get_domHeight: function() {
        throw new Error('Abstract method');
    },

    // #endregion

    // #region Autosize, DOM

    dropFontPropertiesCache: function() {
        var control = this._control,
            domElement = this._control.domElement;

        domElement.__fontFamily = null;
        domElement.__fontSize = null;
        domElement.__lineHeight = null;
    },

    _getDomWidth: function() {
        var parent = this._control.parent,
            control = this._control,
            result = null;
        
        if (parent.isDependsOnChildWidth() || this._containsStretched(parent.controls, 'width')) {
            var domElement = this._control.domElement;
            
            var fontFamily = domElement.__fontFamily;
            var fontSize = domElement.__fontSize;
            
            if (!fontFamily) {
                fontFamily = DOM.getCssProperty(domElement, "font-family", "fontFamily");
                domElement.__fontFamily = fontFamily;
            }

            if (!fontSize) {
                fontSize = DOM.getCssProperty(domElement, "font-size", "fontSize");
                domElement.__fontSize = fontSize;
            }

            var text = domElement.innerText || domElement.textContent || '';

            var isTextNode = DOM.isTextNode(domElement);

            result = isTextNode && Application.getTextWidth ? Application.getTextWidth(text, fontFamily, fontSize.replace('px', '')*1) : null;

            if (result == null) {
                var limitHeight = control.get_height().isAutoSize() ? parent.get_innerHeight() - control.get_padding().get_height() - control.get_border().get_height() : control.get_innerHeight();
                var _domTester = Nimble.Core.LayoutEngines.BaseLayout.__getDomTester('', limitHeight);
                    
                var item = domElement.cloneNode(true);
                item.style.cssText = 'position: static; margin: 0; padding: 0; border: 0; float: none; display: inline; width: ""; height: ""; font-family: ' + fontFamily + '; font-size: ' + fontSize + ';';

                _domTester.appendChild(item);
                result = _domTester.offsetWidth;
            }
        }

        return result;
    },

    _getDomHeight: function() {
        var result = null,
            control = this._control,
            parent = this._control.parent;
        
        if (parent.isDependsOnChildHeight() || this._containsStretched(parent.controls, 'height')) {
            var domElement = this._control.domElement;

            var fontFamily = domElement.__fontFamily;
            var fontSize = domElement.__fontSize;
            var lineHeight = domElement.__lineHeight;
            
            if (!fontFamily) {
                fontFamily = DOM.getCssProperty(domElement, "font-family", "fontFamily");
                domElement.__fontFamily = fontFamily;
            }

            if (!fontSize) {
                fontSize = DOM.getCssProperty(domElement, "font-size", "fontSize");
                domElement.__fontSize = fontSize;
            }

            if (!lineHeight) {
                lineHeight = DOM.getCssProperty(domElement, "line-height", "lineHeight");
                domElement.__lineHeight = lineHeight;
            }

            var limitWidth = control.get_width().isAutoSize() ? parent.get_innerWidth() - control.get_padding().get_width() - control.get_border().get_width() : control.get_innerWidth();

            if (limitWidth < 0) {
                limitWidth = 0;
            }

            var isTextNode = DOM.isTextNode(domElement);

            if (isTextNode && Application.getTextHeight) {
                var lineHeightUnit = 0;

                var fontSize = fontSize.replace('px', '')*1;
            
                if (lineHeight == 'normal') {
                    lineHeightUnit = fontSize * 1.4;
                }

                if (lineHeight.endsWith('pt')) {
                    lineHeightUnit = Math.round(lineHeight.replace('pt','') * 4 / 3);
                }

                if (lineHeight.endsWith('%') || lineHeight.endsWith('px')) {
                    var unit = new DimensionUnit(lineHeight);
                    var lineHeightUnit = unit.percent * fontSize + unit.pixels;
                }

                var text = domElement.innerText || domElement.textContent || '';

                result = Application.getTextHeight(text, fontFamily, fontSize, lineHeightUnit, limitWidth*1);
            }

            if (result == null) {
                var _domTester = Nimble.Core.LayoutEngines.BaseLayout.__getDomTester(limitWidth, '');

                var item = domElement.cloneNode(true);
                item.style.cssText = 'position: static; margin: 0; padding: 0; border: 0; float: none; display: inline; line-height: ' + lineHeight + '; width: ""; height: ""; font-family: ' + fontFamily + '; font-size: ' + fontSize + ';';

                _domTester.appendChild(item);
                result = _domTester.offsetHeight;
            }
        }

        return result;
    },

    // #endregion

    isDependsOnChildSize: function() {
        return this.isDependsOnChildWidth() || this.isDependsOnChildHeight();
    },

    isDependsOnChildWidth: function() {
        return this._control.get_width().isAutoSize();
    },

    isDependsOnChildHeight: function() {
        return this._control.get_height().isAutoSize();
    },

    _getGetters: function(measure) {
        // some magic to increase performance
        var isWidthMeasure = measure == 'width';
        var cacheKey = isWidthMeasure ? '_getChildClientMeasure$getters$width' : '_getChildClientMeasure$getters$height';
        
        if (!___baseLayout[cacheKey]) {
            var pascalCaseMeasure = measure == 'width' ? 'Width' : 'Height';
            
            Nimble.Core.LayoutEngines.BaseLayout[cacheKey] = {
                getMeasure: 'get_' + measure,
                getStretchBaseMeasure: '_getStretchBase' + pascalCaseMeasure,
                getStretchedMeasureForLastChild: '_getStretched' + pascalCaseMeasure + 'ForLastChild',
                getChildsDomMeasure: '_getChilds' + pascalCaseMeasure,
                getInnerMeasure: 'get_inner' + pascalCaseMeasure
            };
        }

        return ___baseLayout[cacheKey];
    },

    _getTargetRect: function(child) {
        return this._control;
    },

    _getChildClientMeasure: function(child, measure) {
        if (!child._instantiatedInDom) {
            return null;
        }

        var targetRect = this._getTargetRect(child),
            getters = this._getGetters(measure),
            result = 0;

        var measureValue = child[getters['getMeasure']]();
        var parentMeasure = targetRect[getters['getMeasure']]();

        // measure: *
        if (measureValue.isStretched()) {
            result = this._getChildStretchedMeasure(child, measure);
        // measure: ?
        } else if (measureValue.isAutoSize()) {
            result = child.get_layoutEngine()[getters['getChildsDomMeasure']]();
        // measure: px, or %
        } else {
            result = measureValue.pixels;

            if (measureValue.percent > 0) {
                if (child.parent && !parentMeasure.isAutoSize()) {
                    var parentMeasure = targetRect[getters['getInnerMeasure']]();
                    result += measureValue.percent * parentMeasure;
                } else {
                    console.warn("Warning: relatively sized child should not be contained in auto-sized parent", child.options);
                }
            }
        }

        if (result && result < 0) {
            result = 0;
        }

        return result;
    },

    _recalculateNotFlowWidth: function() {
        var width = this._control.get_width();
        var clientWidth = null;

        if (width.isAutoSize()) {
            clientWidth = this._getChildsWidth();
        } else if (width.isStretched()) {
            clientWidth = Application.get_clientWidth();
        } else {
            clientWidth = Application.get_clientWidth() * width.percent + width.pixels
        }

        this._control._set_clientWidth(clientWidth);
    },

    recalculateClientWidth: function() {
        if (!this._control._visible) {
            this._control._set_clientWidth(0);
        } else {
            var width = this._control.get_width();

            if (width.isFixed()) {
                this._control._set_clientWidth(width.pixels);
            } else {
                if (!this._control.parent || !this._control._inDocumentFlow) {
                    this._recalculateNotFlowWidth();
                } else {
                    this._control._set_clientWidth(this._control.parent.get_layoutEngine().get_childClientWidth(this._control));
                }
            }
        }
    },

    _recalculateNotFlowHeight: function() {
        var height = this._control.get_height();
        var clientHeight = null;

        if (height.isAutoSize()) {
            clientHeight = this._getChildsHeight();
        } else if (height.isStretched()) {
            clientHeight = Application.get_clientHeight();
        } else {
            clientHeight = Application.get_clientHeight() * height.percent + height.pixels
        }

        this._control._set_clientHeight(clientHeight);
    },

    recalculateClientHeight: function() {
        if (!this._control._visible) {
            this._control._set_clientHeight(0);
        } else {
            var height = this._control.get_height();

            if (height.isFixed()) {
                this._control._set_clientHeight(height.pixels);
            } else {
                if (!this._control._inDocumentFlow || !this._control.parent) {
                    this._recalculateNotFlowHeight();
                } else {
                    this._control._set_clientHeight(this._control.parent.get_layoutEngine().get_childClientHeight(this._control));
                }
            }
        }
    },

    get_position: function() {
        if (this._control.parent) {
            return this._control.parent.get_layoutEngine().get_childPosition(this._control);
        } else {
            return { x: 0, y: 0 };
        }
    },

    get_childClientWidth: function(childControl) {
        return this._getChildClientMeasure(childControl, 'width');
    },

    get_childClientHeight: function(childControl) {
        return this._getChildClientMeasure(childControl, 'height');
    },

    updateDom: function() {
        var control = this._control;
        var clientWidth = control.get_clientWidth();
        var clientHeight = control.get_clientHeight();
        var padding = control.get_padding();
        var border = control.get_border();
        var margin = control.get_margin();
        var width = clientWidth !== null ? (Math.max(clientWidth - padding.get_width() - border.get_width(), 0)) : null;
        var height = clientHeight !== null ? (Math.max(clientHeight - padding.get_height() - border.get_height(), 0)) : null;

        DOM.setBoundingRect(this._control.domElement, width, height);
    },

    updateChildDom: function(child) {
    },

    __getChildsForUpdate: function(control) {
        if (!control.get_visible()) {
            return [];
        }

        // copy childs array and sort it
        // non-strethed controls should be at first place
        // width-strethed controls should be at second place
        // height-stretched controls should be at third place
        var childs = control.controls.slice(0);
        var orientation = control.options.orientation || 'vertical';
        var isHorizontal = orientation == 'horizontal';

        if (childs.length > 1) {
            childs.sort(function $sort (a, b) {
                if (isHorizontal) {
                    var aWidth = a.get_width().isStretched(), bWidth = b.get_width().isStretched();
                    if (aWidth == bWidth) { return a.__cid < b.__cid ? 1 : -1; }
                    if (aWidth) { return -1; }
                    if (bWidth) { return 1; }
                } else {
                    var aHeight = a.get_height().isStretched(), bHeight = b.get_height().isStretched();
                    if (aHeight == bHeight) { return a.__cid < b.__cid ? 1 : -1; }
                    if (aHeight) { return -1; }
                    if (bHeight) { return 1; }
                }
            });
        }
                
        return childs;
    },

    _updateItem: function(control, updating, visited) {
        var containsInVisited = !!visited[control.__cid];
        var dependsOnChildSize = control.isDependsOnChildSize();

        if (!containsInVisited) {
            var childs = this.__getChildsForUpdate(control);

            if (dependsOnChildSize && childs.length == 0) {
                visited[control.__cid] = true;
                containsInVisited = true; // if there are no childs in control then we can think that all childs are already updated
            } else {
                updating.add(control); // add control to queue. We must invoke postUpdate after all its controls would be updated
                updating.add(childs);
            }
        }

        // if we could calculate at least one of control's measure then we should do that
        if (dependsOnChildSize && !containsInVisited) {
            if (!control.isDependsOnChildWidth()) {
                control.get_layoutEngine().recalculateClientWidth();
            }

            if (!control.isDependsOnChildHeight()) {
                control.get_layoutEngine().recalculateClientHeight();
            }
        }

        // autosized control updates only in the second pass, other controls updates normally
        if ((!containsInVisited && !dependsOnChildSize) || (containsInVisited && dependsOnChildSize)) {
            control.get_layoutEngine().recalculateClientWidth();
            control.get_layoutEngine().recalculateClientHeight();
        }

        if (containsInVisited) {
            control.postUpdate();
        } else {
            visited[control.__cid] = true;
        }
    },

    update: function() {
        var time = new Date();
        var visited = {};
        var updating = [this._control];

        if (this._control.parent && this._control.isInDocumentFlow()) {
            var targetParent = this._control.parent;

            // find closest not auto-sized parent
            while (targetParent.parent && targetParent.isDependsOnChildSize()) {
                targetParent = targetParent.parent;
            }
            
            updating.add(targetParent);
        }

        while (updating.length > 0) {
            var control = updating.pop();
            this._updateItem(control, updating, visited);
        }

        for (var i in visited) {
            var control = Controls[i];
            
            if (control.isAttachedToDom()) {
                control.updateDom();
            }
        }

        layoutTime += new Date() - time;
        layoutCount++;

        return visited;
    },

    postUpdate: function() {
    }
};

Nimble.Core.LayoutEngines.BaseLayout.createClass('Nimble.Core.LayoutEngines.BaseLayout');
Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.StackLayout = function (control) {
    Nimble.Core.LayoutEngines.StackLayout.constructBase(this, [control]);
    this._orientation = control.options.orientation || 'vertical';
};

Nimble.Core.LayoutEngines.StackLayout.prototype = {
    _orientation: null,

    _isHorizontal: function() {
        return this._orientation == 'horizontal';
    },

    _isVertical: function() {
        return !this._isHorizontal();
    },

    // #region Stretched

    _getStretchBaseWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var thisWidth = this._control.get_innerWidth();
        var resultWidth = thisWidth;
        var stretchControls = 0;

        for (var i = 0; i < controls.length; i++) {
            var width = controls[i].get_width();

            if (this._isHorizontal()) {
                if (!width.isStretched()) {
                    resultWidth -= controls[i].get_outerWidth();
                } else {
                    stretchControls += width.getStretchLevel();
                    resultWidth -= controls[i].get_margin().get_width();
                }
            } else {
                if (width.isStretched()) {
                    var level = width.getStretchLevel();
                
                    if (level > stretchControls)
                        stretchControls = level;
                }
            }
        }

        if (stretchControls == 0)
            return { base: 0, length: 0 };

        return { base: resultWidth / stretchControls, length: stretchControls };
    },

    _getStretchBaseHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var stretchControls = 0;
        var thisHeight = this._control.get_innerHeight();
        var resultHeight = thisHeight;

        for (var i = 0; i < controls.length; i++) {
            var height = controls[i].get_height();

            if (this._isHorizontal()) {
                if (height.isStretched()) {
                    var level = height.getStretchLevel();
                
                    if (level > stretchControls)
                        stretchControls = level;
                }
            } else {
                if (!height.isStretched()) {
                    resultHeight -= controls[i].get_outerHeight();
                } else {
                    stretchControls += height.getStretchLevel();
                    resultHeight -= controls[i].get_margin().get_height();
                }
            }
        }

        if (stretchControls == 0)
            return { base: 0, length: 0 };

        return { base: resultHeight / stretchControls, length : stretchControls };
    },

    _getStretchedWidthForLastChild: function(child) {
        var resultWidth = child.get_margin().get_width();

        if (this._isHorizontal()) {
            for (var i = 0, length = this._control.controls.length; i < length; i++) {
                var control = this._control.controls[i];

                if (control.isInDocumentFlow() && control != child) {
                    resultWidth += control.get_outerWidth();
                }
            }
        }

        return this._control.get_innerWidth() - resultWidth;
    },

    _getStretchedHeightForLastChild: function(child) {
        var resultHeight = child.get_margin().get_height();

        if (this._isVertical()) {
            for (var i = 0, length = this._control.controls.length; i < length; i++) {
                var control = this._control.controls[i];

                if (control.isInDocumentFlow() && control != child) {
                    resultHeight += control.get_outerHeight();
                }
            }
        }

        return this._control.get_innerHeight() - resultHeight;
    },

    _containsStretched: function(controls, measure) {
        var getMeasure = 'get_' + measure;

        for (var i = 0; i < controls.length; i++) {
            if (controls[i][getMeasure]().isStretched) {
                return true;
            }
        }

        return false;
    },

    // returns true if @child is last stretched child in list of controls
    _isLastStretched: function(child, measure) {
        var isLastStretched = true;
        var otherChilds = this._control.controls;

        for (var i = otherChilds.length - 1; i >= 0 && otherChilds[i] !== child; i--) {
            if (otherChilds[i]['get_' + measure]().isStretched()) {
                isLastStretched = false;
            }
        }

        return isLastStretched;
    },

    _getChildStretchedMeasure: function(child, measure) {
        var result = null,
            targetRect = this._getTargetRect(child),
            getters = this._getGetters(measure);

        var parentMeasure = targetRect[getters['getMeasure']]();

        if (!child.parent || parentMeasure.isAutoSize()) {
            console.warn("Warning: stretched child should not be contained in auto-sized parent", child.options);
            return 0;
        }

        if ((this._isHorizontal() ? measure == 'width' : measure == 'height') && this._isLastStretched(child, measure)) {
            result = this[getters['getStretchedMeasureForLastChild']](child);
        } else {
            var measureValue = child[getters['getMeasure']]();
            var stretchBase = this[getters['getStretchBaseMeasure']]();
            var stretchLevel = measureValue.getStretchLevel();
            result = Math.round(stretchBase.base * stretchLevel);

            if (this._isHorizontal() && measure == 'height') {
                result -= child.get_margin().get_height();
            }
                    
            if (!this._isHorizontal() && measure == 'width') {
                result -= child.get_margin().get_width();
            }
        }

        return result;
    },

    // #endregion

    _getChildsWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;

        if (controls.length == 0) {
            result = this._getDomWidth();
        } else {
            result = 0;

            if (!this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var width = controls[i].get_outerWidth();

                    if (width > result)
                        result = width;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_outerWidth();
                }
            }
        }
        
        if (result !== null) {
            var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();
            result += offset;
        }

        return result;
    },

    _getChildsHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;
        
        if (controls.length == 0) {
            result = this._getDomHeight();
        } else {
            result = 0;

            if (this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var height = controls[i].get_outerHeight();

                    if (height > result)
                        result = height;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_outerHeight();
                }
            }
        }

        if (result !== null) {
            var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();
            result += offset;
        }

        return result;
    },

    get_domWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = 0;

        if (controls.length == 0) {
            if (this._control.get_width().pixels > 0) {
                result = this._control.get_width().pixels;
            } else {
                result = this._control.get_width().isAutoSize() ? this._getDomWidth() : 0;
            }
        } else {
            result = 0;

            if (!this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var width = controls[i].get_domWidth();

                    if (width > result)
                        result = width;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_domWidth();
                }
            }
            
            var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();
            result += offset;
        }

        if (this._control.options.minWidth > result) {
            result = this._control.options.minWidth;
        }

        return result;
    },
    
    get_domHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = 0;
        
        if (controls.length == 0) {
            if (this._control.get_height().pixels > 0) {
                result = this._control.get_height().pixels - this._control.get_border().get_height();
            } else {
                result = this._control.get_height().isAutoSize() ? this._getDomHeight() : 0;
            }
        } else {
            result = 0;

            if (this._isHorizontal()) {
                for (var i = 0; i < controls.length; i++) {
                    var height = controls[i].get_domHeight();

                    if (height > result)
                        result = height;
                }
            } else {
                for (var i = 0; i < controls.length; i++) {
                    result += controls[i].get_domHeight();
                }
            }
            
            var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();
            result += offset;
        }

        if (this._control.options.minHeight > result) {
            result = this._control.options.minHeight;
        }

        return result;
    },

    get_childPosition: function(childControl) {
        var controls = this._control.controls.getControlsInFlow();
        var left = 0;
        var top = 0;
        var thisIsHorizontal = this._isHorizontal();
        var options = childControl.options;
        var margin = childControl.get_margin();

        for (var i = 0; i < controls.length; i++) {
            if (controls[i] == childControl) {
                break;
            }

            if (thisIsHorizontal) {
                left += controls[i].get_clientWidth();
            } else {
                top += controls[i].get_clientHeight();
            }
        }

        if (thisIsHorizontal) {
            if (options.valign == 'middle') {
                top = Math.floor((this._control.get_innerHeight() - childControl.get_clientHeight()) / 2);
            } else if (options.valign == 'bottom') {
                top = Math.floor(this._control.get_innerHeight() - childControl.get_clientHeight());
            }
        } else {
            if (options.halign == 'center') {
                left = Math.floor((this._control.get_innerWidth() - childControl.get_clientWidth()) / 2);
            } if (options.halign == 'right') {
                left = Math.floor(this._control.get_innerWidth() - childControl.get_clientWidth());
            }
        }

        return { x: left + margin.left, y: top + margin.top };
    },

    _updateChildDom: function(child) {
        var position = child.get_layoutEngine().get_position();

        if (this._isHorizontal()) {
            DOM.setFloat(child.domElement.style, 'left');
            child.domElement.style.marginTop = position.y + 'px';
        } else {
            child.domElement.style.marginLeft = position.x + 'px';
        }
    },

    postUpdate: function() {
        var controls = this._control.controls.getControlsInFlow();

        for (var i = 0; i < controls.length; i++) {
            this._updateChildDom(controls[i]);
        }
    }
};

Nimble.Core.LayoutEngines.StackLayout.createClass('Nimble.Core.LayoutEngines.StackLayout', Nimble.Core.LayoutEngines.BaseLayout);
Nimble.Core.LayoutEngines.LayoutFactory.register('stack', Nimble.Core.LayoutEngines.StackLayout);
Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.GridLayout = function (control) {
    Nimble.Core.LayoutEngines.GridLayout.constructBase(this, [ control ]);
    this._columnsOptions = (control.options['columns'] || [ '*' ]).makeObservable();
    this._rowsOptions = (control.options['rows'] || [ '*' ]).makeObservable();
    this._columns = [];
    this._rows = [];
    this._hspacing = control.options['hspacing']*1 || 0;
    this._vspacing = control.options['vspacing']*1 || 0;
    this.updateRowsAndColumns();

    this._rowsOptions.add_changed(this._initRows, this);
    this._columnsOptions.add_changed(this._initColumns, this);
};

Nimble.Core.LayoutEngines.GridLayout.prototype = {
    get_rows: function() {
        return this._rowsOptions;
    },

    get_columns: function() {
        return this._columnsOptions;
    },

     _getTargetRect: function(child) {
        var gridRow = this._getRowId(child);
        var gridColumn = this._getColumnId(child);

        return (function(obj) {
            return {
                get_width: function() {
                    return new DimensionUnit(obj._columns[gridColumn].innerWidth);
                },

                get_height: function() {
                    return new DimensionUnit(obj._rows[gridRow].innerHeight);
                },

                get_innerWidth: function() {
                    return obj._columns[gridColumn].innerWidth;
                },

                get_innerHeight: function() {
                    return obj._rows[gridRow].innerHeight;
                },

                get_offsetLeft: function() {
                    return obj._columns[gridColumn].offsetLeft;
                },
                
                get_offsetTop: function() {
                    return obj._rows[gridRow].offsetTop;
                }
            }
        })(this);
     },

     updateRowsAndColumns: function() {
        this._initRows();
        this._initColumns();
     },

     _initRows: function() {
        if (!isArray(this._rowsOptions)) {
            throw new Error('Rows is undefined in grid');
        }

        this._rowsStrechLevel = 0;

        for (var i = 0; i < this._rowsOptions.length; i++) {
            this._rows[i] = {
                height: new DimensionUnit(this._rowsOptions[i]),
                innerHeight: 0,
                offsetTop: 0
            }

            this._rowsStrechLevel += this._rows[i].height.getStretchLevel();
        }
     },

     _initColumns: function() {
        if (!isArray(this._columnsOptions)) {
            throw new Error('Columns is undefined in grid');
        }

        this._columnsStrechLevel = 0;

        for (var i = 0; i < this._columnsOptions.length; i++) {
            this._columns[i] = {
                width: new DimensionUnit(this._columnsOptions[i]),
                innerWidth: 0,
                offsetLeft: 0
            }

            this._columnsStrechLevel += this._columns[i].width.getStretchLevel();
        }
     },

     _getChilds: function() {
        return this._control.controls.getControlsInFlow();
     },

     _getChildsByColumnId: function(columnId) {
        var columnControls = [];
        var control = this._control;
        var columnId = columnId * 1;

        if (columnId < 0 || columnId > this._columns.length) {
            throw new Error('Wrong column identifier');
        }

        var childs = this._getChilds();

        for (var i = 0; i < childs.length; i++) {
            var child = childs[i];

            if (this._getColumnId(child) === columnId) {
                columnControls.add(child);
            }
        }

        return columnControls;
     },

     _calculateColumnAutoSizeWidth: function(columnId) {
        var controls = this._getChildsByColumnId(columnId);
        var maxWidth = 0;

        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];

            var width = control.get_outerWidth();

            if (control.get_width().isStretched()) {
                width = control.options.minWidth || 0;
            }

            if (width > maxWidth) {
                maxWidth = width;
            }
        }

        return maxWidth;
     },

     _updateColumnsClientWidth: function() {
        var stretchBase = 0,
            clientWidth =  this._control.get_width().isAutoSize() ? 0 : (this._control.get_innerWidth() - this._hspacing * (this._columns.length - 1)),
            usedWidth = 0,
            lastStretched = null;

        // First pass: fixed size columns
        for (var i = 0; i < this._columns.length; i++) {
            var columnWidth = this._columns[i].width;
            var width = 0;
            var isLast = i == this._columns.length - 1;

            if (columnWidth.isStretched()) {
                lastStretched = i;
                continue;
            } else if (columnWidth.isAutoSize()) {
                width = this._calculateColumnAutoSizeWidth(i);
                //throw new Error('Grid layout is not support auto-size');
            } else {
                width = Math.round(columnWidth.percent * clientWidth + columnWidth.pixels);
            }

            this._columns[i].innerWidth = width;
            usedWidth += width;
        }
        
        // Second pass: stretch
        if (this._columnsStrechLevel > 0) {
            stretchBase = (clientWidth - usedWidth) / this._columnsStrechLevel;
        }

        for (var i = 0; i < this._columns.length; i++) {
            var columnWidth = this._columns[i].width;
            
            if (columnWidth.isStretched()) {
                var width = 0;

                if (lastStretched != i) {
                    width = Math.round(columnWidth.getStretchLevel() * stretchBase);
                    usedWidth += width;
                } else {
                    width = clientWidth - usedWidth;
                }

                this._columns[i].innerWidth = Math.max(width, 0);
            }
            
            this._columns[i].offsetLeft = (i > 0 ? (this._columns[i - 1].innerWidth + this._columns[i - 1].offsetLeft + this._hspacing) : 0);
        }
     },

     _getRowId: function(child) {
        var gridRow = isDefined(child.options['grid.row']) ? (child.options['grid.row']*1 - 1) : 0;
        
        if (gridRow > this._rows.length || gridRow < 0) {
            throw new Error('"' + (gridRow + 1) + '" is not valid row number');
        }

        return gridRow;
     },

     _getColumnId: function(child) {
        var gridColumn = isDefined(child.options['grid.column']) ? (child.options['grid.column']*1 - 1) : 0;
        
        if (gridColumn > this._columns.length || gridColumn < 0) {
            throw new Error('"' + (gridColumn + 1) + '" is not valid column number');
        }

        return gridColumn;
     },

     _getChildsByRowId: function(rowId) {
        var rowControls = [];
        var control = this._control;
        var rowId = rowId * 1;

        if (rowId < 0 || rowId > this._rows.length) {
            throw new Error('Wrong row identifier');
        }

        var childs = this._getChilds();

        for (var i = 0; i < childs.length; i++) {
            var child = childs[i];

            if (this._getRowId(child) === rowId) {
                rowControls.add(child);
            }
        }

        return rowControls;
     },

     _calculateRowAutoSizeHeight: function(rowId) {
        var controls = this._getChildsByRowId(rowId);
        var maxHeight = 0;

        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];
            var height = control.get_outerHeight();

            if (control.get_height().isStretched()) {
                height = control.options.minHeight || 0;
            }

            if (height > maxHeight) {
                maxHeight = height;
            }
        }

        return maxHeight;
     },

     _updateRowsClientHeight: function() {
        var stretchBase = 0,
            clientHeight = this._control.get_height().isAutoSize() ? 0 : (this._control.get_innerHeight() - this._vspacing * (this._rows.length - 1)),
            usedHeight = 0,
            lastStretched = null;

        // First pass: fixed size rows
        for (var i = 0; i < this._rows.length; i++) {
            var rowHeight = this._rows[i].height;
            var height = 0;

            if (rowHeight.isStretched()) {
                lastStretched = i;
                continue;
            } else if (rowHeight.isAutoSize()) {
                height = this._calculateRowAutoSizeHeight(i);
            } else {
                height = Math.round(rowHeight.percent * clientHeight + rowHeight.pixels);
            }

            this._rows[i].innerHeight = height;
            usedHeight += height;
        }

        // Second pass: stretch
        if (this._rowsStrechLevel > 0) {
            stretchBase = (clientHeight - usedHeight) / this._rowsStrechLevel;
        }

        for (var i = 0; i < this._rows.length; i++) {
            var rowHeight = this._rows[i].height;
            
            if (rowHeight.isStretched()) {
                if (lastStretched != i) {
                    height = Math.round(rowHeight.getStretchLevel() * stretchBase);
                    usedHeight += height;
                } else {
                    height = clientHeight - usedHeight;
                }

                this._rows[i].innerHeight = Math.max(height, 0);
            }

            this._rows[i].offsetTop = (i > 0 ? (this._rows[i - 1].innerHeight + this._rows[i - 1].offsetTop + this._vspacing) : 0);
        }
     },

     recalculateClientWidth: function() {
        var isAutoSize = this._control.get_width().isAutoSize();

        if (!isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientWidth");
        }

        this._updateColumnsClientWidth();

        if (isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientWidth");
        }
     },

    _getChildClientMeasure: function(child, measure) {
        if (!child['get_' + measure]().isAutoSize()) {
            if (measure == 'height') {
                this._updateRowsClientHeight();
            }

            if (measure == 'width') {
                this._updateColumnsClientWidth();
            }
        }

        return Nimble.Core.LayoutEngines.GridLayout.callBase(this, "_getChildClientMeasure", [ child, measure ]);
    },
     
     recalculateClientHeight: function() {
        var isAutoSize = this._control.get_height().isAutoSize();

        if (!isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientHeight");
        }

        this._updateRowsClientHeight();

        if (isAutoSize) {
            Nimble.Core.LayoutEngines.GridLayout.callBase(this, "recalculateClientHeight");
        }
     },

    _isLastStretched: function(child, measure) {
        return true;
    },

    _getChildStretchedMeasure: function(child, measure) {
        var rect = this._getTargetRect(child);
        var getters = this._getGetters(measure);
            
        return rect[getters['getInnerMeasure']]() - child.get_margin()[getters['getMeasure']]();
    },
    
    _getChildsWidth: function() {
        var innerWidth = 0;

        for (var i = 0; i < this._columns.length; i++) {
            innerWidth += this._columns[i].innerWidth + this._hspacing;
        }

        var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();

        return innerWidth - this._hspacing + offset; // 3 columns, 2 spacings
    },

    _getChildsHeight: function() {
        var innerHeight = 0;

        for (var i = 0; i < this._rows.length; i++) {
            innerHeight += this._rows[i].innerHeight + this._vspacing;
        }

        var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();

        return innerHeight - this._vspacing + offset; // 3 rows, 2 spacings
    },

    get_domWidth: function() {
        this._updateColumnsClientWidth();
        return this._getChildsWidth();
    },
    
    get_domHeight: function() {
                this._updateRowsClientHeight();
        return this._getChildsHeight();
    },

    get_childPosition: function(childControl) {
        var rect = this._getTargetRect(childControl);
        var pos = { x: rect.get_offsetLeft(), y: rect.get_offsetTop() };
        var halign = childControl.options.halign;
        var valign = childControl.options.valign;

        if (halign && halign != 'left') {
            var dw = rect.get_innerWidth() - childControl.get_clientWidth();
            pos.x += halign == 'center' ? Math.round(dw / 2) : dw;
        }

        if (valign && valign != 'top') {
            var dh = rect.get_innerHeight() - childControl.get_clientHeight();
            pos.y += valign == 'middle' ? Math.round(dh / 2) : dh;
        }

        return pos;
    },

    postUpdate: function() {
        var controls = this._getChilds();

        for (var i = 0; i < controls.length; i++) {
            this._updateChildDom(controls[i]);
        }
    },

    updateDom: function() {
        Nimble.Core.LayoutEngines.GridLayout.callBase(this, "updateDom");
        this._control.domElement.style.position = 'relative';
    },

    _updateChildDom: function(child) {
        var position = child.get_layoutEngine().get_position();

        child.domElement.style.position = 'absolute';
        child.domElement.style.left = (position.x + this._control.get_padding().left) + 'px';
        child.domElement.style.top = (position.y + this._control.get_padding().top) + 'px';
    }
};

Nimble.Core.LayoutEngines.GridLayout.createClass('Nimble.Core.LayoutEngines.GridLayout', Nimble.Core.LayoutEngines.BaseLayout);
Nimble.Core.LayoutEngines.LayoutFactory.register('grid', Nimble.Core.LayoutEngines.GridLayout);
Type.createNamespace('Nimble.Core.LayoutEngines');

Nimble.Core.LayoutEngines.WrapLayout = function (control) {
    Nimble.Core.LayoutEngines.WrapLayout.constructBase(this, [ control ]);

    if (this.isDependsOnChildWidth() && this._orientation == 'horizontal') {
        throw new Error('Horizontal wrap layout should have determined width.');
    }
    
    if (this.isDependsOnChildHeight() && this._orientation == 'vertical') {
        throw new Error('Vertical wrap layout should have determined height.');
    }
};

Nimble.Core.LayoutEngines.WrapLayout.prototype = {
    _groupDimensions: null,
    _groupSumMeasure: null,

    _getChildsWidth: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;

        if (controls.length == 0) {
            result = this._getDomWidth();
        } else {
            result = this._groupSumMeasure;
        }
        
        if (result !== null) {
            var offset = this._control.get_border().get_width() + this._control.get_padding().get_width();
            result += offset;
        }

        return result;
    },

    _getChildsHeight: function() {
        var controls = this._control.controls.getControlsInFlow();
        var result = null;
        
        if (controls.length == 0) {
            result = this._getDomHeight();
        } else {
            result = this._groupSumMeasure;
        }

        if (result !== null) {
            var offset = this._control.get_border().get_height() + this._control.get_padding().get_height();
            result += offset;
        }

        return result;
    },

    _recalculateGroupIndex: function() {
        this._groupDimensions = [];
        this._groupSumMeasure = 0;

        var controls = this._control.controls.getControlsInFlow(),
            limiterMeasureFuncName = this._isHorizontal() ? 'get_innerWidth' : 'get_innerHeight',
            measureFuncName = this._isHorizontal() ? 'get_outerWidth' : 'get_outerHeight',
            invMeasureFuncName = !this._isHorizontal() ? 'get_outerWidth' : 'get_outerHeight';

        var groupLimiterMeasure = this._control[limiterMeasureFuncName](),
            currentIndex = 0,
            usedMeasure = 0;

        if (groupLimiterMeasure < 1) {
            for (var i = 0; i < controls.length; i++) {
                delete controls[i].$__wrapLayoutGroupIndex;
            }

            return;
        }

        for (var i = 0; i < controls.length; i++) {
            var controlMeasure = controls[i][measureFuncName](),
                invMeasure = controls[i][invMeasureFuncName](); // invMeasure used to determine width of column or height of row
            
            usedMeasure += controlMeasure;

            if (usedMeasure > groupLimiterMeasure) {
                currentIndex++;
                usedMeasure = controlMeasure;
            }

            if (invMeasure > this._groupDimensions[currentIndex] || !this._groupDimensions[currentIndex]) {
                this._groupDimensions[currentIndex] = invMeasure;
            }

            controls[i].$__wrapLayoutGroupIndex = currentIndex;
        }

        for (var i = 0; i < this._groupDimensions.length; i++) {
            this._groupSumMeasure += this._groupDimensions[i];
        }
    },

    get_childPosition: function(childControl) {
        var controls = this._control.controls.getControlsInFlow();
        var left = 0;
        var top = 0;

        for (var i = 0; i < controls.length; i++) {
            if (controls[i] === childControl) {
                break;
            }

            if (controls[i].$__wrapLayoutGroupIndex === childControl.$__wrapLayoutGroupIndex) {
                if (this._isHorizontal()) {
                    left += controls[i].get_outerWidth();
                } else {
                    top += controls[i].get_outerHeight();
                }
            }
        }

        for (var i = 0; i < childControl.$__wrapLayoutGroupIndex; i++) {
            if (!this._isHorizontal()) {
                left += this._groupDimensions[i];
            } else {
                top += this._groupDimensions[i];
            }
        }

        var valign = childControl.options.valign;
        var halign = childControl.options.halign;
        var offsetLeft = 0;
        var offsetTop = 0;

        if (!this._isHorizontal()) {
            if (halign && halign != 'left') {
                offsetLeft = (this._groupDimensions[childControl.$__wrapLayoutGroupIndex] - childControl.get_clientWidth()) * (halign == 'center' ? 0.5 : 1);
                left += offsetLeft;
            }
        } else {
            if (valign && valign != 'top') {
                offsetTop = (this._groupDimensions[childControl.$__wrapLayoutGroupIndex] - childControl.get_clientHeight()) * (valign == 'middle' ? 0.5 : 1);
                top += offsetTop;
            }
        }

        return { x: left, y: top, dx: offsetLeft, dy: offsetTop };
    },

    postUpdate: function() {
        this._recalculateGroupIndex();

        if (!this._isHorizontal()) {
            if (this._control.isDependsOnChildWidth()) {
                this._control._set_clientWidth(this._getChildsWidth());
            }
        } else {
            if (this._control.isDependsOnChildHeight()) {
                this._control._set_clientHeight(this._getChildsHeight());
            }
        }

        this._updateChildPositions();
    },

    _updateChildPositions: function() {
        var controls = this._control.controls.getControlsInFlow();
        
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];

            if (isNullOrUndefined(control.$__wrapLayoutGroupIndex)) {
                continue;
            }

            var position = this.get_childPosition(control);
            var domElement = controls[i].domElement;
            var style = domElement.style;

            if (this._isHorizontal()) {
                DOM.setFloat(style, 'left');
                style.marginTop = position.dy + 'px';
                style.marginBottom = (this._groupDimensions[control.$__wrapLayoutGroupIndex] - control.get_clientHeight() - position.dy) + 'px';
            }
            
            if (this._isVertical()) {
                DOM.setFloat(style, 'left');
                style.marginTop = position.y + 'px';
                style.marginLeft = (position.dx - (position.y > 0 ? this._groupDimensions[control.$__wrapLayoutGroupIndex] : 0)) + 'px';
                var rightOffset = (this._groupDimensions[control.$__wrapLayoutGroupIndex] - control.get_clientWidth() - position.dx) + 'px';
                style.marginRight = rightOffset;
            }
        }
    },
    
    updateChildDom: function(child) {
    }
};

Nimble.Core.LayoutEngines.WrapLayout.createClass('Nimble.Core.LayoutEngines.WrapLayout', Nimble.Core.LayoutEngines.StackLayout);
Nimble.Core.LayoutEngines.LayoutFactory.register('wrap', Nimble.Core.LayoutEngines.WrapLayout);
if (typeof(Controls) == 'undefined') {
    Controls = [];
    Controls.count = 0;
}

/**
 * Control
 * 
 * @return {Control}
 * @constructor
 * @param {Object} options
 */
var Control = function() {
    this.createControlsCollection();
    
    this.__cid = Controls.length;
    Controls[this.__cid] = this;
    Controls.count++;
    this._height = new DimensionUnit();
    this._width = new DimensionUnit();
    this._left = new DimensionUnit();
    this._top = new DimensionUnit();
    this._cssClasses = [];
    this._enabled = true;
    this._inDocumentFlow = true;
    this._level = 0;
    this.options = {};
    this._innerDelta = { width: 0, height: 0 };
    this._padding = { width: 0, height: 0 };

    this._attachedBindings = {};

    if (!this.defaultOptions || !this.defaultOptions.__initialized) {
        this.defaultOptions = Control._initDefaultOptions(Type.getInstanceType(this));
    }
    
    Auto.ApplyEventDisposing(this);
};

Control._counter = 0;
Control._times = 0;

Control._initDefaultOptions = function(type) {
    var baseType = type.get_baseType();
    var options = type.prototype.defaultOptions || {};

    while (baseType) {
        var defOptions = baseType.prototype.defaultOptions;

        if (defOptions) {
            for (var name in defOptions) {
                if (options[name] !== undefined)
                    continue;

                options[name] = defOptions[name];
            }
        }

        if (defOptions && defOptions.__initialized) {
            break;
        }
        
        baseType = baseType.get_baseType();
    }

    options.__initialized = true;

    type.prototype.defaultOptions = options;
    return options;
};

Control.__updateSortFunc = function(a, b) {
    return a._level - b._level;
};

Control.__globalUpdateQueue = new QueueProcessor(function(sender, args) {
    var controls = args.data;
    controls.sort(Control.__updateSortFunc);

    while (controls.length > 0) {
        var control = controls[0];

        if (control.isAttachedToDom()) {
            var visited = control.updateImmediately();

            for (var i in visited) {
                controls.exclude(Controls[i]);
            }

            //controls.exclude();
        } else {
            controls.exclude(control);
        }
    }
});

Control.forceUpdates = function() {
    Control.__globalUpdateQueue.force('update');
};

Control.prototype = {
    _clientWidth: null,
    _clientHeight: null,
    _position: null,
    _templateItems: null,
    _data: null,
    _inDocumentFlow: null,
    _tooltip: null,
    _level: null, // level in controls' hierarchy
    _disabledCssClass: null,
    _layoutEngine: null,
    _padding: null,
    _margin: null,
    _border: null,
    _innerDelta: null,
    _tooltipTemplate: null,
    _tooltipDelayTime: null,
    _tooltipEnabled: true,
    __tooltipObj: null,
    __tooltipTimeoutId: null,
    __tooltipPosition: null,
    
    controls: null,
    parent: null,
    id: null,
    domElement: null,
    minWidth: null,
    minHeight: null,
    maxWidth: null,
    maxHeight: null,

    get_layoutEngine: function() {
        return this._layoutEngine;
    },

    hide: function () {
        if (!this.get_visible()) {
            return;
        }

        this.set_visible(false);
        this.update();
    },

    show: function () {
        if (this.get_visible()) {
            return;
        }

        this.set_visible(true);
        this.update();
    },

    focus: function() {
        if (this.controls.length > 0) {
            this.controls.first().focus();
        } else if (this.domElement) {
            setTimeout(function() {
                try {
                    this.domElement.focus();
                } catch(e) {
                };
            }.bind(this), 0);
        }
    },

    blur: function() {
        if (this.domElement) {
            this.domElement.blur();
        }
    },

    isFocusable: function() {
        return this._focusable && (this.domElement ? DOM.isFocusable(this.domElement) : false);
    },

    findControl: function(filter) {
        var source = this.controls.clone();

        if (!isFunction(filter)) {
            throw new Error('Argument exception: filter must be a function');
        }

        while (source.length > 0) {
            var control = source.dequeue();

            if (filter(control)) {
                return control;
            }

            source.insert(0, control.controls);
        }

        return null;
    },

    // #region CSS

    addCssClass: function (value) {
        if (!value) {
            return;
        }

        var classes = value.split(' ');
        var changed = false;

        for (var i = 0; i < classes.length; i++) {
            if (!this._cssClasses.contains(classes[i])) {
                this._cssClasses.add(classes[i]);
                changed = true;
            }
        }

        if (changed) {
            this._update_cssClasses();
        }
    },

    _update_cssClasses: function () {
        if (!this.domElement)
            return;

        this.domElement.className = this._cssClasses.length > 0 ? this._cssClasses.join(' ') : '';
    },

    removeCssClass: function (value) {
        if (!value) {
            return;
        }

        var classes = value.split(' ');
        var changed = false;

        for (var i = 0; i < classes.length; i++) {
            if (this._cssClasses.contains(classes[i])) {
                this._cssClasses.remove(classes[i]);
                changed = true;
            }
        }

        if (changed) {
            this._update_cssClasses();
        }
    },

    setCssClass: function (value) {
        this._cssClasses = value ? value.split(' ') : [];
        this._update_cssClasses();
    },

    set_cssClass: function (value) {
        this._cssClasses = value ? value.split(' ') : [];
        this._update_cssClasses();
    },

    set_disabledCssClass: function (value) {
        if (isNullOrUndefined(value)) {
            return;
        }

        if (this._disabledCssClass === value)
            return;

        if (this._disabledCssClass)
            this.removeCssClass(this._disabledCssClass);

        this._disabledCssClass = value;

        if (!this._enabled) {
            this.addCssClass(this._disabledCssClass);
        }
    },

    hasCssClass: function (value) {
        return this._cssClasses.contains(value);
    },

    // #endregion

    get_childsContainer: function () {
        return this.domElement;
    },

    get_data: function () {
        if (!this._dataSync) {
            if (this.parent && !this._data) {
                var parentData = this.parent.get_data();

                if (parentData) {
                    this._data = parentData;
                }
            }
        }

        if (!this._dataSync && this._data) {
            this._dataSync = true;
            this.raise_onDataInit();
        }

        return this._data;
    },

    set_tooltip: function (value) {
        if (this._tooltip === value) {
            return;
        }

        this._tooltip = value;

        if (this.domElement) {
            this.domElement.title = value;
        }
    },

    set_tooltipEnabled: function (value) {
        this._tooltipEnabled = value;
    },

    get_tooltipEnabled: function () {
        return this._tooltipEnabled;
    }, 

    // #region Layout
    // ============================================

    isInDocumentFlow: function() {
        return this._inDocumentFlow;
    },

    isDependsOnChildSize: function() {
        return this.get_layoutEngine().isDependsOnChildSize();
    },

    isDependsOnChildWidth: function() {
        return this.get_layoutEngine().isDependsOnChildWidth();
    },

    isDependsOnChildHeight: function() {
        return this.get_layoutEngine().isDependsOnChildHeight();
    },

    get_clientWidth: function() {
        return this._visible ? this._clientWidth : 0;
    },
    
    _set_clientWidth: function(value) {
        this._clientWidth = value !== null ? Math.round(value) : null;

        if (isDefined(this.options.minWidth) && this._clientWidth < this.options.minWidth*1) {
            this._clientWidth = this.options.minWidth*1;
        }
        
        if (isDefined(this.options.maxWidth) && this._clientWidth > this.options.maxWidth*1) {
            this._clientWidth = this.options.maxWidth*1;
        }
    },

    get_clientHeight: function() {
        return this._visible ? this._clientHeight : 0;
    },
    
    _set_clientHeight: function(value) {
        this._clientHeight = value !== null ? Math.round(value) : null;

        if (isDefined(this.options.minHeight) && this._clientHeight < this.options.minHeight) {
            this._clientHeight = this.options.minHeight;
        }
        
        if (isDefined(this.options.maxHeight) && this._clientHeight > this.options.maxHeight) {
            this._clientHeight = this.options.maxHeight;
        }
    },

    get_innerWidth: function() {
        return Math.max(this._clientWidth - this._innerDelta.width, 0);
    },

    get_innerHeight: function() {
        return Math.max(this._clientHeight - this._innerDelta.height, 0);
    },

    get_outerWidth: function() {
        return this._visible ? (this._clientWidth + this.get_margin().get_width()) : 0;
    },

    get_outerHeight: function() {
        return this._visible ? (this._clientHeight + this.get_margin().get_height()) : 0;
    },

    set_width: function(value) {
        this._width = value instanceof DimensionUnit ? value : new DimensionUnit(value);
    },
    get_width: function() {
        return this._visible ? this._width : DimensionUnit.Zero;
    },

    set_height: function(value) {
        this._height = value instanceof DimensionUnit ? value : new DimensionUnit(value);
    },
    get_height: function() {
        return this._visible ? this._height : DimensionUnit.Zero;
    },

    get_domWidth: function() {
        return this.get_layoutEngine().get_domWidth();
    },

    get_domHeight: function() {
        return this.get_layoutEngine().get_domHeight();
    },

    get_padding: function() {
        return this._padding;
    },

    get_margin: function() {
        return this._margin;
    },

    get_border: function() {
        return this._border;
    },

    set_visible: function(value) {
        var oldValue = this._oldValue;

        if (this._visible == value) {
            return;
        }

        this._visible = value;
        this._updateVisible();

        this.raise_visibleChanged({ newValue: value, oldValue: oldValue });
    },

    _updateVisible: function() {
        if (!this._instantiatedInDom || !this.domElement) {
            return;
        }

        if (this._visible) {
            this.domElement.style.display = '';
        } else {
            this.domElement.style.display = 'none';
        }
    },

    update: function() {
        Control.__globalUpdateQueue.add('update', this);
    },

    postUpdate: function() {
        this.get_layoutEngine().postUpdate();
    },

    updateImmediately: function() {
        if (!this.domElement) {
            return;
        }

        return this.get_layoutEngine().update();
    },

    updateDom: function() {
        this.get_layoutEngine().updateDom();
        
        if (this.parent) {
            this.parent.get_layoutEngine().updateChildDom(this);
        }

        this.domElement.style.visibility = '';
    },

    // ============================================
    // #endregion

    initFromOptions: function (options) {
        this.options = options;

        this._applyResourceOptions(this.options);
        this._applyDefaultOptions(this.options);

        if (isFunction(options.onLoad)) {
            this.add_onLoaded(options.onLoad, this);
        }

        if (isFunction(options.onInit)) {
            this.add_initComplete(options.onInit, this);
        }

        if (isFunction(options.onFree)) {
            this.add_onFree(options.onFree, this);
        }

        if (isFunction(options.onAttached)) {
            this.add_onAttached(options.onAttached, this);
        }

        if (isFunction(options.onDataInit)) {
            this.add_onDataInit(options.onDataInit, this);
        }

        if (!isNullOrUndefined(options.width)) {
            this.set_width(new DimensionUnit(options.width));
        }

        if (!isNullOrUndefined(options.height)) {
            this.set_height(new DimensionUnit(options.height));
        }

        if (this.get_width().isNull() || this.get_height().isNull()) {
            throw new Error('Every control must have width and height');
        }

        this.addCssClass(options.cssClass || null);
        this.id = options.id || null;
        this.set_zIndex(options.zIndex || null);
        this.minWidth = options.minWidth || 0;
        this.minHeight = options.minHeight || 0;
        this.maxWidth = options.maxWidth || Infinity;
        this.maxHeight = options.maxHeight || Infinity;
        this._visible = options.visible === false ? false : true;
        this.set_enabled(isNullOrUndefined(options.enabled) ? true : options.enabled);
        this.set_disabledCssClass(options.disabledCssClass);
        this._tooltip = options.tooltip;
        this._tooltipTemplate = options.tooltipTemplate;
        this._tooltipDelayTime = options.tooltipDelayTime || 500;
        this._focusable = isDefined(options.focusable) ? options.focusable : true;

        this._layoutEngine = Nimble.Core.LayoutEngines.LayoutFactory.create(options.layout || 'stack', this);

        if (options.customFunctions)
            this._attachCustomFunctions(options.customFunctions);

        if (options.data) {
            this._data = this.initControlData(options.data);
        }

        if (options.handlers)
            this.attachHandlers(options.handlers);

        if (options.domHandlers) {
            this.attachDomHandlers(options.domHandlers);
        }

        if(this._tooltipTemplate!=null) {
            this.attachEventHandlers();
        }
        
        var controls = options.controls;
        var collection = this.controls;

        collection.eventsEnabled = false;
        
        this._initBindings('backward');

        if (controls) {
            for (var i = 0, len = controls.length; i < len; i++) {
                var controlItem = controls[i];
                // I HATE IE: in IE [ {}, {}, ].length === 3, but the third element is undefined
                if (!controlItem) {
                    continue;
                }

                if (!controlItem.hasOwnProperty("type")) {
                    throw new Error('[Control]: Undefined type for a control');
                }

                var control = ControlsFactory.create(controlItem.type);
                control.id = controlItem.id;
                control.parent = this;
                control._level = this._level + 1;
                collection.add(control);

                control.initFromOptions(controlItem);
                
                this._attachBackwardHandlers(control);
            }
            this._childsTreeLoaded();
        }

        collection.eventsEnabled = true;

        //TODO: This method is for 'template' initializations - should be removed
        this._postInit();
    },

    __updateTooltipPosition: function(sender, args) {
        if(this.__tooltipPosition == null) {
            this.__tooltipPosition = {x: args.clientX, y: args.clientY};
        } else {
            this.__tooltipPosition.x = args.clientX;
            this.__tooltipPosition.y = args.clientY;
        }
    },

    __clearTooltipTimeoutId: function() {
        if(this.__tooltipTimeoutId != null) {
            clearTimeout(this.__tooltipTimeoutId);
            this.__tooltipTimeoutId = null;
        }
    },

    _createTooltipWithDelay: function(sender, args) {
        this.__clearTooltipTimeoutId();
        this.__tooltipPosition = {x: args.clientX, y: args.clientY};
        this.__tooltipTimeoutId = setTimeout(function() {
                this._createTooltip(sender, args);
            }.bind(this),this._tooltipDelayTime);
    },
    
    _createTooltip: function(sender, args) {
        if(this._tooltipTemplate != null && this.__tooltipObj == null && this._tooltipEnabled) {
            this.__tooltipObj = new Phoenix.UI.Tooltip();
            this.__tooltipObj.initFromOptions({ template: this._tooltipTemplate });
            if(this._tooltipTemplate != null) {
                this.__tooltipObj.controls[0].set_dataSource(this.get_dataSource());
            }
            this.__tooltipObj.set_position({x: this.__tooltipPosition.x + 16, y: this.__tooltipPosition.y + 16});
            this.controls.add(this.__tooltipObj);
            this.__tooltipObj.open();  
        }
    },

    _destroyTooltip: function() {
        this.__clearTooltipTimeoutId();
        this.__tooltipPosition = null;
        
        if (this.__tooltipObj != null) {
            this.controls.remove(this.__tooltipObj);
            this.__tooltipObj.free();
            this.__tooltipObj = undefined;
        }
    },
    
    _childsTreeLoaded: function() {
    },

    _postInit: function() {
        this._initDataSourceMethods();
        this._initBindings('forward');
        this.attachDataHandlers();
        this.attachHistoryHandlers();
        this.raise_initComplete();
    },

    _applyDefaultOptions: function (options) {
        var defaultOptions = this.defaultOptions;

        if (!defaultOptions) {
            return;
        }

        for (var name in defaultOptions) {
            if (options[name] !== undefined)
                continue;

            options[name] = defaultOptions[name];
        }
    },

    _applyResourceOptions: function(options) {
        if (!options.resource) {
            return;
        }

        var resource = Application.get_resource(options.resource);

        if (!resource) {
            console.log('Unknown resource: ' + options.resource);
            return;
        }

        for (var name in resource) {
            if (options[name] !== undefined)
                continue;

            options[name] = resource[name];
        }
    },

    _attachCustomFunctions: function (functionsObject) {
        for (var funcName in functionsObject) {
            if (!isFunction(functionsObject[funcName]))
                throw new Error('Member "' + funcName +'" expected to be a function');

            if (this[funcName])
                throw new Error('Function "' + funcName +'" already exists');

            this[funcName] = functionsObject[funcName].bind(this);
        }
    },

    initControlData: function (dataOptions) {
        if (!dataOptions) {
            return;
        }

        var data = {};

        for (var i = 0; i < dataOptions.length; i++) {
            var item = dataOptions[i];

            if (item.name) {
                Auto.Property(data, { name: item.name, autoEvent: true });
                data['set_' + item.name](item.value);

                if (item.saveHistory) {
                    History.Observer.init(item.name, item.type, data, this.$get_page());
                }                

            } else {
                Auto.Property(data, { name: item, autoEvent: true });
            }
        }

        this.add_onFree(function () {
            if (data && data.dispose) {
                data.dispose();
            }
        }, this);

        return data;
    },

    attachDataHandlers: function() {
        if (!this.options)
            return;

        var window = this.get_window();
        var data = window ? window.get_data() : null;

        if(data)
            this._attachForwardHandlers(data, true, false);
    },

    detachDataHandlers: function() {
        var window = this.get_window();

        if (window) {
            var data = window.get_data();
            if(data)
                this._detachForwardHandlers(data, true);
        }
    },

    attachHistoryHandlers: function() {
        if (!this.options)
            return;

        var window = (this instanceof Page) ? this : this.$get_page();

        if (window)
            this._attachForwardHandlers(window, false, true);
    },

    detachHistoryHandlers: function() {
        var window = this.get_window();

        if (window) {
            var data = window.get_data();
            if(data)
                this._detachForwardHandlers(data, true);
        }
    },

    set_dataSource: function (value) {
        if (value === this._dataSource)
            return;

        if (this._dataSource)
            this._detachForwardHandlers(this._dataSource);
        
        var oldValue = this._dataSource;
        this._dataSource = value;
        
        if (this._dataSource)
            this._attachForwardHandlers(value, false, false);

        this.raise_dataSourceChanged({newValue: value, oldValue: oldValue});
    },

    set_enabled: function (value) {
        var oldValue = this._enabled;

        if (this._enabled === value)
            return false;

        this._enabled = value;

        if (this._disabledCssClass) {
            if (!value) {
                this.addCssClass(this._disabledCssClass);
            } else {
                this.removeCssClass(this._disabledCssClass);
            }
        }

        this.raise_enabledChanged({ newValue: value, oldValue: oldValue });
    },

    attachHandlers: function (handlers) {
        for (var name in handlers) {
            if (handlers.hasOwnProperty(name)) {
                var attachMethod = this['add_' + name];

                if (!attachMethod || !isFunction(attachMethod))
                    throw new Error('"' + name + '" event not found in control');

                if (!isFunction(handlers[name])) {
                    throw new Error('"' + name + '" handler must be a function');
                }

                attachMethod.bind(this)(handlers[name], this);
            }
        }
    },

    // #region Handling events 
    attachEventHandlers: function() {
        this.attachDomHandler('mousemove', this.__updateTooltipPosition);
        this.attachDomHandler('mouseover', this._createTooltipWithDelay);
        this.attachDomHandler('mouseout', this._destroyTooltip);
    },

    detachEventHandlers: function() {
        this.detachDomHandler('mousemove', this.__updateTooltipPosition);
        this.detachDomHandler('mouseover', this._createTooltipWithDelay);
        this.detachDomHandler('mouseout', this._destroyTooltip);
    },
    // #endregion

    // #region DOM Handlers
    
    attachDomHandlers: function (eventHandlersCollection) {
        if (!eventHandlersCollection)
            return;

        for (var eventName in eventHandlersCollection) {
            this.attachDomHandler(eventName, eventHandlersCollection[eventName]);
        }
    },

    attachDomHandler: function (eventName, handler) {
        if (!handler || !isFunction(handler))
            throw new Error('Dom event handler must be specified.');

        if (!this['add_' + eventName]) {
            Auto.Event(this, eventName);

            if (!this.__domEvents)
                this.__domEvents = [];

            if (!this.__domEvents.contains(eventName)) {
                this.__domEvents.add(eventName);

                if (this.domElement)
                    jQuery(this.domElement).bind(eventName,
                        function (args) {
                            this['raise_' + eventName](args);
                        } .bind(this)
                    );
            }
        }

        this['add_' + eventName](handler, this);
    },

    detachDomHandler: function (eventName, handler) {
        this['remove_' + eventName](handler);
    },

    initializeDomEvents: function () {
        if (!this.__domEvents)
            return;

        var that = this;

        for (var i = 0; i < this.__domEvents.length; i++) {
            jQuery(this.domElement).bind(this.__domEvents[i],
                function (eName) {
                    return function (args) {
                        this['raise_' + eName](args);
                    } .bind(that);
                } (this.__domEvents[i])
            );
        }
    },

    // #endregion

    // #region Bindings

    __parseSourceNames: function(sourceName) {
        var names = sourceName.split(','),
            result = [];

        for (var i = 0; i < names.length; i++) {
            result.add(names[i].trim());
        }

        return result;
    },

    _initBindings: function(bindingType) {
        if(!this._bindingHandlers)
            this._bindingHandlers = {
                forward: [],
                backward: []
            };

        if (!this.options.bindings)
            return;

        var funcName;
        
        if(bindingType === 'forward')
            funcName = '_initForwardBindingHandlers';
        else if(bindingType === 'backward')
            funcName = '_initBackwardBindingHandlers';
        else
            throw new Error('unknown binding initialization type');
        
        for (var sourceName in this.options.bindings) {
            var names = this.__parseSourceNames(sourceName);
            
            for (var i = 0; i < names.length; i++) {
                this[funcName](names[i], this.options.bindings[sourceName]);
            }
        }
    },

    _initForwardBindingHandlers: function(bindingSource, binding) {
        var handlers = [],
            isGlobal = false,
            isHistory = false,
            source = bindingSource,
            globalParts = bindingSource.split(':');

        // check if current binding is global
        if (globalParts.length == 2 && globalParts[0] == 'data') {
            isGlobal = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('Global data binding on * is not allowed');
            }
        }

        // check if current binding is history
        if (globalParts.length == 2 && globalParts[0] == 'history') {
            isHistory = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('History data binding on * is not allowed');
            }
        }

        // if target binding is an array then attach binding for each items of it
        if (binding instanceof Array) {
            for (var i = 0; i < binding.length; i++) {
                if (isDefined(binding[i])) {
                    handlers.add(this._getForwardHandlerFor(binding[i]));
                }
            }
        } else if (isDefined(binding)) {
            handlers.add(this._getForwardHandlerFor(binding));
        }

        if (!isHistory && this._tryInitComplexBinding(bindingSource, handlers, isGlobal)) {
            return;
        }

        var length = this._bindingHandlers.forward.length;
        this._bindingHandlers.forward[length] = source;
        this._bindingHandlers.forward[length + 1] = handlers;

        if (bindingSource === '*') {
            for (var i = 0; i < handlers.length; i++) {
                this.add_dataSourceChanged(handlers[i], this);
            }
        }
    },

    _tryInitComplexBinding: function(binding, handlers, isGlobal) {
        var parts = binding.split('.');

        if (parts.length < 2) {
            return false;
        }

        var thisObj = this;

        var source = parts[0];
        var event = parts[1];

        if(isGlobal && source === '*')
            throw new Error('Global data binding on * is not allowed');

        var handler = function(sender, args) {
            if (args.oldValue) {
                for(var i = 0; i < handlers.length; i++)
                    args.oldValue['remove_' + event](handlers[i], thisObj);
            }            
            
            if (args.newValue) {
                for(var i = 0; i < handlers.length; i++) {
                    args.newValue['add_' + event](handlers[i], thisObj);
                }
            }
        };
        handler._isComplex = true;
        
        if(source === '*')
            this.add_dataSourceChanged(handler, this);
        else if(source !== '*') {
            var length = this._bindingHandlers.forward.length;
            this._bindingHandlers.forward[length] = !isGlobal ? source : 'data:' + source;
            this._bindingHandlers.forward[length + 1] = [handler];
        }
        
        return true;
    },

    _initBackwardBindingHandlers: function(bindingSource, binding) {
        if (bindingSource.split('.').length > 1) {
            return;
        }

        var isGlobal = false,
            isHistory = false;

        var globalParts = bindingSource.split(':');
        if (globalParts.length == 2 && globalParts[0] == 'data') {
            isGlobal = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('Global data binding on * is not allowed');
            }
        }

        if (globalParts.length == 2 && globalParts[0] == 'history') {
            isHistory = true;
            bindingSource = globalParts[1];

            if (bindingSource === '*') {
                throw new Error('History data binding on * is not allowed');
            }
        }

        var trackingProps = [];
        var backwardHandler = this._getBackwardHandlerFor(bindingSource, isGlobal, isHistory);

        if (binding instanceof Array) {
            for (var i = 0; i < binding.length; i++) {
                if (typeof binding[i] === 'string') {
                    trackingProps.add(binding[i]);
                } 
            }        
        } else if (typeof binding === 'string') {
            trackingProps.add(binding);
        }

        for (var i = 0; i < trackingProps.length; i++) {
            if (!this._checkAndAttachHandlerToSelf(trackingProps[i], backwardHandler)) {
                var length = this._bindingHandlers.backward.length;
                
                this._bindingHandlers.backward[length] = trackingProps[i];
                this._bindingHandlers.backward[length + 1] = backwardHandler;
            }
        }
    },

    _checkAndAttachHandlerToSelf: function(prop, handler) {
        if(this['add_' + prop + 'Changed'] && isFunction(this['add_' + prop + 'Changed'])) {
            this['add_' + prop + 'Changed'](handler, this);
            return true;
        }

        if(this[prop] && isFunction(this[prop]))
            return true;

        return false;
    },

    _getForwardHandlerFor: function(bindingTarget) {
        var thisObj = this;

        if(isFunction(bindingTarget))
            return bindingTarget;
        
        if(typeof bindingTarget !== 'string')
            throw Error('Unknow binding type: ' + bindingTarget);

        if(bindingTarget === '*')
            return function Binding$forwardHandler$toAllChilds(sender, args) {
                var controls = thisObj.controls;

                for (var i = 0; i < controls.length; i++) {
                    var control = controls[i];
                    control.set_dataSource(args.newValue);
                }
            };

        if(this['set_' + bindingTarget] && isFunction(this['set_' + bindingTarget]))
            return function Binding$forwardHandler$property(sender, args) { thisObj['set_' + bindingTarget](args.newValue); };

        var target = this[bindingTarget];

        if(isFunction(target))
            return target;
        else if (Control.isInstance(target))
            return function Binding$forwardHandler$child$toDataSource (sender, args) { thisObj[bindingTarget].set_dataSource(args.newValue); };
        else if (target !== undefined)
            return function Binding$forwardHandler$child$toField (sender, args) { thisObj[bindingTarget] = args.newValue; };
        else
            return function Binding$forwardHandler$lazy$child$toDataSource (sender, args) {
                var target = thisObj[bindingTarget];

                if (Control.isInstance(target)) {
                    target.set_dataSource(args.newValue);
                } else if (isUndefined(target)) {
                    throw new Error('Undefined property ' + bindingTarget);
                }
            };
    },

    _getBackwardHandlerFor: function(prop, isGlobal, isHistory) {
        if(typeof prop !== 'string')
            throw Error('Unknown property binding type');
        
        if(prop === '*')
            return function Binding$backwardHandler$this$dataSource(sender, args) {
                this.set_dataSource(args.newValue);
            };

        if (isGlobal) {
            return function Binding$backwardHandler$data$property(sender, args) {
                var ds;

                if (this instanceof Page) {
                    ds = this.get_data();
                } else {
                    ds = this.get_window().get_data();
                }

                if(!ds)
                    return;

                ds['set_' + prop](args.newValue);
            };
        }
        
        if (isHistory) {
            return function Binding$backwardHandler$history$property(sender, args) {
                var page;
                
                if (this instanceof Page) {
                    page = this;
                } else {
                    page = this.$get_page();
                }

                page.set_param(prop, args.newValue);
            };
        }

        return function Binding$backwardHandler$dataSourcec$propertyOrField(sender, args) {
            var ds = this.get_dataSource();

            if(!ds)
                return;

            if(ds['set_' + prop] && isFunction(ds['set_' + prop]))
                ds['set_' + prop](args.newValue);
            else
                ds[prop] = args.newValue;
        };
    },

    _attachForwardHandlers: function(object, globalMode, historyMode) {
        for (var i = 0; i < this._bindingHandlers.forward.length; i+=2) {
            var bindingSource = this._bindingHandlers.forward[i];
            
            if (bindingSource === '*') {
                continue;
            }

            if (globalMode) {
                var splitted = bindingSource.split(':');

                if (splitted.length != 2 || splitted[0] != 'data')
                    continue;

                bindingSource = splitted[1];
            }

            if (historyMode) {
                var splitted = bindingSource.split(':');

                if (splitted.length != 2 || splitted[0] != 'history')
                    continue;

                bindingSource = splitted[1];
            }

            if (object) {
                if (historyMode) {
                    var $event = function (callback, thisObj) {
                        var onParamChanged = (function(bSource) {
                            return function(sender, args) {
                                if (args.key === bSource) {
                                    callback.bind(thisObj)(sender, args);
                                }
                            };
                        })(bindingSource);

                        object.add_onParamChanged(onParamChanged);
                    };
                    
                    value = object.get_param(bindingSource);
                } else {
                    var $event = object['add_' + bindingSource + 'Changed'],
                        getter = object['get_' + bindingSource],
                        value = getter ? getter.call(object) : object[bindingSource];
                }
                
                var defined = !isUndefined(value),
                    handlers =  this._bindingHandlers.forward[i+1];

                for (var k = 0; k < handlers.length; k++) {
                    if ($event) {
                        $event.call(object, handlers[k], this);
                    }

                    if (defined) {
                        handlers[k].bind(this)(object, { newValue: value, oldValue: undefined });
                    }
                }
            } else {
                var handlers =  this._bindingHandlers.forward[i+1];

                for(var k = 0; k < handlers.length; k++) {
                    handlers[k].bind(this)(null, {newValue: null, oldValue: undefined});
                }
            }
        }
    },

    _attachBackwardHandlers: function(control) {        
        if (!control.id)
            return;        

        for (var i = 0; i < this._bindingHandlers.backward.length; i+=2) {
            if(this._bindingHandlers.backward[i] !== control.id)
                continue;

            control.add_dataSourceChanged(this._bindingHandlers.backward[i+1], this);
        }
    },

    _detachForwardHandlers: function(object, globalMode) {
        for(var i = 0; i < this._bindingHandlers.forward.length; i+=2) {
            var bindingSource = this._bindingHandlers.forward[i];
            
            if(bindingSource === '*')
                continue;

            if (globalMode) {
                var splitted = bindingSource.split(':');
                if(splitted.length <= 1)
                    continue;

                bindingSource = splitted[1];
            }

            var $event = object['remove_' + bindingSource + 'Changed'];
            
            if(!$event || !isFunction($event))
                continue;
            
            var handlers =  this._bindingHandlers.forward[i+1];

            for(var k=0; k < handlers.length; k++) {
                if(handlers[k]._isComplex)
                    handlers[k](this, {newValue: undefined, oldValue: object['get_' + bindingSource]()});

                object['remove_' + bindingSource + 'Changed'](handlers[k], this);
            }
        }
    },

    _detachBackwardHandlers: function(control) {
        if(!control.id)
            return;        

        for(var i = 0; i < this._bindingHandlers.backward.length; i+=2) {
            if(this._bindingHandlers.backward[i] !== control.id)
                continue;

            control.remove_dataSourceChanged(this._bindingHandlers.backward[i+1], this);
        }
    },

    _initDataSourceMethods: function () {
        if (this.options.setter) {
            var setter = this.options.setter;

            this.set_dataSource = function (value) {
                if (this._dataSource === value)
                    return;

                var oldVal = this._dataSource;

                setter.bind(this)(value);

                if (oldVal === this._dataSource)
                    this._dataSource = value;
            };
        }

        if (this.options.getter)
            this.get_dataSource = this.options.getter;
    },

    // #endregion

    // #region DOM

    isAttachedToDom: function() {
        return this.domElement && this._instantiatedInDom && !this._detachedFromDom;
    },

    // attach control in DOM to @domElement
    attachToDom: function(domElement) {
        if (!this._instantiatedInDom) {
            this.instantiateInDom(domElement);
        } else if (this._detachedFromDom) {
            DOM.appendChild(this.domElement, domElement);
            this._detachedFromDom = false;
        }

        this.raise_onAttached();
    },

    _calculateInnerDelta: function() {
        this._innerDelta.width = this._padding.get_width() + this._border.get_width();
        this._innerDelta.height = this._padding.get_height() + this._border.get_height();
    },

    // instantiate control into @domElement
    instantiateInDom: function (domElement) {
        this.set_instantiatedInDom(true);
        var element = this.domElement;
        var style = element.style;
        style.visibility = 'hidden';

        if (element) {
            if (this._cssClasses.join('')) {
                this._update_cssClasses();
            }

            if (this._tooltip) {
                element.title = this._tooltip;
            }

            this._padding = new Rect(this.options.padding);
            this._margin = new Rect(this.options.margin);
            this._border = new Rect(this.options.border);
            
            //style.cssText += 'padding: ' + this._padding.toString() + ' !important; margin: ' + this._margin.toString() + ' !important; border-width: ' + this._border.toString() + ' !important;';
            style.padding = this._padding.toString();
            style.margin = this._margin.toString();
            style.borderWidth = this._border.toString();
            
            this._calculateInnerDelta();
        }

        if (this.controls.length > 0) {
            this.instantiateChildsInDom();
        }

        this.initializeDomEvents();

        if (!this._visible) {
            this._updateVisible();
        }

        this.onLoad();
    },

    onLoad: function() {
        this.raise_onLoaded();
    },

    /**
    * Instantiate child elements in some domElement
    * @param {DOMElement} domElement
    */
    instantiateChildsInDom: function () {
        var controls = this.controls;

        for (var i = 0, len = controls.length; i < len; i++) {
            this.instantiateChildInDom(controls[i]);
        }
    },

    /**
    * Instantiate a child element in DOM
    * @param {Control} control
    * @param {String} type of instantiating (child, after, before)
    * @param {Control} reference control (if type is after or before)
    * @param {DomElement} dom container for the child
    */
    instantiateChildInDom: function (control, type, refControl, container) {
        var domElement = container || this.get_childsContainer();

        control.attachToDom(domElement);

        if ((type == 'after' || type == 'before') && refControl) {
            var refDom = refControl.domElement;
            var cDom = control.domElement;

            if (refDom && cDom && ((type == 'after' && cDom.previousSibling !== refDom) || (type == 'before' && cDom.nextSibling !== refDom)) && refDom.parentNode === cDom.parentNode) {
                DOM.insert(type, refDom, cDom);
            }
        }
    },

    /**
    * Remove a child element from DOM
    */
    removeChildFromDom: function (control) {
        if (control._instantiatedInDom) {
            control.detachFromDom();
            //control.removeFromDom();
        }
    },

    removeFromDom: function () {
        // remove dom element
        var element = this.domElement;

        if (element) {
            if (!this.parent || !this.parent.__free) {
                DOM.remove(element);
            }

            this.domElement = null;
        }

        this.set_instantiatedInDom(false);
    },

    detachFromDom: function() {
        this._clientWidth = null;
        this._clientHeight = null;

        if (this._instantiatedInDom) {
            var element = this.domElement;
            DOM.detachNode(this.domElement);
            this._detachedFromDom = true;
        }

        //this.detachDataHandlers();
    },

    // #endregion


    // disposes the control
    free: function () {
        if (this.__free) {
            return;
        }

        // destroy tooltip
        this._destroyTooltip();

        if (this._dataSource)
            this._detachForwardHandlers(this._dataSource);
        
        this.__free = true;
        this.raise_onFree();

        if(this._tooltipTemplate != null) {
            this.detachEventHandlers();
        }

        this.detachDataHandlers();
        this.detachHistoryHandlers();

        // free childs
        var childs = this.controls;
        childs.eventsEnabled = false;

        if (childs) {
            for (var i = childs.length - 1; i >= 0; i--) {
                var control = childs[i];
                childs[i].free();
            }

            childs.clear();
        }

        this.removeFromDom();
        this._data = null;
        this._dataSource = null;
        this._templateItems = null;
        this.options = null;

        this.clearEvents();

        Controls.count--;
        delete Controls[this.__cid];
    },

    get_window: function () {
        var parent = this.parent || this;
        
        var parentType = Type.getInstanceType(parent);

        if (parentType === Type.getType('Page') || parentType === Type.getType('Phoenix.UI.Popup'))
            return parent;

        return parent === this ? null : parent.get_window();
    },

    $get_page: function () {
        var parent = this.parent || this;
        
        var parentType = Type.getInstanceType(parent);

        if (parentType === Type.getType('Page'))
            return parent;

        return parent === this ? null : parent.$get_page();
    },

    // #region Child collections
    // ==================================

    _childAdded: function (sender, args) {
        var control = args.control;

        if (control.parent && control.parent !== this) {
            control.parent.controls.remove(control);
        }

        control.parent = this;
        control._level = this._level + 1;

        if (this._instantiatedInDom) {
            var container = document.createDocumentFragment();

            if (!args.nextControl) {
                this.instantiateChildInDom(control, null, null, container);
            } else {
                if (args.prevControl) {
                    this.instantiateChildInDom(control, 'after', args.prevControl, container);
                } else {
                    this.instantiateChildInDom(control, 'before', args.nextControl, container);
                }
            }

            this.get_childsContainer().appendChild(container);
        }

        this._attachBackwardHandlers(control);
    },

    _childRemoved: function (sender, args) {
        var control = args.control;

        this.removeChildFromDom(control);
        control.parent = null;
        
        this._detachBackwardHandlers(control);
     },
     
    _childsCollectionModified: function () {
        this.update();
    },
    
    appendControl: function (control) {
        this.controls.add(control);
    },

    createControlsCollection: function () {
        this.controls = [].makeControlsCollection(this);
        this.controls.add_added(this._childAdded, this);
        this.controls.add_removed(this._childRemoved, this);
        this.controls.add_modified(this._childsCollectionModified, this);
    },

    // ==================================
    // #endregion

    toString: function () {
        return Type.getInstanceType(this).get_name() + " #" + this.__cid +
            (this.id ? ", Id: " + this.id : "") +
            (this.controls ? ", Childs count: " + this.controls.length : "");
    },

    attachEventsFromOptions: function(options, eventsList) {
        if (!isArray(eventsList)) {
            throw new Error('Array exprected');
        }

        for (var i = 0; i < eventsList.length; i++) {
            var eventName = eventsList[i];

            if (!this['add_' + eventName]) {
                throw new Error('Event with name "' + eventName + '" have not been found');
            }

            if (options[eventName] && isFunction(options[eventName])) {
                this['add_' + eventName](options[eventName], this);
            }
        }
    }
};

Auto.Events(Control.prototype, [
    'initComplete',
    'onLoaded',
    'onAttached',
    'onFree',
    'onDataInit'
]);

Auto.Properties(Control.prototype, [
    { name: 'instantiatedInDom', defaultValue: false },
    { name: 'left' }, // dimension unit
    { name: 'top' }, // dimension unit
    { name: 'cssClass' },
    { name: 'dataSource', autoEvent: true },
    { name: 'zIndex' },
    { name: 'enabled', autoEvent: true },
    { name: 'visible', autoEvent: true },
    { name: 'tooltip' },
    { name: 'tooltipTemplate' },
    { name: 'tooltipDelayTime'},
    { name: 'disabledCssClass' }
]);

Control.createClass('Control');
var ControlsCollection = {
    _owner: null,
    _controlsInFlow: null,
    _itemOptions: null,
    eventsEnabled: null,

    // ======== Properties ========
    _addItem: function (item, options) {
        var collection = this;

        for (var i = collection.length - 1; i >= 0; i--) {
            if (collection[i] === item) {
                this._unregisterControl(item);
                collection.removeAt(i);
                this._itemOptions.removeAt(i);
                break;
            }
        }

        this._registerControl(item);
        var idx = collection.length;

        // find a new place if previous control is passed
        if (options) {
            if (!isUndefined(options.prevControl)) {
                if (options.prevControl === null) {
                    idx = 0;
                } else {
                    for (var i = 0; i < collection.length; i++) {
                        if (collection[i] === options.prevControl) {
                            idx = i + 1;
                            break;
                        }
                    }
                }
            }
        }

        while (idx > 0 && this._itemOptions[idx - 1] && this._itemOptions[idx - 1].alwaysLast) {
            idx--;
        }

        while (idx < collection.length && this._itemOptions[idx] && this._itemOptions[idx].alwaysFirst) {
            idx++;
        }

        collection.insert(idx, item);
        this._itemOptions.insert(idx, options);

        if (this.eventsEnabled) {
            this.raise_added({
                control: item,
                prevControl: idx > 0 ? collection[idx - 1] : null,
                nextControl: idx < collection.length - 1 ? collection[idx + 1] : null
            });
        }
        
        this._controlsInFlow = null;
    },

    _registerControl: function (control) {
        var owner = this._owner;

        if (control.id) {
            if (owner[control.id]) {
                throw new Error('[ControlsCollection]: Conflict with property "' + control.id + '" was occured or control with that id had registered already');
            }

            owner[control.id] = control;
        }
    },

    _unregisterControl: function (control) {
        var owner = this._owner;

        if (control.id) {
            delete owner[control.id];
        }
    },

    // ======== Methods ========
    add: function (control, options) {
        this._addItem(control, options);

        if (this.eventsEnabled) {
            this.raise_modified();
        }
    },

    addRange: function (controls, optionsArr) {
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];
            var options = optionsArr ? optionsArr[i] : null;

            this._addItem(control, options);
        }

        if (this.eventsEnabled) {
            this.raise_modified();
        }
    },

    remove: function (control) {
        this._unregisterControl(control);

        var idx = this.indexOf(control);

        if (idx >= 0) {
            this.splice(idx, 1);
            this._itemOptions.removeAt(idx);
        } else {
            throw new Error('Control not found in child collection');
        }

        if (this.eventsEnabled) {
            this.raise_removed({ control: control });
            this.raise_modified();
        }
        
        this._controlsInFlow = null;
    },

    removeRange: function (controls) {
        for (var i = 0; i < controls.length; i++) {
            var control = controls[i];

            this._unregisterControl(control);

            var idx = this.indexOf(control);

            if (idx >= 0) {
                this.splice(idx, 1);
                this._itemOptions.removeAt(idx);
            } else {
                throw new Error('Control not found in child collection');
            }

            this.raise_removed({ control: control });
        }

        if (this.eventsEnabled) {
            this.raise_modified();
        }
        
        this._controlsInFlow = null;
    },

    clear: function () {
        for (var i = 0; i < this.length; i++) {
            var control = this[i];
            this.raise_removed({ control: control });
            this._unregisterControl(control);
        }

        this._controlsInFlow = null;

        if (this.eventsEnabled) {
            this.raise_modified();
        }
    },

    contains: function (control) {
        return this.indexOf(control) >= 0;
    },

    getControlsInFlow: function () {
        if (!this._controlsInFlow) {
            var result = [];

            for (var i = 0; i < this.length; i++) {
                if (this[i].isInDocumentFlow()) {
                    result.add(this[i]);
                }
            }

            this._controlsInFlow = result;
        }

        return this._controlsInFlow;
    },

    first: function () {
        return this[0] || null;
    },

    last: function () {
        return this[this.length - 1] || null;
    }
};

Auto.Events(ControlsCollection, [
    'added',
    'removed',
    'modified'
]);

Array.prototype.makeControlsCollection = function(owner) {
    if (this.__controlsCollection) {
       return this; 
    }
    
    Object.extend(this, ControlsCollection);
    this.__controlsCollection = true;
    this._itemOptions = [];
    this._owner = owner;
    this.eventsEnabled = true;
    
    return this;
};
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
Template = function(options) {
    if(!options)
        throw new Error('Template options must be specified');
        
    this.options = options;
};

Template.prototype = {
    _domInstantiation: null,

    instantiate: function(dataItem, parent) {
        var controlType = this.options.type || 'container';
        var container = ControlsFactory.create(controlType);
        
        container.parent = parent;
        var options = Object.copy(this.options, true);
        container.initFromOptions(options);

        if (dataItem) {
            if (!parent) {
                console.warn('Ensure you passed the parent when you was passing dataItem to template');
            }
            container.set_dataSource(dataItem);
        }

        return container;
    }
};

Template.createClass('Template');
var BaseListDataControl = {
    _get_freeControls: function() {
        if (!this._freeControls) {
            this._freeControls = [];
        }
        
        return this._freeControls;
    },

    get_childsHash: function() {
        if (!this._childsHash) {
            this._childsHash = new Hashtable();
        }
        
        return this._childsHash;
    },

    set_dataSource: function(value) {
        if (this._dataSource === value) {
            return;
        }
        
        if (this._dataSource) {
            this._detachForwardHandlers(this._dataSource);
            this.clear();
        }
        
        var oldValue = this._dataSource;
        this._dataSource = value;
        
        if (this._dataSource) {
            this._attachForwardHandlers(value);

            this._attachHandlers();
        
            var newControls = [];
        
            for (var i = 0; i < value.length; i++) {
                newControls.add(this._createItem(value[i]));
            }
        
            this.controls.addRange(newControls);
        }

        this._checkEmptiness();

        this.raise_dataSourceChanged({newValue: value, oldValue: oldValue});
    },

    isEmpty: function() {
        if (!this._dataSource || this._dataSource.length == 0) {
            return true;
        }

        if (this._get_freeControls().length == this.controls.length) {
            return true;
        }

        return false;
    },
    
    /**
    * Returns child control which datasource equals to @dataItem
    */
    findByDataItem: function(dataItem) {
        return this.get_childsHash().get(dataItem);
    },
    
    _checkEmptiness: function() {
        if (this._emptyDataTemplate) {
            if (!(this._emptyDataTemplate instanceof Template)) {
                throw new Error('Empty data template must be an instance of Template');
            }
        
            if (!this._dataSource || this._dataSource.length == 0) {
                if (!this.__emptyDataControl) {
                    var control = this._emptyDataTemplate.instantiate();
                    control.container = this;
                    this.__emptyDataControl = control;
                    
                    this.controls.add(control);
                }
            } else if (this.__emptyDataControl) {
                this.__emptyDataControl.free();
                this.controls.remove(this.__emptyDataControl);
                this.__emptyDataControl = null;
            }
        }
    },
    
    _attachHandlers: function() {
        this._dataSource.add_added(this._dataSource_itemAdded, this);
        this._dataSource.add_removed(this._dataSource_itemRemoved, this);
    },

    _detachHandlers: function() {
        this._dataSource.remove_added(this._dataSource_itemAdded, this);
        this._dataSource.remove_removed(this._dataSource_itemRemoved, this);
    },
    
    _get_anchorElement: function() {
        return null;
    },
    
    _dataSource_itemAdded: function(sender, args) {
        this._checkEmptiness();

        var controls = [];
        var options = [];
        
        for (var i = 0; i < args.items.length; i++) {
            var prevDataItem = args.after[i];
            var prevControl = prevDataItem ? this.get_childsHash().get(prevDataItem) : this._get_anchorElement();
            
            controls.add(this._createItem(args.items[i]));
            options.add({ prevControl: prevControl });
        }
        
        this.controls.addRange(controls, options);
    },
    
    _dataSource_itemRemoved: function(sender, args) {
        this._removeItems(args.items);
        this._checkEmptiness();
    },
    
    _createItem: function(dataItem) {
        var freeControls = this._get_freeControls();
        var control;
        
        if (freeControls.length > 0) {
            control = freeControls.pop();
            //this.controls.add(control);
            control.set_dataSource(dataItem);
        } else {
            control = this.createItem(dataItem);
        }
    
        this.__onItemCreated(control, dataItem);
        this.get_childsHash().put(dataItem, control);

        return control;
    },
    
    _removeItem: function(dataItem) {
        var control = this.get_childsHash().get(dataItem);
        this.get_childsHash().remove(dataItem);
        
        this.controls.remove(control);

        if (true || this.options.cacheDisabled) {
            control.free();
        } else {
            control.set_dataSource(null);
            var freeControls = this._get_freeControls();
            freeControls.push(control);
        }
    },

    __onItemCreated: function(control, dataItem) {
        if (isFunction(this.options.onItemCreated)) {
            this.options.onItemCreated.bind(this)(this, { control: control, dataItem: dataItem });
        }
    },
    
    _removeItems: function(dataItems) {
        var freeControls = this._get_freeControls();
        var controls = [];

        for (var i = 0; i < dataItems.length; i++) {
            var dataItem = dataItems[i];
            
            var control = this.get_childsHash().get(dataItem);
            controls.add(control);
            this.get_childsHash().remove(dataItem);
        
            if (false && !this.options.cacheDisabled) {
                freeControls.push(control);
            }
        }

        this.controls.removeRange(controls);
        
        if (true || this.options.cacheDisabled) {
            for (var i = 0; i < controls.length; i++) {
                controls[i].free();
            }
        }
    },
    
    free: function() {
        if (this.__free) {
            return;
        }

        if (this.raise_onFree) {
            this.raise_onFree();
        }
    
        if (this._dataSource) {
            this._detachHandlers();

            this._detachForwardHandlers(this._dataSource);

            this._dataSource = null;
        }
        
        var keys = this.get_childsHash().keys();
        this._removeItems(keys);
        
        this.__emptyDataControl = null;
        
        var freeControls = this._get_freeControls();
        
        var controls = [];
        
        for (var i = 0; i < freeControls.length; i++) {
            if (!freeControls[i].__free) {
                controls.add(freeControls[i]);
            }
        }
               
        for (var i = 0; i < controls.length; i++) {
            controls[i].free();
        }
        
        this._freeControls.clear();
        
        //Type.getInstanceType(this).callBase(this, 'free');
    },
    
    clear: function() {
        if (this._dataSource) {
            this._detachHandlers();
            this._dataSource = null;
        }
        
        var keys = this.get_childsHash().keys();
        this._removeItems(keys);
        
        this._checkEmptiness();
    }
};
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Panel = function() {
    Phoenix.UI.Panel.constructBase(this);
};

Phoenix.UI.Panel.__layouts = {};

Phoenix.UI.Panel.GetLayoutEngine = function(layout) {
    var layoutEngine = Phoenix.UI.Panel.__layouts[layout];
    
    if (!layoutEngine) {
        throw new Error('[Panel]: Layout engine "' + layout + '" have not been found');
    }
    
    return new layoutEngine();
}

Phoenix.UI.Panel.RegisterLayoutEngine = function(layout, layoutEngine) {
    if (Phoenix.UI.Panel.__layouts[layout]) {
        throw new Error('[Panel]: Layout engine "' + layout + '" already registered');        
    }
    
    Phoenix.UI.Panel.__layouts[layout] = layoutEngine;
}

Phoenix.UI.Panel.prototype = {
    defaultOptions: {
        width: '*',
        height: '*'
    },
    
    /**
     * Instantiate the control into some domElement
     * @param {DOMELement} domElement
     */
    instantiateInDom: function(domElement) {
        var element = DOM.create("div", domElement);
        
        if (this._zIndex) {
            element.style.zIndex = this._zIndex;
        }

        this.domElement = element;
        
        /*if (this._layoutEngine && this._layoutEngine.initInDom) {
            this._layoutEngine.initInDom();
        }*/

        Phoenix.UI.Panel.callBase(this, "instantiateInDom", [ domElement ]);
    },
    
    instantiateChildInDom: function(control, type, refControl) {
        /*if (!this._layoutEngine || !control.isInDocumentFlow()) {*/
            Phoenix.UI.Panel.callBase(this, "instantiateChildInDom", [ control, type, refControl ]);            
        /*} else {
            this._layoutEngine.instantiateChildInDom(control, type, refControl);
        }*/
    },
      
    /**
    * Initializes size and position of blocks in DOM
    */
    _updateChildsDom: function() {
        this._clientWidth = null;
        this._clientHeight = null;
        
        if (this._layoutEngine) {
            var controlsLength = this.controls.length;
            
            for (var i = 0; i < controlsLength; i++) {
                var control = this.controls[i];
                
                if (control.isInDocumentFlow()) {
                    this._layoutEngine.updateChildDom(control);
                }
            }
        }
    },

    removeChildFromDom: function(control) {
        Phoenix.UI.Panel.callBase(this, "removeChildFromDom", [ control ]);

        /*if (this._layoutEngine) {
            this._layoutEngine.removeChildFromDom(control);
        }*/
    },
    
    free: function() {
        Phoenix.UI.Panel.callBase(this, "free");
    }
};

Phoenix.UI.Panel.createClass('Phoenix.UI.Panel', Control);
ControlsFactory.registerControl('panel', Phoenix.UI.Panel);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Repeater = function() {
    Phoenix.UI.Repeater.constructBase(this);
};

Phoenix.UI.Repeater.prototype = {
    firstItemCssClass: null,
    _emptyDataTemplate: null,

    initFromOptions: function (options) {
        this.set_template(options.template);
        this.firstItemCssClass = options.firstItemCssClass;

        if (options.templateSelector) {
            options.cacheDisabled = true;
        }

        if (options.orientation == 'horizontal') {
            options.width = options.width || '?';
            options.height = options.height || '*';
        } else {
            options.width = options.width || '*';
            options.height = options.height || '?';
        }

        if (options.emptyDataTemplate) {
            this._emptyDataTemplate = new Template(options.emptyDataTemplate);
        } else if (options.emptyDataText) {
            this._emptyDataTemplate = new Template({
                type: 'label',
                width: '?',
                height: '28px',
                text: options.emptyDataText,
                cssClass: 'data_not_found_label',
                padding: '5px'
            });
        }

        Phoenix.UI.Repeater.callBase(this, "initFromOptions", [options]);
        this._checkEmptiness();
    },

    _getTemplate: function(dataItem) {
        if (isFunction(this.options.templateSelector)) { // TODO: Should we rename that property?
            var result = new Template(this.options.templateSelector(dataItem));

            if (result) {
                return result;
            }
        }

        return new Template(this.get_template());
    },

    createItem: function (dataItem) {
        var template = this._getTemplate(dataItem);
        var control = template.instantiate(dataItem, this);
        control.container = this;

        if (this.firstItemCssClass && this.get_childsHash().size() == 0) {
            control.addCssClass(this.firstItemCssClass);
        }

        this.raise_itemCreated({ item: control, data: dataItem });

        return control;
    }
};

Trait.Apply(Phoenix.UI.Repeater.prototype, BaseListDataControl, {
    asOriginal: ['set_dataSource']
});
//Object.extend(Phoenix.UI.Repeater.prototype, BaseListDataControl);

Auto.Events(Phoenix.UI.Repeater.prototype, [
    'itemCreated'
]);

Auto.Properties(Phoenix.UI.Repeater.prototype, [
    { name: 'template' }
]);

Phoenix.UI.Repeater.createClass('Phoenix.UI.Repeater', Phoenix.UI.Panel);
ControlsFactory.registerControl('repeater', Phoenix.UI.Repeater);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Button = function() {
    Phoenix.UI.Button.constructBase(this);
};

Phoenix.UI.Button.prototype = {
    defaultOptions: {
        disabledCssClass: 'link_button_disabled',
        cssClass: 'link_button',
        padding: '20 0 0 13',
        margin: '1 0',
        width: '?',
        height: '33',
        maxWidth: 200,
        maxHeight: 33,
        text: 'Button',
        bindings: {
            '*': 'text'
        }
    },

    initFromOptions: function(options) {
        Phoenix.UI.Button.callBase(this, "initFromOptions", [ options ]);
        
        this._text = options.text;
        
        if (typeof(options.onClick) == 'function') {
            this.add_onClick(options.onClick, this);
        }
    },
    
    set_text: function(value) {
        if (this._text === value) {
            return;
        }
        
        this._text = value;
        
        if (this.domElement) {
            $(this.domElement.firstChild).text(this._text);
            this.update();
        }
    },

    update: function() {
        if (this.domElement) {
            DOM.setBoundingRect(this.domElement.firstChild, null, null);
        }

        Phoenix.UI.Button.callBase(this, "update");
    },

    updateDom: function() {
        Phoenix.UI.Button.callBase(this, "updateDom");
        
        if (this.domElement) {
            DOM.setBoundingRect(this.domElement.firstChild, Math.max(this.get_innerWidth() - this.get_padding().get_width(), 0) || null, this.get_innerHeight() || null);
        }
    },
      
    /**
     * Instantiate this control into DOM
     */
    instantiateInDom: function(domElement) {
        var element = DOM.create('a', domElement);
        element.href = "javascript:void(0);";
        element.innerHTML = ['<span>', this._text, '</span>'].join('');
        
        this.domElement = element;
        
        $(element).bind('click',
            function() {
                if (this._enabled) {
                    this.raise_onClick()
                }
            }.bind(this)
        );
        
        DOM.disableSelection(element);
        Phoenix.UI.Button.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Events(Phoenix.UI.Button.prototype, [
    'onClick'
]);

Auto.Properties(Phoenix.UI.Button.prototype, [
    { name: 'text' }
]);

Phoenix.UI.Button.createClass('Phoenix.UI.Button', Control);
ControlsFactory.registerControl('button', Phoenix.UI.Button);
Type.createNamespace('Phoenix.UI');

var CheckBoxStates = {
    empty: 0,
    checked: 1,
    gray: 2
};

Phoenix.UI.Checkbox = function() {
    Phoenix.UI.Checkbox.constructBase(this);
    this._state = CheckBoxStates.empty;
};

Phoenix.UI.Checkbox.prototype = {
    _text: null,
    _selectedCssClass: null,
    _isTriState: null,

    defaultOptions: {
        disabledCssClass: 'checkbox_disabled',
        width: '?',
        height: '16px',
        bindings: {
            '*': 'state'
        }
    },

    initFromOptions: function (options) {
        this.addCssClass('checkbox');
        this._selectedCssClass = 'checkbox_selected';
        this._grayCssClass = 'checkbox_gray';
        this._text = options.text;
        this._state = options.state || CheckBoxStates.empty;
        this._isTriState = !!options.isTriState;

        if (isFunction(options.onChanged)) {
            this.add_onChanged(options.onChanged, this);
        }

        Phoenix.UI.Checkbox.callBase(this, "initFromOptions", [options]);
    },

    set_text: function (value) {
        if (this._text === value) {
            return;
        }

        this._text = value;
        this._initTextNode();

        if (this._textNode) {
            this._textNode.data = value;
        }
    },

    set_state: function (value) {
        var oldValue = this._state;

        if (value === true) {
            value = CheckBoxStates.checked;
        }

        if (value === false || isNullOrUndefined(value)) {
            value = CheckBoxStates.empty;
        }

        if (oldValue === value) {
            return;
        }

        value = value*1;

        this._state = value;
        this._applyState();
        this.raise_stateChanged({ newValue: value, oldValue: oldValue });
    },

    _applyState: function () {
        if (this.domElement) {
            if (this._state == CheckBoxStates.checked) {
                this.addCssClass(this._selectedCssClass);
            } else {
                this.removeCssClass(this._selectedCssClass);
            }
            
            if (this._state == CheckBoxStates.gray) {
                this.addCssClass(this._grayCssClass);
            } else {
                this.removeCssClass(this._grayCssClass);
            }
        }
    },

    _initTextNode: function () {
        if (this.domElement && this._text && !this._textNode) {
            this._textNode = DOM.createTextNode(this._text, this.domElement);
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('div', domElement);
        this.checkNode = DOM.create('span', this.domElement);
        this._initTextNode();

        $(this.domElement).click(function (ev) {
            if (this._enabled) {
                this.setNextState();
            }
        }.bind(this));

        this._applyState();

        DOM.disableSelection(this.domElement);

        Phoenix.UI.Checkbox.callBase(this, "instantiateInDom", [domElement]);
    },

    setNextState: function () {
        switch (this._state) {
            case CheckBoxStates.empty:
                this.set_state(this._isTriState ? CheckBoxStates.gray : CheckBoxStates.checked);
                break;
            case CheckBoxStates.gray:
                this.set_state(CheckBoxStates.checked);
                break;
            case CheckBoxStates.checked:
                this.set_state(CheckBoxStates.empty);
                break;
        }
        this.raise_onChanged();
    },

    free: function() {
        DOM.remove(this.checkNode);
        delete this.checkNode;
        Phoenix.UI.Checkbox.callBase(this, "free");
    }
};

Auto.Events(Phoenix.UI.Checkbox.prototype, [
    'onChanged'
]);

Auto.Properties(Phoenix.UI.Checkbox.prototype, [
    { name: 'text' },
    { name: 'state', autoEvent: true }
]);

Phoenix.UI.Checkbox.createClass('Phoenix.UI.Checkbox', Control);
ControlsFactory.registerControl('checkbox', Phoenix.UI.Checkbox);

///
/// Warning! It seems that control is nowhere used
///

Type.createNamespace('Phoenix.UI');

Phoenix.UI.CheckList = function () {
    Phoenix.UI.CheckList.constructBase(this);
};

Phoenix.UI.CheckList.prototype = {
    initFromOptions: function (options) {
        var thisObj = this;

        options.layout = 'stack';
        options.orientation = 'vertical';

        options.bindings = {
            '*': 'container',
            '*.changed': function (sender, args) {
                this._processSelection();
            }
        };

        options.displayProperty = options.displayProperty || 'name';

        options.controls = [
            {
                id: 'container',
                type: 'panel',
                layout: 'stack',
                orientation: 'vertical',
                height: '100%',
                bindings: {
                    '*': 'itemsPanel'
                },
                controls: [
                    {

                        type: 'scrollablePanel', id: 'itemsPanel', width: '100%', height: '100%-29',
                        bindings: { '*': 'items' },

                        controls: [
                            { type: 'repeater', id: 'items', width: '100%',
                                template: {
                                    type: 'panel', width: '100%', height: '24',
                                    customFunctions: {
                                        select: function () { this.checkBox.set_state(1); },
                                        deselect: function () { this.checkBox.set_state(0); }
                                    },

                                    bindings: {
                                        '*': function (sender, args) {
                                            var value = args.newValue;
                                            this.checkBox.set_text(value['get_' + thisObj.options.displayProperty]());
                                        }
                                    },
                                    controls: [
                                        { type: 'checkbox', id: 'checkBox', width: '100%', height: 24,
                                            onChanged: function (sender) {
                                                if (this.get_state() === CheckBoxStates.checked)
                                                    thisObj.get_selectedItems().add(this.parent.get_dataSource(), { source: thisObj });
                                                else
                                                    thisObj.get_selectedItems().remove(this.parent.get_dataSource(), { source: thisObj });
                                            }
                                        }
                                    ]
                                }
                            }
                        ]
                    },
                    {
                        type: 'searchPane',
                        onSearch: function (sender, args) {
                            var filterQuery = args.query || '';
                            this.parent.get_dataSource().set_searchQuery(filterQuery, true);
                        }
                    }
                ]
            }
        ];

        this.add_selectedItemsChanged(this.__selectedItemsChanged.bind(this));

        Phoenix.UI.CheckList.callBase(this, "initFromOptions", [options]);
    },

    __selectedItemsChanged: function (sender, args) {
        if (args.oldValue && args.oldValue.__observable)
            args.oldValue.remove_changed(this.__selectedItems_onChanged, this);

        this._processSelection();

        if (args.newValue && args.newValue.__observable)
            args.newValue.add_changed(this.__selectedItems_onChanged, this);
    },

    __selectedItems_onChanged: function (sender, args, context) {
        if (context && context.source && context.source === this)
            return;

        var controls = this.container.itemsPanel.items.get_childsHash().values();

        controls
            .where(function (control) { return args.added.contains(control.get_dataSource()); })
            .forEach(function (control) { control.select(); })

        controls
            .where(function (control) { return args.removed.contains(control.get_dataSource()); })
            .forEach(function (control) { control.deselect(); })
    },

    _processSelection: function () {
        if (!this._dataSource || this._dataSource.length == 0)
            return;

        this._clearSelections();

        if (!this._selectedItems || this._selectedItems.length == 0) {
            return;
        }

        var repeater = this.container.itemsPanel.items;

        this._selectedItems.forEach(function (item) {
            var found = repeater.findByDataItem(item);

            if (found)
                found.select();
        });
    },

    _clearSelections: function () {
        this.container.itemsPanel.items.get_childsHash().values().forEach(function (item) {
            item.deselect();
        });
    },

    instantiateInDom: function (domElement) {
        Phoenix.UI.CheckList.callBase(this, "instantiateInDom", [domElement]);
    }
};

Auto.Properties(Phoenix.UI.CheckList.prototype, [
    { name: 'selectedItems', autoEvent: true }
]);

Phoenix.UI.CheckList.createClass('Phoenix.UI.CheckList', Phoenix.UI.Panel);
ControlsFactory.registerControl('checkList', Phoenix.UI.CheckList);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.CollapsablePanel = function() {
    Phoenix.UI.CollapsablePanel.constructBase(this);
};

Phoenix.UI.CollapsablePanel.prototype = {
    _title: null,
    _oldHeight: null,
    _isCollapsed: null,
    _showCollapseButton: null,
    _showStatusBar: null,
    _cookieEnabled: null,
    _cookieKey: null,
    _scrolling: null,
    
    set_title: function(value) {
        if (this._title === value) {
            return;
        }
    
        this.header.lblTitle.set_text(value);
        this._title = value;
    },
    
    set_statusText: function(value) {
        if (this._statusText === value) {
            return;
        }
        
        if (this.statusBar) {
            this.statusBar.lblStatus.set_dataSource(value);
        }

        this._statusText = value;
    },

    showStatusBar: function() {
        if (this.statusBar) {
            this.statusBar.show();
        }
    },
    
    hideStatusBar: function() {
        if (this.statusBar) {
            this.statusBar.hide();
        }
    },

    initFromOptions: function(options) {
        this._showCollapseButton = isNullOrUndefined(options.showCollapseButton) ? true : options.showCollapseButton;
        this._headerAsCollapseButton = isNullOrUndefined(options.headerAsCollapseButton) ? true : options.headerAsCollapseButton;
        
        if (isFunction(options.onCollapsed)) {
            this.add_onCollapsed(options.onCollapsed, this);
        }
        
        if (isFunction(options.onExpanded)) {
            this.add_onExpanded(options.onExpanded, this);
        }
        
        if (isFunction(options.onCommand)) {
            this.add_onCommand(options.onCommand, this);
        }
        
        options.minHeight = options.minHeight || 28;
        this._scrolling = options.scrolling;
        
        this.addCssClass('collapsable_panel');
        this._cookieKey = options.cookieKey;
        this._cookieEnabled = !!this._cookieKey;
        this._statusText = options.statusText;
        
        var panel = {};

        // TODO: !!! Bad smell code
        panel.type = this._scrolling ? 'scrollablePanel' : 'panel';
        panel.cssClass = "section " + (options.innerPanelCssClass || '');
        panel.id = 'innerPanel';
        panel.border = '1 0 1 1';
        panel.width = '100%';
        panel.height = options.height == '?' ? '?' : '*';
        panel.layout = options.layout;
        panel.orientation = options.orientation;
        panel.controls = options.controls;
        panel.valign = options.valign;
        panel.halign = options.halign;
        panel.padding = options.padding;
        panel.columns = options.columns;
        panel.rows = options.rows;
        panel.vertical = options.vertical;
        panel.horizontal = options.horizontal;
        panel.margin = '0 1 0 0';
        options.padding = undefined;
        
        if (options.bindings) {
            panel.bindings = { '*': '*' };
        }
        
        options.layout = 'stack';
        options.orientation = 'vertical';
        
        options.controls = [
            {
                type: 'panel',
                cssClass: this._showCollapseButton ? 'section_header_expandable section_header' : 'section_header',
                width: '100%',
                height: options.showHeader == false ? '0' : '28',
                padding: '5px 0',
                border: options.showHeader == false ? '1 0 1 1' : '1',
                id: 'header',
                orientation: 'horizontal',
                domHandlers: (this._showCollapseButton && this._headerAsCollapseButton) ? {
                    click: this._collapseClicked.bind(this)
                } : null,
                controls: [
                    {
                        id: 'lblTitle',
                        type: 'label',
                        width: '*',
                        height: '20px',
                        valign: 'middle',
                        text: options.title || ''
                    },
                    {
                        id: 'lnkCollapse',
                        type: 'link',
                        width: '18px',
                        height: '*',
                        focusable: false,
                        cssClass: 'collapse_arrow',
                        text: '',
                        domHandlers: (this._showCollapseButton && !this._headerAsCollapseButton) ? {
                            click: this._collapseClicked.bind(this)
                        } : null
                    }
                ]
            },
            panel
        ];
        
        if (options.showStatusBar) {
            var thisObj = this;

            var statusLabel = {
                type: 'label',
                hideIfEmpty: true,
                width: '?',
                padding: '5 0'
            };

            if (options.statusLabelTemplate) {
                statusLabel = options.statusLabelTemplate;
            }
            
            statusLabel.id = 'lblStatus';
            statusLabel.cssClass = 'status_label';

            var statusBar = {
                id: 'statusBar',
                type: 'panel',
                cssClass: 'section_status',
                height: '25',
                width: '100%',
                border: '1 0 1 1',
                orientation: 'horizontal',
                customFunctions: {
                    'setButtonStatus': function(buttonName, status) {
                        var button = this.buttons.findControl(function(c) { return c.get_text() == buttonName; });
                        if (!button) {
                            throw new Error('button ' + buttonName + ' not found!');
                        }
                        button.set_enabled(status);
                    }
                },
                controls: [
                    statusLabel,
                    {
                        id: 'buttons',
                        type: 'repeater',
                        cssClass: 'status_buttons',
                        width: '?',
                        height: '*',
                        orientation: 'horizontal',
                        template: {
                            type: 'button',
                            padding: '10 0',
                            border: '1',
                            margin: '1',
                            height: '*',
                            cssClass: 'status_button',
                            bindings: {
                                '*': 'text'
                            },
                            onClick: function() {
                                thisObj.raise_onCommand({ button: this.get_text() });
                            }
                        }
                    }
                ],
                onLoad: function() {
                    this.buttons.set_dataSource([].makeObservable());
                    if (this.lblStatus) {
                        this.lblStatus.set_dataSource(thisObj._statusText);
                    }
                    
                    if (options.buttons) {
                        thisObj.set_buttons(options.buttons);
                    }
                }
            };
            
            options.controls.add(statusBar);
        }
        
        Phoenix.UI.CollapsablePanel.callBase(this, "initFromOptions", [ options ]);
    },

    _childsTreeLoaded: function() {
        this.__registerInnerChilds();
        Phoenix.UI.CollapsablePanel.callBase(this, "_childsTreeLoaded");
    },

    __registerInnerChilds: function() {
        var controls = this.innerPanel.controls;

        for (var i = 0; i < controls.length; i++) {
            var child = controls[i];
            this.controls._registerControl(child);
        }
    },
    
    __unregisterInnerChilds: function() {
        var controls = this.innerPanel.controls;

        for (var i = 0; i < controls.length; i++) {
            var child = controls[i];
            this.controls._unregisterControl(child);
        }
    },

    get_buttons: function() {
        return this.statusBar.buttons.get_dataSource();
    },

    setButtonStatus: function(buttonName, status) {
        if (this.statusBar) {
            this.statusBar.setButtonStatus(buttonName, status);
        }
        else {
            throw new Error("Status bar is not enabled! Use 'showStatusBar' option");
        }
    },
    
    set_buttons: function(buttons) {
        var ds = this.statusBar.buttons.get_dataSource();
        ds.clear();
        
        //buttons.reverse();
        ds.add(buttons);
    },
    
	instantiateInDom: function(domElement) {
        Phoenix.UI.CollapsablePanel.callBase(this, "instantiateInDom", [ domElement ]);
	    
        if (this.get_isCollapsed()) {
            this._collapse();
        }
        
        this._initCollapsable();
	},

    free: function() {
        this.__unregisterInnerChilds();
        Phoenix.UI.CollapsablePanel.callBase(this, "free");
    },
	
	_setCollapsedHeight: function() {
        this._oldHeight = this.get_height();
        this._oldMinHeight = this.options.minHeight;
        this.options.minHeight = undefined;
        this.set_height(new DimensionUnit('?'));
	},
	
	_setOriginalHeight: function() {
        this.set_height(this._oldHeight);
        this.options.minHeight = this._oldMinHeight;
	},

    _collapse: function() {
        var collapseLink = this.header.lnkCollapse;
        var $element = $(this.innerPanel.domElement);

        collapseLink.addCssClass("collapse_arrow_toggled");
        collapseLink.removeCssClass("collapse_arrow");
            
        $element.fadeOut(350, function() {
            this._setCollapsedHeight();
            this.innerPanel.hide();
            this.raise_onCollapsed();
            this.update();
        }.bind(this));
    },

    _expand: function() {
        var collapseLink = this.header.lnkCollapse;
        var $element = $(this.innerPanel.domElement);

        collapseLink.removeCssClass("collapse_arrow_toggled");
        collapseLink.addCssClass("collapse_arrow");
            
        if (this._oldHeight) {
            this._setOriginalHeight();
            this.innerPanel.show();
            this.update();
        }
            
        $element.fadeIn(350);
        this.raise_onExpanded();
    },
	
	get_isCollapsed: function() {
	    if (this._isCollapsed === null) {
            this._isCollapsed = this._showCollapseButton && (this._cookieEnabled && $.cookie(this._getCookieKey()) == 1);	        
	    }
	    
	    return this._isCollapsed;
	},
	
	set_isCollapsed: function(value) {
	    if (value == this.get_isCollapsed()) {
	        return;
	    }
	
        if (this.get_isCollapsed()) {
            this._expand();
        }
        else {
            this._collapse();
        }

	    this._isCollapsed = value;
	    
	    if (this._cookieEnabled) {
            $.cookie(this._getCookieKey(), value ? 1 : 0, { expires: 7 });
        }
	},
	
	_getCookieKey: function() {
	    return encodeURIComponent(document.location.href + this._cookieKey);
	},
	
	_initCollapsable: function() {
        var element = this.innerPanel;
        var collapseLink = this.header.lnkCollapse;
        var $element = $(this.innerPanel.domElement);
        
        if (!this._showCollapseButton) {
            collapseLink.hide();
        }

        if (this.get_isCollapsed()) {
            collapseLink.addCssClass("collapse_arrow_toggled");
            collapseLink.removeCssClass("collapse_arrow");
            
            $element.fadeOut(0);
            
            this.raise_onCollapsed();
        }
        else {
            collapseLink.removeCssClass("collapse_arrow_toggled");
            collapseLink.addCssClass("collapse_arrow");
            
            this.raise_onExpanded();
        }
	},
	
	_collapseClicked: function(sender, args) {
        args.stopPropagation();
        args.preventDefault();
        this.set_isCollapsed(!this._isCollapsed);
	}
};

Auto.Events(Phoenix.UI.CollapsablePanel.prototype, [
    'onCollapsed',
    'onExpanded',
    'onCommand'
]);

Auto.Properties(Phoenix.UI.CollapsablePanel.prototype, [
    { name: 'title' },
    { name: 'statusText' }
]);

Phoenix.UI.CollapsablePanel.createClass('Phoenix.UI.CollapsablePanel', Phoenix.UI.Panel);
ControlsFactory.registerControl('collapsablePanel', Phoenix.UI.CollapsablePanel);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Container = function() {
    Phoenix.UI.Container.constructBase(this);
};

Phoenix.UI.Container.prototype = {
    _childContainer: null,
    
    initFromOptions: function(options) {        
        this.set_tag(options.tag);
    
        Phoenix.UI.Container.callBase(this, "initFromOptions", [ options ]);
    },
    
    get_childsContainer: function() {
        return this._childContainer;
    },
    
	instantiateInDom: function(domElement) {
	    if(this._tag) {
	        this.domElement = DOM.create(this._tag, domElement);
	    }	        
	    
	    this._childContainer = this.domElement || domElement;

        Phoenix.UI.Container.callBase(this, "instantiateInDom", [ domElement ]);
	}
};

Auto.Properties(Phoenix.UI.Container.prototype, [
    'tag'
]);

Phoenix.UI.Container.createClass('Phoenix.UI.Container', Control);
ControlsFactory.registerControl('container', Phoenix.UI.Container);

Phoenix.UI.DataGrid.Editable = {
    _buildRow: function(properties, id, cssClass, allowBinding, cellControlGetter, rowProperties, createdRow) {        
        var cell = this._createEmptyCell();
        cell.id = 'editCell';
        cell.cssClass += ' editCell'
        cell.width = '25px';
        
        
        if(createdRow.id !== 'header' && createdRow.id !== 'newItemRow')
            cell.domHandlers = {
                mouseover: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)                                
                        return;
                        
                    sender.addCssClass('hover');
                }.bind(this),
                
                mouseout: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)
                        return;
                
                    sender.removeCssClass('hover');
                    sender.removeCssClass('pressed');
                }.bind(this),
                
                mousedown: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)
                        return;
                
                    sender.addCssClass('pressed');
                }.bind(this),
                
                mouseup: function(sender, args) {
                    if(this.__editRow && sender.parent !== this.__editRow)
                        return;
                
                    sender.removeCssClass('pressed');
                }.bind(this),

                click: function(sender, args) {
                    args.stopPropagation();
                
                    if((this.__editRow && sender.parent !== this.__editRow) ||
                        this._newItemRow)
                        this.__clearActiveStates();                    
                    
                    if(!this.__editRow)
                        this.__toEditMode(sender.parent);
                    else
                        this.__endEdit(true);
                        
                }.bind(this)
            };

        createdRow.controls.add(cell);
    },
    
    __toEditMode: function(row) {
        this.__editRow = row;
    
        var ds = row.get_dataSource();
        
        for(var prop in this.options.columnProperties) {
            if(this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                continue;
                
            var cell = row[prop + 'Cell'];
            
            cell.controls.first().hide();
            
            var editControl = this.__createEditFrom(this.options.columnProperties[prop]);
            cell.controls.add(editControl, {prevControl: null});
            
            editControl.set_dataSource(ds['get_' + prop]());
        }
        
        var editCell = row.editCell;
        
        editCell.addCssClass('editMode');
        
        this.get_window().attachDomHandler('click', this.__onWindowClick_Bound);
    },
    
    __endEdit: function(applyChanges) { 
        var ds = this.__editRow.get_dataSource();

        if(applyChanges) {
            var hasErrors = false;
        
            for(var prop in this.options.columnProperties) {
                if(this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                    continue;

                var cell = this.__editRow[prop + 'Cell'];
                var editControl = cell.controls.first();

                if(editControl.validate) {
                    editControl.validate();
                    
                    if(!editControl.get_isValid())
                        hasErrors = true;
                }
            }
            
            if(hasErrors)
                return;
            
            for(var prop in this.options.columnProperties) {
                if(this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                    continue;
                
                var cell = this.__editRow[prop + 'Cell'];
                var editControl = cell.controls.first();
                
                ds['set_' + prop](editControl.get_dataSource());
            }
        }
        
        for(var prop in this.options.columnProperties) {
            if(this.options.columnProperties[prop].readOnly  || this.options.columnProperties[prop].custom)
                continue;
                
            var cell = this.__editRow[prop + 'Cell'];
            var editControl = cell.controls.first();
        
            editControl.free();  
            cell.controls.remove(editControl);  
                    
            cell.controls.last().show();
        }
        
        this.__editRow.editCell.removeCssClass('editMode');
        this.__editRow = null;
    },
    
    __createEditFrom: function(columnProperty) {
        if(columnProperty.editTemplate)
            return new Template(columnProperty.editTemplate).instantiate();

        var editControl =  ControlsFactory.create('textBox');

        var options = {
            width: columnProperty.width || '100%',
            validation: columnProperty.validation
        };

        editControl.initFromOptions(options);

        return editControl;
    },
    
    _dataSource_itemRemoved: function(sender, args) {
        if(this.__editRow && args.items.contains(this.__editRow.get_dataSource()))
            this.__editRow = null;
    }
};

Phoenix.UI.DataGrid.prototype.makeEditable = function() {
    Trait.Apply(this, Phoenix.UI.DataGrid.Editable);
};
Phoenix.UI.DataGrid.Resizable = {
    _border: { none: 0, left: 1, right: 2 },

    _resizableBorderSize: 5,

    _resizableColumnsSizes: {
        indexes: []
    },

    _buildRow: function (properties, id, cssClass, allowBinding, cellControlGetter, rowProperties, createdRow) {
        if (createdRow.id !== 'header')
            return;

        var that = this;

        var forPropFunc = function (propName) {
            var cellName = propName + 'Cell';
            var cell = createdRow.controls.single(function (c) { return c.id === cellName });

            cell.__hoveredBorder = that._border.none;

            cell.domHandlers = {
                mousemove: function (sender, args) {
                    if (this._isResizing) {
                        sender.addCssClass('resizable');
                        return;
                    }

                    var border = this._getHoveredBorder(sender, args);

                    if (border === sender.__hoveredBorder)
                        return;

                    if (border !== this._border.none) {
                        sender.addCssClass('resizable');
                    } else {
                        sender.removeCssClass('resizable');
                    }

                    sender.__hoveredBorder = border;
                } .bind(that),

                mousedown: function (sender, args) {
                    if (this._isResizing)
                        return;

                    if (sender.__hoveredBorder === this._border.none)
                        return;

                    this._beginResize(propName, sender.__hoveredBorder, args);
                } .bind(that)
            };

            that._resizableColumnsSizes['add_' + propName + 'WidthChanged'](function (sender, args) {
                var headerControl = this.header[propName + 'Cell'];

                this.get_template().controls.single(function (control) {
                    return control.id === propName + 'Cell';
                }).width = args.newValue.pixels;

                headerControl.set_width(args.newValue);
                this.header.update(headerControl);

                this.forEachRow(function (row, index) {
                    var control = row[propName + 'Cell'];

                    control.set_width(args.newValue);
                    row.update(control);
                }, this);
            }, that);
        };

        for (var prop in properties) {
            forPropFunc(prop);
        }
    },

    _getHoveredBorder: function (item, hoverArgs) {
        var itemWidth = item.get_clientWidth();

        if (0 <= hoverArgs.offsetX && hoverArgs.offsetX <= this._resizableBorderSize)
            return this._border.left;

        var delta = itemWidth - hoverArgs.offsetX;

        if (0 <= delta && delta <= this._resizableBorderSize)
            return this._border.right;

        return this._border.none;
    },

    _beginResize: function (columnName, border, args) {
        this._isResizing = true;
        this._resizeArgs = args;

        var columnIndex = this._resizableColumnsSizes.indexes.indexOf(columnName);
        var columnToResize = border === this._border.left ?
                this._getLeftVisibleColumnFrom(columnIndex - 1) :
                columnName;

        var onMouseMove = function (sender, args) {
            args.preventDefault();
            args.stopPropagation();

            if (columnToResize) {
                var delta = args.screenX - this._resizeArgs.screenX;

                var currentSize = this._resizableColumnsSizes['get_' + columnToResize + 'Width']();

                if (currentSize === undefined) {
                    currentSize = this.header[columnToResize + 'Cell'].get_width();
                }

                if (currentSize.pixels < -delta)
                    return;

                var newValue = currentSize.add(new DimensionUnit(delta));
                this._resizableColumnsSizes['set_' + columnToResize + 'Width'](newValue);

                this._resizeArgs = args;
            }
        } .bind(this);

        var onEndResize = function (sender, args) {
            this._isResizing = false;

            this.header.detachDomHandler('mousemove', onMouseMove);
            this.header.detachDomHandler('mouseup', onEndResize);
            //this.header.detachDomHandler('mouseout', onEndResize);

            if (columnToResize)
                this.raise_columnResized({ columnName: columnToResize, width: this._resizableColumnsSizes['get_' + columnToResize + 'Width']() });
        } .bind(this);

        this.header.attachDomHandler('mousemove', onMouseMove);
        this.header.attachDomHandler('mouseup', onEndResize);
        //this.header.attachDomHandler('mouseout', onEndResize);
    },

    _getLeftVisibleColumnFrom: function (index) {
        while (index >= 0 && this._hiddenColumns.contains(this._resizableColumnsSizes.indexes[index]))
            index--;

        return index >= 0 ? this._resizableColumnsSizes.indexes[index] : null;
    }
};

Auto.Event(Phoenix.UI.DataGrid.Resizable, 'columnResized');

Phoenix.UI.DataGrid.prototype.makeResizable = function (options) {
    if (this.__resizable)
        return;

    Trait.Apply(this, Phoenix.UI.DataGrid.Resizable);

    for (var prop in options.columnProperties) {
        Auto.Property(this._resizableColumnsSizes, { name: prop + 'Width', autoEvent: true });
        this._resizableColumnsSizes.indexes.add(prop);
    }

    this.__resizable = true;
};
Phoenix.UI.DataGrid.Selectable = {
    set_dataSource: function() {
        this._selectedItems.clear();
    },
    
    _dataSource_itemAdded: function(sender, args) {
        this._checkHeaderSelection();
        this.updateSelection(args.items, []);
    },

    _dataSource_itemRemoved: function(sender, args) {
        var toRemove = this._selectedItems.where(function(item) { return args.items.contains(item); } );
        
        //this._selectedItems.remove(toRemove);
        
        this._checkHeaderSelection();
        this.updateSelection([], args.items);
    },
    
    _buildRow: function(properties, id, cssClass, allowBinding, cellControlGetter, rowProperties, createdRow) {
        var cell = this._createEmptyCell({ cellType: id });
        cell.id = 'selectionCell';
        cell.width = '26px';
        
        if(createdRow.id !== 'newItemRow' && (createdRow.id !== 'header' || this._selectionMode !== 'single')) {
            var checkBox = {
                id: 'checkBox',
                type: 'checkbox',
                width: '13px',
                height: '14px',
                halign: 'center',
                valign: 'middle',
                bindings: {},
                onChanged: this._itemSelectionChanged.bind(this)
            };
            
            cell.controls.add(checkBox);
        }
        createdRow.controls.insert(0, cell);
    },
    
    _itemSelectionChanged: function(sender) {
        var row = sender.parent.parent;
        var selectionState = sender.get_state();        

        if(row.id === 'header') {
            this.changeAllSelection(selectionState);            
            return;
        }

        if (this._selectionMode == 'single') {
            this._selectedItems.clear();
        }
        
        if(selectionState) {
            this._selectedItems.add(row.get_dataSource());
        } else if (this._selectionMode == 'multiple') {
            this._selectedItems.remove(row.get_dataSource());
        }
        
        this._checkHeaderSelection();
    },
    
    updateSelection: function(added, removed) {
        var items = this._selectedItems;

        this.forEachRow(function(row) {
            if(!row.selectionCell)
                return;

            var checkBox = row.selectionCell.checkBox;
            var ds = row.get_dataSource();

            if (items.contains(ds)) {
                checkBox.set_state(CheckBoxStates.checked);
            } else {
                checkBox.set_state(CheckBoxStates.empty);
            }
        });
    },

    changeAllSelection: function(state) {
        var items = [];

        this.forEachRow(function(row) {
            if(!row.selectionCell)
                return;
        
            var checkBox = row.selectionCell.checkBox;
            
            if(checkBox.get_state() !== state)
                items.add(row.get_dataSource());
            
            checkBox.set_state(state);
        });
        
        if(state)
            this._selectedItems.add(items);
        else
            this._selectedItems.remove(items);
    },

    _selectedChanged: function(sender, args) {
        this.updateSelection();
    },

    // TODO: I think we should detach handlers on disposing too
    set_selectedItems: function(value) {
        if (this._selectedItems === value) {
            return;
        }

        this._detachSelectionHandlers();
        this._selectedItems = value;
        this._attachSelectionHandlers();
    },

    _detachSelectionHandlers: function() {
        if (this._selectedItems && this._selectedItems.__observable) {
            this._selectedItems.remove_changed(this._selectedChanged, this);
        }
    },

    _attachSelectionHandlers: function() {
        if (this._selectedItems) {
            this._selectedItems.add_changed(this._selectedChanged, this);
        }
    },
    
    _checkHeaderSelection: function() {
        if (this._selectionMode === 'multiple') {
            if (this._selectedItems.length == 0) {
                this._header.selectionCell.checkBox.set_state(CheckBoxStates.empty);
            } else if (this._selectedItems.length !== this._dataSource.length) {
                this._header.selectionCell.checkBox.set_state(CheckBoxStates.gray);
            }
            else {
                this._header.selectionCell.checkBox.set_state(CheckBoxStates.checked);
            }
        }
    }
};

Auto.Properties(Phoenix.UI.DataGrid.Selectable, [
    'selectedItems'
]);

Phoenix.UI.DataGrid.prototype.makeSelectable = function(options) {
    Trait.Apply(this, Phoenix.UI.DataGrid.Selectable);
    
    this.set_selectedItems([].makeObservable());
    this._selectionMode = options.selectionMode || 'multiple';
};
Type.createNamespace('Phoenix.UI');

Phoenix.UI.DataGrid = function() {
    Phoenix.UI.DataGrid.constructBase(this);
};

Phoenix.UI.DataGrid.prototype = {
    defaultOptions: {
        'width': '*'
    },
    
    initFromOptions: function (options) {
        if (!options.columnProperties)
            throw new Error('Column properties must be specified for dataGrid');

        options.padding = '0 3px 0 0';
        options.layout = 'stack';
        options.orientation = 'vertical';

        this.set_hiddenColumns((options.hiddenColumns || []).makeObservable());

        this.width = options.width || '25px';
        this.rowHeight = options.rowHeight || '25px';
        this.rowWidth = options.width == '?' ? '?' : '*';
        this.headerHeight = options.headerHeight || '25px';

        this.addCssClass('dataGrid');

        if (options.selectable)
            this.makeSelectable(options);

        if (options.editable)
            this.makeEditable(options);

        if (options.resizable)
            this.makeResizable(options);

        options.template = this._createRowTemplate(options.rowProperties, options.columnProperties);
        Phoenix.UI.DataGrid.callBase(this, "initFromOptions", [options]);

        this.set_header(this._createHeader(options.columnProperties));

        if (options.newItemResolver) {
            this.set_newItemPanelTemplate(options.newItemPanelTemplate || this._createNewItemPanelTemplate(options.columnProperties));

            this.set_newItemPanel(this._createNewItemPanel());
        }

        this.__onWindowClick_Bound = this.__onWindowClick.bind(this);
    },

    set_dataSource: function (value) {
        Phoenix.UI.DataGrid.callBase(this, "set_dataSource", [value]);
        
        if (value.__isOrdered) {
            this._setSortOrderMarks(value);
        }

        if (value && value.length > 0) {
            this._styleRows();
        }
    },

    _setSortOrderMarks: function(value) {
        if(value._orderByAndSortDirection !== undefined) {
            for (var i = 0; i < value._orderByAndSortDirection.length; i++) {
                var orderItem = value._orderByAndSortDirection[i];
                var orderPropName = orderItem.value.substr(1);
                
                for (var j = 0; j < this.header.options.controls.length; j++) {
                    var headerLabel = this.header.controls[j].headerLabel;

                    if (!headerLabel) {
                        continue;
                    }

                    if (orderPropName === headerLabel.options.sortName) {
                        if (orderItem.order === 'asc') {
                            headerLabel.set_cssClass('sort_up');
                        } else {
                            headerLabel.set_cssClass('sort_down');
                        }
                    }
                }
            }
        }
    },

    _clearSortOrderMarks: function(value) {
        for (var j = 0; j < this.header.options.controls.length; j++) {
            var headerLabel = this.header.controls[j].headerLabel;

            if (!headerLabel) {
                continue;
            }

            if(headerLabel.options == null) {
                continue;
            }

            if(headerLabel.options.sortName === undefined) {
                headerLabel.set_cssClass('');
            } else {
                headerLabel.set_cssClass('sort_no');
            }
        }
    },
        
    _dataSource_itemAdded: function (sender, args) {
        Phoenix.UI.DataGrid.callBase(this, "_dataSource_itemAdded", [sender, args]);

        this._styleRows();
    },

    _dataSource_itemRemoved: function (sender, args) {
        Phoenix.UI.DataGrid.callBase(this, "_dataSource_itemRemoved", [sender, args]);

        if (this._dataSource.length > 0)
            this._styleRows();
    },

    _createHeader: function (properties) {
        this.properties = properties;
        var row = this._buildRow(properties, 'header', 'dataGrid_header', false,
            function (propName, propValue) {
                return function(){
                    return {
                        id: 'headerLabel',
                        type: 'label',
                        width: '*',
                        height: '*',
                        text: propValue.headerText || '',
                        cssClass: propValue.sortName !== undefined ? 'sort_no' : '',
                        sortName: propValue.sortName,
                        domHandlers: propValue.sortName !== undefined ? {
                            click: function (element, arg) {
                                this._changeSortingOrder(element, propValue.sortName, arg.ctrlKey);
                                this.get_dataSource().load();
                            }.bind(this)
                        }: {}
                    };
                }.bind(this)();
            }.bind(this));

        row.height = this.headerHeight;

        var control = ControlsFactory.create('panel');
        control.initFromOptions(row);

        return control;
    },

    _getSortingOrderByItemName: function(itemName, dataSource) {
        for(var i = 0;i < dataSource._orderByAndSortDirection.length; i++) {
            if(dataSource._orderByAndSortDirection[i].value.substr(1) === itemName) {
                return dataSource._orderByAndSortDirection[i].order;
            }
        }
        return '';
    },
        
    _changeSortingOrder: function(element, itemName, isMultiColumnSorting) {
        var dataSource = this.get_dataSource();
        
        if (!dataSource.__isOrdered) {
            return;
        }

        var currentOrder = this._getSortingOrderByItemName(itemName,dataSource);

        if (isMultiColumnSorting) {
            switch (currentOrder) {
                case '':
                    element.set_cssClass('sort_up');
                    dataSource._orderByAndSortDirection.push({ value: '_' + itemName, order: 'asc' });
                    break;
                case 'asc':
                    element.set_cssClass('sort_down');
                    this._changeDataSourceSortingOrder(dataSource, itemName, 'desc');
                    break;
                case 'desc':
                    element.set_cssClass('sort_up');
                    this._changeDataSourceSortingOrder(dataSource, itemName, 'asc');
                    break;
            }
        } else {
            this._clearSortOrderMarks(dataSource);
            dataSource._orderByAndSortDirection = new Array();
            
            switch(currentOrder) {
                case 'asc':
                    element.set_cssClass('sort_down');
                    dataSource._orderByAndSortDirection.push({ value: '_' + itemName, order: 'desc' });
                    break;
                case 'desc':
                case '':
                    element.set_cssClass('sort_up');
                    dataSource._orderByAndSortDirection.push({ value: '_' + itemName, order: 'asc' });
                    break;
            }
        }

        var sortingString = this._composeSortingString(dataSource._orderByAndSortDirection);

        if (sortingString !== '') {
            dataSource.set_orderBy(sortingString);
        }
    },

    _changeDataSourceSortingOrder: function(dataSource, itemName, operationType) {
        for(var i=0;i<dataSource._orderByAndSortDirection.length;i++) {
            if(dataSource._orderByAndSortDirection[i].value.substr(1) === itemName) {
                switch (operationType) {
                    case '':
                        dataSource._orderByAndSortDirection.splice(i,1);
                        break;
                    case 'asc':
                        dataSource._orderByAndSortDirection[i].order = 'asc';
                        break;
                    case 'desc':
                        dataSource._orderByAndSortDirection[i].order = 'desc';
                        break;
                }
                break;
            }
        }
    },

    _composeSortingString: function(orderByAndSortDirection) {
        var orderArray = new Array();
        for(var i=0;i<orderByAndSortDirection.length;i++) {
            orderArray.push(orderByAndSortDirection[i].value.substr(1) + ' ' + orderByAndSortDirection[i].order);
        }
        return orderArray.join();
    },     

    _createRowTemplate: function (rowProperties, properties) {
        return this._buildRow(properties, null, null, true, function (propName, propValue) {
            return propValue.template || {
                type: 'label',
                height: this.rowHeight == '?' ? '?' : '*',
                width: '*',
                text: '',
                multiline: propValue.multiline,
                format: propValue.format
            };
        }.bind(this), rowProperties);
    },

    _buildRow: function (properties, id, cssClass, allowBinding, cellControlGetter, rowProperties) {

        var that = this;

        var row = this._createEmptyRow(rowProperties);

        if (id != null && id != undefined)
            row.id = id;

        if (cssClass)
            row.cssClass += ' ' + cssClass;

        for (var name in properties) {
            if (typeof properties[name] === 'function')
                continue;

            properties[name].cellType = id;

            var cell = this._createEmptyCell(properties[name]);
            cell.id = name + 'Cell';
            cell.controls.add(cellControlGetter(name, properties[name]));

            if (this._hiddenColumns.contains(name))
                cell.visible = false;

            row.controls.add(cell);

            if (allowBinding) {
                var bindingName = properties[name].custom ? '*' : name;

                if (!row.bindings[bindingName]) {
                    row.bindings[bindingName] = function (sender, args) {
                        var functions = arguments.callee.__bindingsArray;

                        for (var i = 0; i < functions.length; i++) {
                            functions[i].apply(this, [sender, args]);
                        }
                    };
                }

                if (!row.bindings[bindingName].__bindingsArray) {
                    row.bindings[bindingName].__bindingsArray = [];
                }

                row.bindings[bindingName].__bindingsArray.add((function (n) {
                    return function (sender, args) {
                        this[n + 'Cell'].controls.last().set_dataSource(args.newValue);

                        if (properties[n].showTooltip) {
                            this[n + 'Cell'].set_tooltip(args.newValue);
                        }
                    };
                })(name));
            }
        }

        return row;
    },

    _createEmptyRow: function (rowOptions) {
        var row = {
            type: 'panel',
            orientation: 'horizontal',
            cssClass: 'dataGrid_row',
            width: this.rowWidth,
            height: this.rowHeight,
            controls: [],
            bindings: {}
        };

        if (rowOptions) {
            for (var prop in rowOptions) {
                if (rowOptions.hasOwnProperty(prop)) {
                    row[prop] = rowOptions[prop];
                }
            }
        }

        return row;
    },

    _createEmptyCell: function (options) {
        var cell = {
            type: 'panel',
            cssClass: 'dataGrid_cell',
            height: '100%',
            padding: '3px',
            border: (options && options.cellType == 'header') ? '1px 1px 1px 0px' : '1px 1px 0px 0px',
            controls: [],
            bindings: {}
        };

        if (options) {
            if (options.width)
                cell.width = options.width;

            if (options.minWidth)
                cell.minWidth = options.minWidth;

            if (options.maxWidth)
                cell.maxWidth = options.maxWidth;

            if (options.cssClass)
                cell.cssClass += ' ' + options.cssClass;
        }

        return cell;
    },

    _createNewItemPanelTemplate: function (properties) {
        var row = this._buildRow(properties, 'newItemRow', null, true, function (propName, propValue) {
            var control = propValue.createTemplate || propValue.editTemplate || {
                type: propValue.readOnly || propValue.custom ? 'label' : 'textBox',
                width: '95%',
                text: '',

                validation: propValue.validation
            };

            if (propValue.custom)
                control.bindings = {};

            return control;
        });

        var editCell = row.controls.last();

        if (editCell.id !== 'editCell')
            throw new Error('Edit mode must be active for new row creation');

        editCell.cssClass += ' editMode';

        editCell.domHandlers = {
            mouseover: function (sender, args) {
                sender.addCssClass('hover');
            } .bind(this),

            mouseout: function (sender, args) {
                sender.removeCssClass('hover');
                sender.removeCssClass('pressed');
            } .bind(this),

            mousedown: function (sender, args) {
                sender.addCssClass('pressed');
            } .bind(this),

            mouseup: function (sender, args) {
                sender.removeCssClass('pressed');
            } .bind(this),

            click: function (sender, args) {
                this._newItem_onEditComplete();
            } .bind(this)
        };

        return new Template(row);
    },

    _createNewItemPanel: function () {
        var panel = ControlsFactory.create('panel');

        panel.initFromOptions({
            height: '25px',
            cssClass: 'dataGrid_addItemPanel',
            controls: [
                {
                    type: 'label',
                    text: 'Add new item',
                    cssClass: 'plusImage'
                }
            ],
            domHandlers: {
                click: function (sender, args) {
                    args.stopPropagation();

                    var dataItem = this.options.newItemResolver();

                    var editRow = this._newItemPanelTemplate.instantiate(dataItem, this);

                    this.__clearActiveStates();
                    this.set_newItemRow(editRow);
                    this.get_window().attachDomHandler('click', this.__onWindowClick_Bound);
                } .bind(this),
                mouseenter: function (sender, arguments) {
                    sender.addCssClass('hover')
                },
                mouseleave: function (sender, arguments) {
                    sender.removeCssClass('hover')
                }
            }
        });

        return panel;
    },

    _newItem_onEditComplete: function () {
        var dataItem = this._newItemRow.get_dataSource();

        var hasErrors = false;

        for (var prop in this.options.columnProperties) {
            if (this.options.columnProperties[prop].readOnly || this.options.columnProperties[prop].custom)
                continue;

            var control = this._newItemRow[prop + 'Cell'].controls.first();

            if (control.validate) {
                control.validate();

                if (!control.get_isValid())
                    hasErrors = true;
                else
                    dataItem['set_' + prop](control.get_dataSource());
            }
            else {
                dataItem['set_' + prop](control.get_dataSource());
            }
        }

        if (hasErrors)
            return;

        this.set_newItemRow(null);
        this.raise_newItemCreated(dataItem);
    },

    forEachRow: function (action) {
        var startIndex = this._header ? 1 : 0;
        var endIndex = this._newItemPanel ? this.controls.length - 2 : this.controls.length - 1;

        if (endIndex < startIndex)
            return;

        for (var i = startIndex; i <= endIndex; i++) {
            var row = this.controls[i];

            action(row, i - startIndex);
        }
    },

    set_header: function (value) {
        if (this._header === value)
            return;

        if (this._header) {
            this._header.free();
            this.controls.remove(this._header);
        }

        this._header = value;

        if (!value)
            return;

        this.controls.add(value, { prevControl: null, alwaysFirst: true });
    },

    set_newItemPanel: function (value) {
        if (this._newItemPanel === value)
            return;

        if (this._newItemPanel) {
            this._newItemPanel.free();
            this.controls.remove(this._newItemPanel)
        }

        this._newItemPanel = value;

        if (!value)
            return;

        this.controls.add(value, { prevControl: this.controls[this.controls.length] - 1 });
    },

    set_newItemRow: function (row) {
        if (this._newItemRow === row)
            return;

        if (this._newItemRow) {
            this.controls.remove(this._newItemRow);
            this._newItemRow.free();
            this._newItemRow = null;
        }

        if (!row) {
            this._newItemPanel.show(true);
        }
        else {
            this._newItemPanel.hide();

            row.parent = this;
            this.controls.add(row);
        }

        this._newItemRow = row;
    },

    set_hiddenColumns: function (value) {
        if (value === this._hiddenColumns)
            return;

        var oldValue = this._hiddenColumns;

        if (oldValue) {
            if (value)
                oldValue.synchronize(value);

            oldValue.remove_changed(this.__hiddenColumns_Changed, this);
        }

        this._hiddenColumns = value;

        if (this._hiddenColumns)
            this._hiddenColumns.add_changed(this.__hiddenColumns_Changed, this);
    },

    _get_anchorElement: function () {
        return this._header || null;
    },

    _styleRows: function () {
        this.forEachRow(function (row, rowIndex) {
            if (rowIndex % 2) {
                row.removeCssClass('odd');
            } else {
                row.addCssClass('odd');
            }
        });
    },

    __onWindowClick: function (sender, args) {
        if (
           (this._newItemRow && !jQuery.contains(this._newItemRow.domElement, args.target)) ||
           (this.__editRow && !jQuery.contains(this.__editRow.domElement, args.target))
          )
            this.__clearActiveStates();
    },

    __clearActiveStates: function () {
        if (this._newItemRow) {
            this.set_newItemRow(null);
            this.get_window().detachDomHandler('click', this.__onWindowClick_Bound);
        }

        if (this.__editRow) {
            this.__endEdit(false);
            this.get_window().detachDomHandler('click', this.__onWindowClick_Bound);
        }
    },

    __hiddenColumns_Changed: function (sender, args) {
        args.added.forEach(function (columnName) {
            this._changeColumnVisibility(columnName, false);
        }, this);

        args.removed.forEach(function (columnName) {
            this._changeColumnVisibility(columnName, true);
        }, this);

        this.update();
    },

    _changeColumnVisibility: function (columnName, visible) {      
        this.get_template().controls.single(function (control) {
            return control.id === columnName + 'Cell';
        }).visible = visible;

        this.header[columnName + 'Cell'].set_visible(visible);
        if (visible)
            this.header.update(this.header[columnName + 'Cell']);

        this.forEachRow(function (row) {
            row[columnName + 'Cell'].set_visible(visible);
            if(visible)
                row.update(row[columnName + 'Cell']);
        } .bind(this));
    }
};

Auto.Events(Phoenix.UI.DataGrid.prototype, [
    'newItemCreated'
]);

Auto.Properties(Phoenix.UI.DataGrid.prototype, [
    'header',
    'selectable',
    'newItemPanel',
    'newItemPanelTemplate',
    'newItemRow',
    'hiddenColumns'
]);

Phoenix.UI.DataGrid.createClass('Phoenix.UI.DataGrid', Phoenix.UI.Repeater);
ControlsFactory.registerControl('dataGrid', Phoenix.UI.DataGrid);

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
Type.createNamespace('Phoenix.UI');

Phoenix.UI.DropDownList = function() {
    Phoenix.UI.DropDownList.constructBase(this);
    this.set_actionsBar([].makeObservable());
    this.add_selectedValueChanged(function() {
        this._updateSelectedItem();
    }, this);
};

Phoenix.UI.DropDownList.selectedItemCssClass = "dropDownItem_selected";

Phoenix.UI.DropDownList.prototype = {
    _textProperty: null,
    _valueProperty: null,
    _isShowed: false,
    _actionsBar: null,
    _stretchToMaxItem: false,
    _isDisabledBefore: false,
    _waitingForRightItem: false,
    _autoSelectDefaultValue: false,

    defaultOptions: {
        width: '*',
        height: 20,
        'border': '1',
        disabledCssClass: 'dropDownList_disabled',
        bindings: {
            '*': 'selectedValue'
        }
    },

    set_enabled: function (value) {
        Phoenix.UI.DropDownList.callBase(this, "set_enabled", [value]);

        if (!value && this._isShowed) {
            this.hideDropDown();
        }
    },

    initFromOptions: function (options) {
        var thisObj = this;
        var showActionPane = false;
        this._autoSelectDefaultValue = isNullOrUndefined(options.autoSelectDefaultValue) ? true : options.autoSelectDefaultValue;

        this.get_actionsBar().add_changed(this.__actionsBarChanged, this);

        this.addCssClass('dropDownList');
        this._textProperty = options.textProperty;
        this._valueProperty = options.valueProperty || "*";
        this._stretchToMaxItem = options.width == '?';
        this._notSelectedControl = null;

        if (isFunction(options.onChanged)) {
            this.add_onChanged(options.onChanged, this);
        }

        if (isFunction(options.onCommand)) {
            this.add_onCommand(options.onCommand, this);
        }

        if (isFunction(options.onClose)) {
            this.add_onClose(options.onClose, this);
        }

        if (isFunction(options.onOpen)) {
            this.add_onOpen(options.onOpen, this);
        }

        if (options.template) {
            this._template = options.template;
            this._bindingProperty = options.bindingProperty || '*';
        } else {
            if (options.bindingsProperty) {
                throw new Error("The \"bindingProperty\" cannot used when template was not defined");
            }

            /*
            if (!options.textProperty) {
                throw new Error("The \"textProperty\" is not specified");
            }*/

            this._bindingProperty = options.textProperty || '*';
            this._template = { type: 'label', padding: '3', width: '?', height: '22px' };
        }

        if (options.selectedItemTemplate) {
            this._selectedItemTemplate = options.selectedItemTemplate;
            this._selectedItemTemplate.id = 'dropdownItem';
        }

        if (isArray(options.actions)) {
            showActionPane = true;
        }

        this._template.id = 'dropdownItem';
        this._listHeight = options.listHeight*1 || 150;

        if (options.items && (options.items instanceof Array)) {
            var setDs = function () {
                this.set_items(options.items.makeObservable());
                this.remove_onLoaded(setDs);
            }

            this.add_onLoaded(setDs, this);
        }

        options.controls = [
            {
                id: 'dropDownPanel',
                type: 'panel',
                height: '?',
                visible: false,
                border: '1px',
                cssClass: 'dropDownList_container ' + (options.cssClass + '_container' || ''),
                onLoad: function () {
                    document.body.appendChild(this.domElement);
                },
                onFree: function () {
                    document.body.removeChild(this.domElement);
                },
                controls: [
                    {
                        id: 'scrollable',
                        type: 'scrollablePanel',
                        height: '?',
                        maxHeight: this._listHeight,
                        controls: [
                            {
                                type: 'repeater',
                                height: '?',
                                id: 'list',
                                cacheDisabled: true,
                                templateSelector: this._getTemplate.bind(this),
                                onLoad: function () {
                                    if (options.notSelectedItem) {
                                        thisObj.set_notSelectedItem(options.notSelectedItem);
                                    }
                                }
                            }
                        ]
                    }

                ]
            }
        ];

        Phoenix.UI.DropDownList.callBase(this, "initFromOptions", [options]);

        if (options.actionsBar) {
            this.get_actionsBar().add(options.actionsBar);
        }
    },

    // #region Actions bar

    __getIdForAction: function(actionName) {
        return 'action' + actionName.hashCode();
    },

    __actionsBarChanged: function(sender, args) {
        if (!this.get_actionsPane()) {
            this._initActionPane();
        }

        var actionsPane = this.get_actionsPane();

        for (var i = 0; i < args.added.length; i++) {
            var options = {
                id: this.__getIdForAction(args.added[i]),
                height: '23',
                padding: '3',
                border: '0 1 0 0',
                width: '*',
                type: 'link',
                text: args.added[i],
                onClick: (function(thisObj, actionName) {
                    return function() {
                        thisObj.raise_onCommand({ command: actionName });
                    };
                })(this, args.added[i])
            }

            var control = ControlsFactory.create('link');
            control.initFromOptions(options);

            actionsPane.controls.add(control);
        }

        for (var i = 0; i < args.removed.length; i++) {
            var control = actionsPane[this.__getIdForAction(args.removed[i])];
            actionsPane.controls.remove(control);
            control.free();
        }
    },

    get_actionsPane: function() {
        return this.dropDownPanel.actionPane;
    },

    _initActionPane: function () {
        var actionPane = {
            id: 'actionPane',
            type: 'panel',
            height: '?',
            border: '0 1 0 0',
            cssClass: 'actionPane',
            controls: []
        };

        var pane = ControlsFactory.create('panel');
        pane.initFromOptions(actionPane);

        this.dropDownPanel.controls.add(pane);
    },

    // #endregion

    /**
    * Returns template for the specified item
    * @args DataItem item - element of datasource
    * @args bool withHandlers - if it is true then onhover and onclick handlers will be attached
    */
    _getTemplate: function (item, withHandlers) {
        var dropDownList = this;
        var withHandlers = isNullOrUndefined(withHandlers) ? true : withHandlers;

        var obj = {
            type: 'panel',
            height: '?',
            cssClass: 'dropDownItem',
            controls: [],
            bindings: {}
        };

        var selectedValue = this.get_selectedValue(),
            selectedItem = this._getItemByValue(selectedValue),
            isShowedItem = !withHandlers && item && (item === selectedItem || (selectedItem === null && item === this._notSelectedItem)),
            template = null;

        if (isFunction(this.options.templateSelector)) {
            template = this.options.templateSelector(item, isShowedItem);
        }

        if (!template) {
            template = this._template;

            if (isShowedItem && this._selectedItemTemplate) {
                template = this._selectedItemTemplate;
            }
        }

        if (isShowedItem) {
            obj.height = '18px';
        }

        obj.controls.add(template);

        if (withHandlers) {
            obj.onLoad = function () {
                $(this.domElement)
                .hover(
                    function () { this.addCssClass('dropDownItem_hover'); } .bind(this),
                    function () { this.removeCssClass('dropDownItem_hover'); } .bind(this)
                )
                .click(function () {
                    var dataItem = this.get_dataSource(),
                        value = dropDownList._getValueOfItem(dataItem);
                    dropDownList.set_selectedValue(value);
                    dropDownList.hideDropDown();
                } .bind(this));
            };
        }

        obj.bindings[this._bindingProperty] = '*';

        return obj;
    },

    _getDataItemText: function (dataItem) {
        if (!this._textProperty) {
            return dataItem;
        }

        if (isFunction(dataItem['get_' + this._textProperty])) {
            return dataItem['get_' + this._textProperty]();
        }

        return dataItem[this._textProperty] || null;
    },

    set_notSelectedItem: function(dataItem) {
        this._notSelectedItem = dataItem;

        if (this._notSelectedControl) {
            this._notSelectedControl.free();
            this._getItemsRepeater().controls.remove(this._notSelectedControl);
            this._notSelectedControl = null;
        }

        if (dataItem) {
            this._attachNotSelectedItemControl();
        }

        if (!this.isEmpty()) {
            this._updateSelectedItem();
        }
    },

    _attachNotSelectedItemControl: function() {
        if (this._notSelectedItem) {
            var repeater =  this._getItemsRepeater();
            var notSelectedTemplate = new Template(this._getTemplate(this._notSelectedItem, true));
            this._notSelectedControl = notSelectedTemplate.instantiate(this._notSelectedItem, repeater);
            repeater.controls.add(this._notSelectedControl, { alwaysFirst: true, prevControl: null });
        } else {
            throw new Error("Not selected item is undefined");
        }
    },

    _getItemsRepeater: function () {
        return this.dropDownPanel.scrollable.list;
    },

    _getDropDownItem: function (dataItem) {
        if(dataItem) {
            var dropDownItem = this._getItemsRepeater().findByDataItem(dataItem);
            return (dataItem == this._notSelectedItem && this.supportsNotSelectedItem()) ? this._notSelectedControl : dropDownItem;
        }
        return null;
    },

    _doForAllItemControls: function(processFunc) {
        for (var i = 0; i < this._getItemsRepeater().controls.length; i++) {
            processFunc( this._getItemsRepeater().controls[i]);
        }
    },

    _removeAllItemControlsSelectionClasses: function() {
        this._doForAllItemControls(function(itemControl) { itemControl.removeCssClass(Phoenix.UI.DropDownList.selectedItemCssClass); });
    },

    /**
    * Finds the data item by its value
    */
    _getItemByValue: function(value) {
        var ds = this.get_items();

        if (!ds) {
            return null;
        }

        for (var i = 0; i < ds.length; i++) {
            var item = ds[i];

            if (this._getValueOfItem(item) === value) {
                return item;
            }
        }

        return null;

    },

    _getDropDownItemByValue: function(value) {
        var selectedItem = this._getItemByValue(value);
        return this._getDropDownItem(selectedItem);
    },

    /**
    * Returns value of data item
    */
    _getValueOfItem: function(item) {
        if (item === this._notSelectedItem) {
            return null;
        }

        var valueProperty = this._valueProperty;

        if (valueProperty == "*") {
            return item;
        }

        var getter = 'get_' + valueProperty;

        if (isFunction(item[getter])) {
            return item[getter]();
        } else if (item[valueProperty]) {
            return item[valueProperty];
        } else {
            throw new Error("Cannot get value of item. Unknown property \"" + valueProperty + "\" is declared in options of a dropdown list");
        }
    },

    /**
    * Returns selected data item
    */
    getSelectedDataItem: function() {
        return this._getItemByValue(this.get_selectedValue());
    },

    /**
    * Sets the dataItem as selected
    */
    setSelectedDataItem: function(dataItem) {
        this.set_selectedValue(this._getValueOfItem(dataItem));
    },

    scrollToItem: function (dataItem) {
        var control = this._getDropDownItem(dataItem);

        if (!control) {
            this.dropDownPanel.scrollable.scrollToTop();
            return;
        }

        var offsetTop = control.domElement.offsetTop;
        this.dropDownPanel.scrollable.set_scrollY(offsetTop);
    },

    findDataItemByText: function (namePart) {
        var ds = this.get_items();
        var namePart = namePart.toLowerCase();

        for (var i = 0; i < ds.length; i++) {
            var itemName = this._getDataItemText(ds[i]).toString().toLowerCase();

            if (itemName.startsWith(namePart)) {
                return ds[i];
            }
        }

        return null;
    },

    set_items: function (value) {
        if (this._items === value) {
            return;
        }

        oldValue = this._items;

        if (this._items) {
            this.clear();
        }

        this._items = value;
        this._getItemsRepeater().set_dataSource(value);

        if (this._items) {
            this._items.add_changed(this._items_itemChanged, this);
        }

        this._updateSelectedItem();
        this.__cachedMaxWidth = null;
        this.dropDownPanel.set_width('*');
        this.raise_itemsChanged({ newValue: value, oldValue: oldValue });
    },

    _items_itemChanged: function (sender, args) {
        this.__cachedMaxWidth = null;
        this.dropDownPanel.set_width('*');
        this._updateSelectedItem();
    },

    _getMaxItemWidth: function() {
        if (this.__cachedMaxWidth) {
            return this.__cachedMaxWidth;
        }

        var controls = this._getItemsRepeater().controls;
        var maxWidth = 0;

        for (var i = 0; i < controls.length; i++) {
            var width = controls[i].get_domWidth();
            
            if (maxWidth < width) {
                maxWidth = width;
            } 
        }

        this.__cachedMaxWidth = maxWidth;

        return maxWidth;
    },

    isEmpty: function() {
        var items = this.get_items();
        return !items || items.length == 0;
    },

    supportsNotSelectedItem: function() {
        return !!this._notSelectedItem;
    },

    _shouldWaitForItem: function() {
        var selectedValue = this.get_selectedValue(),
            selectedDataItem = this.getSelectedDataItem();

        var shouldUseNotSelected = selectedValue === null && this.supportsNotSelectedItem();

        if (!isUndefined(selectedValue) && !shouldUseNotSelected && !selectedDataItem && !this.isEmpty()) {
            return true;
        }
        
        if (this._waitingForRightItem) {
            this._cancelWaitForItem();
        }
    },

    _waitForItem: function() {
        if (!this._waitingForRightItem) {
            this._isDisabledBefore = !this.get_enabled();
            this._waitingForRightItem = true;
            this.set_enabled(false);
        }

        if (this.supportsNotSelectedItem()) {
            this._useNotSelectedItem();
        } else if (!this.isEmpty()) {
            this._removeActiveItem();
        }
    },

    _cancelWaitForItem: function() {
        this.set_enabled(this._isDisabledBefore ? false : true);
        this._waitingForRightItem = false;
    },

    _shouldSetDefaultItem: function() {
        var selectedDataItem = this.getSelectedDataItem();

        return !selectedDataItem && this._autoSelectDefaultValue && !this.supportsNotSelectedItem() && !this.isEmpty();
    },

    _setDefaultItem: function() {
        var items = this.get_items();
        this.setSelectedDataItem(items.first());
    },

    _shouldUseNotSelectedItem: function() {
        var selectedDataItem = this.getSelectedDataItem();

        return !selectedDataItem && this.supportsNotSelectedItem();
    },

    _useNotSelectedItem: function() {
        this._setActiveItem(this._notSelectedItem);
        this._setSelectedStyleToDataItem(this._notSelectedItem);
    },
    
    _updateSelectedItem: function() {
        if (this._shouldWaitForItem()) {
            this._waitForItem();
            return;
        }
        
        if (this._shouldUseNotSelectedItem()) {
            this._useNotSelectedItem();
            return;
        }
        
        if (this._shouldSetDefaultItem()) {
            this._setDefaultItem();
            return;
        }

        var selectedDataItem = this.getSelectedDataItem();
        this._setSelectedStyleToDataItem(selectedDataItem);
        this._setActiveItem(selectedDataItem);
    },

    _setSelectedStyleToDataItem: function(dataItem) {
        var selectedDropDownItem = this._getDropDownItem(dataItem);

        if (selectedDropDownItem && selectedDropDownItem.hasCssClass(Phoenix.UI.DropDownList.selectedItemCssClass)) {
            return;
        }

        this._removeAllItemControlsSelectionClasses();
        if (selectedDropDownItem) {
            selectedDropDownItem.addCssClass(Phoenix.UI.DropDownList.selectedItemCssClass);
        }
    },

    /**
    * Removes currently displayed item
    */
    _removeActiveItem: function() {
        if (this._selectedItemControl) {
            this._selectedItemControl.free();
            this.controls.remove(this._selectedItemControl);
            this._selectedItemControl = null;
        }
    },

    /**
    * Display an dataItem as a selected item of the dropdown list
    */
    _setActiveItem: function(dataItem) {
        if (this._selectedItemControl && this._selectedItemControl.get_dataSource() === dataItem) {
            return;
        }
        
        this._removeActiveItem();

        if (dataItem) {
            var templateOptions = this._getTemplate(dataItem, false);
            templateOptions.margin = '0 0 18 0'; // item shouldn't be placed above dropdown button's image
            this._selectedItemControl = new Template(templateOptions).instantiate(dataItem, this);
            this.controls.add(this._selectedItemControl);
        }
    },

    /**
    * Opens the dropdown list
    */
    showDropDown: function () {
        if (this._isShowed || !this.get_enabled()) {
            return;
        }

        this.__oldSelectedValue = this._selectedValue;

        var $element = $(this.domElement);
        var pos = $element.offset();

        var listPosition = pos;
        listPosition.top += this.get_clientHeight() - 1;

        this.raise_onOpen({ listPosition: listPosition });

        var style = this.dropDownPanel.domElement.style;
        var maxWidth = this._getMaxItemWidth() + 28;

        if (this._stretchToMaxItem || maxWidth > this.get_innerWidth()) {
            this.dropDownPanel.set_width(maxWidth);
        }

        this.dropDownPanel.show();
        style.left = listPosition.left + "px";
        style.top = listPosition.top + "px";
        style.zIndex = "70000";

        this.addCssClass('dropDownList_active');

        if (!this._globalClickCallback) {
            this._globalClickCallback = function (ev) {
                if (this.dropDownPanel.domElement !== ev.target && !$.contains(this.dropDownPanel.domElement, ev.target)) {
                    this.hideDropDown();
                }
            } .bind(this);

            this._globalKeyPressCallback = function (ev) {
                if (ev.keyCode == '13' || ev.keyCode == '10' || ev.keyCode == '27' || ev.keyCode == '9') {
                    this.hideDropDown();
                    return;
                }

                var pressedKey = String.fromCharCode(ev.charCode || ev.keyCode);

                this._searchString = this._searchString || '';
                var item = this.findDataItemByText(this._searchString + pressedKey);

                if (item) {
                    this._searchString = this._searchString + pressedKey;
                    this.scrollToItem(item);
                    this.setSelectedDataItem(item);
                }

                if (this._searchClearTimer) {
                    window.clearTimeout(this._searchClearTimer);
                }

                this._searchClearTimer = window.setTimeout(function () {
                    this._searchString = '';
                } .bind(this), 500);

            }.bind(this);
        }

        window.setTimeout(function () {
            $(document.body).click(this._globalClickCallback); // !! we are in a click handler, if we wouldn't set timeout then clickCallback will be called at this line
            $(document).keyup(this._globalKeyPressCallback);
        } .bind(this), 200);

        this._isShowed = true;
        this.scrollToItem(this.getSelectedDataItem());
    },

    update: function() {
        if (this._stretchToMaxItem && !this.__free) {
            this.set_width(this._getMaxItemWidth() + 28);
        }

        Phoenix.UI.DropDownList.callBase(this, "update");
    },

    hideDropDown: function () {
        if (this._isShowed) {
            this.removeCssClass('dropDownList_active');
            var style = this.dropDownPanel.domElement.style;
            style.left = "-1000px";
            style.top = "-1000px";
            this.dropDownPanel.hide();
            this._isShowed = false;
            $(document.body).unbind('click', this._globalClickCallback);
            $(document).unbind('keyup', this._globalKeyPressCallback);
            this.raise_onClose();
            
            var oldValue = this.__oldSelectedValue;

            if (this._selectedItem !== oldValue) {
                this.raise_onChanged({ oldValue: oldValue, newValue: this.get_selectedValue() });
            }
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('div', domElement);

        $(this.domElement)
            .hover(
                function () {
                    if (this.get_enabled()) {
                        this.addCssClass('dropDownList_hover');
                    }
                } .bind(this),
                function () { this.removeCssClass('dropDownList_hover'); } .bind(this)
            )
            .click(function (event) {
                if (this._isShowed) {
                    this.hideDropDown();
                } else {
                    this.showDropDown();
                }
            } .bind(this));

        DOM.disableSelection(this.domElement);
        Phoenix.UI.DropDownList.callBase(this, "instantiateInDom", [domElement]);
    },

    clear: function () {
        if (this._items) {
            this._items.remove_changed(this._items_itemChanged, this);
        }

        this._items = null;
        this._getItemsRepeater().clear();
    },

    free: function () {
        this.clearEvents();
        this.clear();
        this.set_selectedValue(null);
        this.get_actionsBar().remove_changed(this.__actionsBarChanged, this);
        Phoenix.UI.DropDownList.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.DropDownList.prototype, [
    { name: 'selectedValue', autoEvent: true },
    { name: 'items', autoEvent: true },
    { name: 'actionsBar', autoEvent: true }
]);

Auto.Events(Phoenix.UI.DropDownList.prototype, [
    'onChanged',
    'onOpen',
    'onCommand',
    'onClose'
]);

Phoenix.UI.DropDownList.createClass('Phoenix.UI.DropDownList', Control);
ControlsFactory.registerControl('dropDownList', Phoenix.UI.DropDownList);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.FileUpload = function() {
    Phoenix.UI.FileUpload.constructBase(this);
    this._silverlightId = "FileUpload_" + Phoenix.UI.FileUpload.prototype._silverlightNextId;
    Phoenix.UI.FileUpload.prototype._silverlightNextId++;
};

Phoenix.UI.FileUpload.prototype = {
    _imageType: null,
    _silverlightId: null,
    _silverlightNextId: 1,

    defaultOptions: {
        height: '33px'
    },

    initFromOptions: function (options) {
        if (isFunction(options.onProgress)) {
            this.add_onProgress(options.onProgress, this);
        }

        if (isFunction(options.onComplete)) {
            this.add_onComplete(options.onComplete, this);
        }

        _imageType = options.imageType ? ImageType[options.imageType] : ImageType.user;

        Phoenix.UI.FileUpload.callBase(this, "initFromOptions", [options]);
    },

    _onSlLoad: function (service) {
        var oldEnabled = true;
        delete Silverlight[this._silverlightId + "Load"];

        service.ImageType = _imageType._value;

        service.OnProgress = function (sender, result) {
            var progress = result.Progress;

            if (progress == 0) {
                oldEnabled = this.get_enabled();
                this.set_enabled(false);

                var style = this._slContainer.style;
                style.position = "absolute";
                style.left = "-1000px";
                style.top = "-1000px";
            }

            this.set_text('Uploading... ' + progress + '%');
            this.raise_onProgress({ progress: progress });
        } .bind(this);

        service.OnComplete = function (sender, result) {
            this.update();
            this.set_text(this.options.text || '');
            this.set_enabled(oldEnabled);

            this.raise_onComplete({ id: result.ImageId });
        } .bind(this);
    },

    instantiateInDom: function (domElement) {
        Phoenix.UI.FileUpload.callBase(this, "instantiateInDom", [domElement]);

        this._slContainer = DOM.create("div", domElement);
        var style = this._slContainer.style;
        style.position = "absolute";
        style.overflow = "auto";
        //style.left = "-1000px";
        //style.top = "-1000px";
        //style.width = 100;
        //style.height = 100;
        //$(this._slContainer).css("color", "#FF0000");
        //$(this._slContainer).html("!test!");

        Silverlight[this._silverlightId + "_Load"] = this._onSlLoad.bind(this);

        Silverlight.createObject(
        	Application.resolveUrl('silverlight:Phoenix.FileUpload.xap'),
        	this._slContainer,
        	null,
        	{
        	    version: "4.0.50401.0",
        	    isWindowless: 'true',
        	    background: '#00FFFFFF'
        	},
            {
                onError: function () { }
            },
            "id=" + this._silverlightId
        );

        this._silverlightUploader = this._slContainer.firstChild;
    },

    clearUpdateStyleTimer: function() {
        if (this._updateStyleTimer) {
            clearTimeout(this._updateStyleTimer);
            this._updateStyleTimer = null;
        }
    },

    createUpdateStyleTimer: function() {
        this.clearUpdateStyleTimer();
        this._updateStyleTimer = setTimeout(this._updateStyle.bind(this), 100);
    },

    updateDom: function (sender) {
        var element = this.domElement;
        var offset = $(element).position();
        var style = this._slContainer.style;
        style.zIndex = 900000;
        style.left = offset.left + "px";
        style.top = offset.top + "px";

        this.createUpdateStyleTimer();

        var uploaderStyle = this._silverlightUploader.style;
        uploaderStyle.width = (element.offsetWidth + 1) + "px";
        uploaderStyle.height = element.offsetHeight + "px";
        uploaderStyle.overflow = "auto";

        Phoenix.UI.FileUpload.callBase(this, "updateDom", [sender]);
    },

    _updateStyle: function () {
        var element = this.domElement;
        var offset = $(element).position();
        var style = this._slContainer.style;
        style.zIndex = 900000;
        style.left = offset.left + "px";
        style.top = offset.top + "px";
        
        var uploaderStyle = this._silverlightUploader.style;
        uploaderStyle.width = (element.offsetWidth + 1) + "px";
        uploaderStyle.height = element.offsetHeight + "px";
        uploaderStyle.overflow = "auto";
    },

    free: function () {
        DOM.remove(this._silverlightUploader);
        this._silverlightUploader = null;

        DOM.remove(this._slContainer);
        this._slContainer = null;
        this.clearUpdateStyleTimer();

        Phoenix.UI.FileUpload.callBase(this, "free");
    }
};

Auto.Events(Phoenix.UI.Button.prototype, [
    'onProgress',
    'onComplete'
]);

Phoenix.UI.FileUpload.createClass('Phoenix.UI.FileUpload', Phoenix.UI.Button);
ControlsFactory.registerControl('fileUpload', Phoenix.UI.FileUpload);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.PhotoUpload = function() {
    Phoenix.UI.PhotoUpload.constructBase(this);
};

Phoenix.UI.PhotoUpload.prototype = {        
    initFromOptions: function(options) {

        var thisObj = this;

        if (isFunction(options.onProgress)) {
            this.add_onProgress(options.onProgress, this);
        }
    
        if (isFunction(options.onComplete)) {
            this.add_onComplete(options.onComplete, this);
        }

        if (isFunction(options.onShotClicked)) {
            this.add_onShotClicked(options.onShotClicked, this);
        }

        if (isFunction(options.onFatalError)) {
            this.add_onFatalError(options.onFatalError, this);
        }
        
        options.controls = [
            {
                type: 'panel',
                id: 'panel',
                width: '320',
                layout: 'stack',
                orientation: 'vertical',
                halign: 'center',
                controls: [
                    {
                        id: 'photoSilverlight',
                        type: 'silverlight',
                        url: 'silverlight:Phoenix.PhotoUpload.xap',
                        width: '320',
                        height: '240',
                        isWindowless: ($.browser.firefox ? 'false' : 'true'),
                        onSlLoad: function (sender, args) {
                            var content = this.get_content();

                            content.Service.OnComplete = function (sender, args) {
                                this.get_content().Content.DeInit();
                                this.parent.parent.raise_onComplete({ id : args.ImageId });
                            }.bind(this);

                            content.Service.OnProgress = function (sender, args) {                                
                                this.parent.parent.raise_onProgress({ progress: args.Progress });
                            }.bind(this);

                            content.Content.ReadyToShot = function () {
                                this.parent.shotButton.set_enabled(true);
                            }.bind(this);

                            content.Content.CameraNotFound = function () {                                
                                this.parent.parent.raise_onFatalError({ caption: 'Camera not found!', text: 'Please connect your camera' });
                            }.bind(this);
                                                                              
                            content.Content.Init();
                        }
                    },
                    {
                        type: 'button',
                        id: 'shotButton',
                        text: 'Shot',
                        margin: '10 5 0 0',
                        enabled: false,
                        onClick: function (sender, args) {      
                            this.parent.parent.set_shotWasClicked(true);                      
                            this.parent.parent.raise_onShotClicked();
                            this.parent.photoSilverlight.get_content().Content.Shot();                            
                        }
                    }
                ]
            }            
        ];
                
        Phoenix.UI.PhotoUpload.callBase(this, "initFromOptions", [ options ]);        
    },

    init: function() {
        this.set_shotWasClicked(false);
        this.panel.shotButton.set_enabled(false);
        
        if ($.browser.msie) {
            this.panel.photoSilverlight.reload();
        }
    },

    deInit: function() {        
        if (!this.get_shotWasClicked())
        {
            this.panel.photoSilverlight.get_content().Content.DeInit();
        }
    }
};

Auto.Properties(Phoenix.UI.PhotoUpload.prototype, [
    { name: 'shotWasClicked' }
]);

Auto.Events(Phoenix.UI.Panel.prototype, [
    'onProgress',
    'onComplete',
    'onShotClicked',
    'onFatalError'    
]);

Phoenix.UI.PhotoUpload.createClass('Phoenix.UI.PhotoUpload', Phoenix.UI.Panel);
ControlsFactory.registerControl('photoUpload', Phoenix.UI.PhotoUpload);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Image = function() {
    Phoenix.UI.Image.constructBase(this);
};

Phoenix.UI.Image.prototype = {
	_src: null,

    defaultOptions: {
        bindings: {
            '*': 'src'
        }
    },
	
	get_src: function() { return this._src; },
	set_src: function(value)
	{
	    if (this._src === value) {
	        return;
	    }
	
	    this._src = Application.resolveUrl(value);
	    
	    if (this.domElement) {
	        this._loadImg(this._src);
	    }
	},
	
	refresh: function()
	{
	    var separator = this._src.indexOf('?') >= 0 ? '&' : '?';
	    
	    if (this.domElement) {
	        this._loadImg(this._src + separator + Math.random());
	    }
	},
	
    initFromOptions: function(options) {
        Phoenix.UI.Image.callBase(this, "initFromOptions", [ options ]);
        
        this._src = Application.resolveUrl(options.src);
    },
    
	instantiateInDom: function(domElement) {
        this.domElement = DOM.create("img", domElement);
        this._loadImg(this.get_src());
        
        Phoenix.UI.Image.callBase(this, "instantiateInDom", [ domElement ]);
	},
	
	_loadImg: function(src) {
	    if (!this.domElement) {
	        return;
	    }
	    
	    var element = this.domElement;
        var $element = $(element);
        $element.removeClass("loading_error");
		
		$element.css({
		    width: this.get_innerWidth(),
		    height: this.get_innerHeight()
		});

		var img = DOM.create('img');
		
		$(img).css({
		    position: "absolute",
		    left: -1000,
		    top: -1000,
		    width: 1,
		    height: 1
		})
		.bind("load", function()
		{
            $element.removeClass("loading");
            element.src = this.src;
	        $(this).unbind("load");
            
            DOM.remove(this);
		})
		.bind("error", function() { $element.addClass("loading_error"); DOM.remove(this); });
        
        element.src = Application.get_configuration().get_blankImageUrl();
        $element.addClass("loading");
        
        document.body.appendChild(img);
		img.src = src;	    
	}
};

Phoenix.UI.Image.createClass('Phoenix.UI.Image', Control);
ControlsFactory.registerControl('image', Phoenix.UI.Image);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Label = function() {
    Phoenix.UI.Label.constructBase(this);
};

Phoenix.UI.Label.prototype = {
    defaultOptions: {
        width: '?',
        height: '?',
        bindings: {
            '*': 'text'
        },
        multiline: false
    },

    initFromOptions: function (options) {
        this._emptyText = options.emptyText || "";
        this._format = options.format;
        this._hideIfEmpty = !!options.hideIfEmpty;
        this._multiline = options.multiline;

        if (isDefined(options.text) && isUndefined(this._text)) {
            this.set_text(options.text);
        }

        Phoenix.UI.Label.callBase(this, "initFromOptions", [options]);
        this._checkIsEmpty();
    },

    _updateText: function() {
        this._checkIsEmpty();
        var text = this.get_text();

        if (this._format) {
            text = this._format.format(text);
        }

        if (this._multiline) {
            text = this._prepareMultiline(text);

            if (this.domElement) {
                this.domElement.innerHTML = text;
                this.update();
            }
        } else {
            if (this.domElement) {
                this.domElement.firstChild.data = text;
                this.update();
            }
        }
    },

    _checkIsEmpty: function() {
        if (this._hideIfEmpty) {
            if (!this._text) {
                this.hide();
            } else {
                this.show();
            }
        }
    },

    set_text: function (value) {
        this._text = value;
        this._updateText();
    },

    get_text: function () {
        return isNullOrUndefined(this._text) || this._text === '' ? this._emptyText : this._text;
    },

    _prepareMultiline: function (text) {
        text = text.replaceAll("\n", "<br />");
        text = text.replaceAll("\u000d\u000a", "<br />");

        return text;
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div");
        var text = this._format ? this._format.format(this.get_text()) : this.get_text();

        if (this._multiline) {
            text = this._prepareMultiline(text);
            this.domElement.innerHTML = text;
        } else {
            var textNode = document.createTextNode(text);
            this.domElement.appendChild(textNode);
        }

        domElement.appendChild(this.domElement);
        Phoenix.UI.Label.callBase(this, "instantiateInDom", [domElement]);
    },

    updateDom: function(sender) {
        if (this.options.width || this.options.height) {
            if (sender || !this.parent) {
                var element = this.domElement, width = this.get_clientWidth(), height = this.get_clientHeight();
                DOM.setBoundingRect(element, width, height);
            }
        }
            
        Phoenix.UI.Label.callBase(this, "updateDom", [ sender ]);
    }
};

Phoenix.UI.Label.createClass('Phoenix.UI.Label', Control);
ControlsFactory.registerControl('label', Phoenix.UI.Label);

Auto.Properties(Phoenix.UI.Label.prototype, [
    { name: 'text' },
    { name: 'format'}
]);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Link = function() {
    Phoenix.UI.Link.constructBase(this);
};

Phoenix.UI.Link.prototype = {
    _url: null,
    _text: null,
    
    defaultOptions: {
        disabledCssClass: 'link_disabled',
        url: 'javascript:void(0);',
        text: '',
        width: '?',
        height: '?',  
        bindings: {
            '*' : 'text'
        }
    },
    
    initFromOptions: function(options) {
        Phoenix.UI.Link.callBase(this, "initFromOptions", [ options ]);
        
        this._url = options.url || "";
        this._text = options.text || "";
        
        if (typeof(options.onClick) == 'function') {
            this.add_onClick(options.onClick, this);
        }
        
        if (typeof(options.onDblClick) == 'function') {
            this.add_onDblClick(options.onDblClick, this);
        }
    },
    
    set_text: function(value) {
        if (this.domElement) {
            $(this.domElement).text(value);
            this.update();
        }
        
        this._text = value;
    },
    
    set_url: function(value) {           
        if (this.domElement) {
            this.domElement.href = Application.resolveUrl(value);
        }
        
        this._url = value;
    },
    
    /**
     * Instantiate this control into DOM
     */
    instantiateInDom: function(domElement) {
        var element = DOM.create("a", domElement);
        var $element = $(element);
        
        this.domElement = element;
        
        $element
            .attr('href', Application.resolveUrl(this._url))
            .bind('click', function() {
                if (this._enabled) {
                    this.raise_onClick()
                }
            }.bind(this))
            .bind('dblclick', function() {
                if (this._enabled) {
                    this.raise_onDblClick()
                }
            }.bind(this));
            
        if (this.controls.getControlsInFlow().length == 0) {
            $element.text(this._text || '');
        }
                
        Phoenix.UI.Link.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Properties(Phoenix.UI.Link.prototype, [
    'text',
    'url'
]);

Auto.Events(Phoenix.UI.Link.prototype, [
    'onClick',
    'onDblClick'
]);

Phoenix.UI.Link.createClass('Phoenix.UI.Link', Control);
ControlsFactory.registerControl('link', Phoenix.UI.Link);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.ListView = function() {
    Phoenix.UI.ListView.constructBase(this);
};

Phoenix.UI.ListView.prototype = {
    defaultOptions: {
        disabledCssClass: 'listview_disabled',
        width: '*',
        height: '*',  
        bindings: {
            '*' : 'selectedValue'
        }
    },

    set_dataSource: function(value) {
        Phoenix.UI.ListView.callBase(this, "set_dataSource", [ value ]);
    },

    getChildControlByValue: function(value) {
        return this._getChildControlByDataItem(this._getDataItemByValue(value));
    },

    _getChildControlByDataItem: function(dataItem) {
        if (!dataItem) {
            return null;
        }

        return this._getRepeater().findByDataItem(dataItem);
    },

    _getRepeater: function() {
        return this._scrollable._repeater;
    },

    _setSelectedItem: function(dataItem) {
        var selectedValue = this._getValueOfItem(dataItem);
        this.set_selectedValue(selectedValue);
    },

    _getValueOfItem: function(dataItem) {
        return dataItem;
    },

    _getDataItemByValue: function(value) {
        for (var i = 0; i < this._items.length; i++) {
            if (this._getValueOfItem(this._items[i]) == value) {
                return this._items[i];
            }
        }

        return null;
    },

    set_selectedValue: function(value) {
        if (this._selectedValue === value) {
            return;
        };

        var oldValue = this._selectedValue;
        this._selectedValue = value;

        var oldControl = this.getChildControlByValue(oldValue);
        var newControl = this.getChildControlByValue(value);

        if (oldControl) {
            oldControl.removeCssClass('item_selected');
        }

        if (newControl) {
            newControl.addCssClass('item_selected');
        }

        this.raise_selectedValueChanged({ newValue: value, oldValue: oldValue });
    },
    
    initFromOptions: function(options) {
        var thisObj = this;
        this.addCssClass('listview');

        if (isFunction(options.onItemClick)) {
            this.add_onItemClick(options.onItemClick, this);
        }

        options.template.domHandlers = {
            'mouseover': function() {
                this.addCssClass('item_hovered');
            },
            'mouseout': function() {
                this.removeCssClass('item_hovered');
            },
            'click': function() {
                var ds = this.get_dataSource();
                thisObj._setSelectedItem(ds);
                thisObj.raise_onItemClick({ item: ds, control: this });
            }
        };

        options.controls = [
            {
                id: '_scrollable',
                type: 'scrollablePanel',
                controls: [
                    {
                        id: '_repeater',
                        type: 'repeater',
                        height: '?',
                        template: options.template,
                        onLoad: function() {
                            this._link = Auto.Property.CreateOneWayLink(thisObj, this, "items", "dataSource");
                        },
                        onFree: function() {
                            this._link.dispose();
                        }
                    }
                ]
            }
        ];

        Phoenix.UI.ListView.callBase(this, "initFromOptions", [ options ]);
    }
};

Auto.Properties(Phoenix.UI.ListView.prototype, [
    { name: 'selectedValue', autoEvent: true },
    { name: 'items', autoEvent: true }
]);

Auto.Events(Phoenix.UI.ListView.prototype, [
    'onItemClick'
]);

Phoenix.UI.ListView.createClass('Phoenix.UI.ListView', Phoenix.UI.Panel);
ControlsFactory.registerControl('listView', Phoenix.UI.ListView);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Literal = function() {
    Phoenix.UI.Literal.constructBase(this);
};

Phoenix.UI.Literal.prototype = {
    _type: null,
    _text: null,

    defaultOptions: {
        'width': '?',
        height: '?'
    },

    initFromOptions: function(options) {
        Phoenix.UI.Literal.callBase(this, "initFromOptions", [ options ]);
        this._text = options.text || "";
        this._type = isNullOrUndefined(options.dataType) ? 'text' : options.dataType;
    },
    
    set_text: function(value) {           
        if (this.domElement) {
            if (this._type == 'text') {
                this.domElement.data = value;
            } else {
                var domElement = this._getDomElementForHtmlValue(value);
                $(this.domElement).replaceWith(domElement);
                this.domElement = domElement;
            }
        }
        
        this._text = value;
    },
    
    _getDomElementForHtmlValue: function(value) {
        if (!value) {
            value = '<span style="display: none"></span>';
        }

        var $data = $(value);
        var domElement;
            
        if ($data.length > 1) {
            domElement = DOM.create("span");
            domElement.innerHTML = value;
        } else {
            domElement = $data.get(0);
        }

        return domElement;
    },

    instantiateInDom: function(domElement) {
        if (this._type == 'text')
        {
            var textNode = document.createTextNode(this._text || '');
            this.domElement = textNode;
    
            domElement.appendChild(textNode);
        } else if (this._text) {
            this.domElement = this._getDomElementForHtmlValue(this._text);
            domElement.appendChild(this.domElement);
        } else {
            this.domElement = DOM.create("span", domElement);
        }
        
        Phoenix.UI.Literal.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Properties(Phoenix.UI.Literal.prototype, [
    { name: 'text' }
]);

Phoenix.UI.Literal.createClass('Phoenix.UI.Literal', Control);
ControlsFactory.registerControl('literal', Phoenix.UI.Literal);

/**
 * Page class
 
 * @inherits Control
 * @constructor
 */
Page = function() {
    Page.constructBase(this);
    this._params = {};
    this._paramsIdx = {}; // indexes of param. It's used for right ordering of parameters. Last changed params should be at the end of uri.
    this._lastParamIdx = 0;
};

Page.prototype = {
    _isLoaded: false,
    _loadingXhr: null,
    _isLoading: false,
    
    // ======== Properties ========
    get_title: function(){
        return this.options.title;
    },
    
    get_isLoaded: function() {
        return this._isLoaded;
    },

    isSameUri: function(uri) {
        if (isNullOrUndefined(uri)) {
            return false;
        }

        if (isNullOrUndefined(this._fullUri)) {
            return this._uri === uri;
        }

        var nav = PageUri.parseUri(this._fullUri);
        var nav2 = PageUri.parseUri(uri);

        return nav.uri == nav2.uri;
    },

    __update_fullUri: function() {
        var oldFullUri = this._fullUri;
        var uri = PageUri.buildUri(this.get_uri(), this._params, function(a, b) {
            return this._paramsIdx[a] - this._paramsIdx[b];
        }.bind(this));

        if (uri === oldFullUri) {
            return;
        }

        this._fullUri = uri;
        this.raise_fullUriChanged({ newValue: uri, oldValue: oldFullUri });
    },

    sendMessage: function(msg) {
        this.raise_onMessage({ message: msg });
    },

    update_fullUri: function() {
        if (this.__updateUriTimeOut) {
            clearTimeout(this.__updateUriTimeOut);
        }

        this.__updateUriTimeOut = setTimeout(function() {
            this.__update_fullUri();
            this.__updateUriTimeOut = null;
        }.bind(this), 500);
    },

    set_fullUri: function(fullUri) {
        var nav = PageUri.parseUri(fullUri);
        this._uri = nav.uri;
        var names = [];

        for (var paramName in nav.params) {
            if (nav.params.hasOwnProperty(paramName)) {
                names.add(paramName);
            }
        }

        names.sort(function(a, b) { return nav.paramsIdx[a] - nav.paramsIdx[b]; })

        for (var i = 0; i < names.length; i++) {
            var paramName = names[i];
            this.set_param(paramName, nav.params[paramName]);
        }

        this.update_fullUri();
    },

    get_fullUri: function() {
        return this._fullUri;
    },
    
    get_param: function(key) {
        return this._params[key];
    },
    
    set_param: function(key, value) {
        var oldValue = this._params[key];

        if (oldValue === value) {
            return;
        }

        this._lastParamIdx++;
        this._params[key] = value;
        this._paramsIdx[key] = this._lastParamIdx;
        this.update_fullUri();
        this.raise_onParamChanged({key: key, newValue: value, oldValue: oldValue});
        this._set_sessionParam(key, value);
    },

    _set_sessionParam: function(key, value) {
        if (window.sessionStorage) {
           sessionStorage.setItem(this.get_uri() + ':' + key, value + '');
        }
    },
    
    loadParamsFromSession: function() {
        if (window.sessionStorage) {
            var pageUri = this.get_uri();
            for (var i = 0; i < sessionStorage.length; i++) {
                var storageKey = sessionStorage.key(i);

                var separatorIndex = storageKey.indexOf(':');
                if (separatorIndex == -1)
                    continue;

                var storedPageUri = storageKey.substring(0, separatorIndex);
                if (storedPageUri != pageUri)
                    continue;

                var paramKey = storageKey.substring(separatorIndex + 1);
                this.set_param(paramKey, sessionStorage.getItem(storageKey));
            }
        }
    },

    defaultOptions: {
        'width': '100%',
        height: '100%'
    },

    get_resource: function(resourceName) {
        return this.options.resources ? this.options.resources[resourceName] : null;
    },

    // ======== Methods ========
    initFromOptions: function(pageOptions) {
        if (typeof(pageOptions.onParamChanged) == 'function') {
            this.add_onParamChanged(pageOptions.onParamChanged, this);
        }

        this._resources = pageOptions.resources || {};

        Page.callBase(this, "initFromOptions", [ pageOptions ]);
        this._isLoaded = true;

        for (var i = 0; i < this._params.length; i++) {
            var param = this._params[i];
            this.raise_onParamChanged({key: param.key, value: param.value});
        }
    },
    
    initFromUri: function(pageUri) {
        this.abortLoading();
        this.set_fullUri(pageUri);

        if (!Application.pageIsAvailable(this.get_uri())) {
            pageUri = Application.get_error403Uri();
            this.set_fullUri(pageUri);
        }       
        
        this._isLoading = true;
        this._loadingXhr = Services.PagesService.getPage(this.get_uri(),
            function(data) {
                this._isLoading = false;
                this.loadParamsFromSession();
                this.initFromOptions(data);
            }.bind(this),
            function(request, errorType, errorThrown) {
                this._isLoading = false;
                var error = errorThrown;
                
                if (!error) {
                    try {
                        error = eval(request.responseText).Message;
                    }
                    catch(e) {
                        error = errorType + "\r\n" + request.responseText;
                    }
                }
                
                Application.throwError(errorThrown || error);
            }.bind(this)
        );
    },
    
    abortLoading : function() {
        if (this._isLoading && this._loadingXhr) {
            this._loadingXhr.abort();
        }
        
        this._isLoading = false;
        this._uri = null;
    },
    
    attachDataHandlers: function() {
        if (!this.options)
            return;

        var window = this;
        var data = window ? window.get_data() : null;

        if(data)
            this._attachForwardHandlers(data, true);
    },

    detachDataHandlers: function() {
        var window = this;

        if (window) {
            var data = window.get_data();
            if(data)
                this._detachForwardHandlers(data, true);
        }
    },

    free: function() {
        this.abortLoading();
        Page.callBase(this, "free");
    }
};

Auto.Events(Page.prototype, [
    'onParamChanged',
    'onMessage'
]);

Auto.Properties(Page.prototype, [
    { name: 'fullUri', autoEvent: true },
    { name: 'uri', autoEvent: true },
    { name: 'params' }
]);

Page.createClass('Page', Phoenix.UI.Panel);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Frame = function() {
    Phoenix.UI.Frame.constructBase(this);
};

Phoenix.UI.Frame.prototype = {
    defaultOptions: {
        bindings: {
            '*': '*'
        }
    },

    _uri: null,
    _page: null,

    _createPage: function() {
        this._disposePage();
            
        if (this._uri) {
            var navObj = Application.getPageNavigationObject(this._uri);
            var page = new Page(navObj.params);
		    this._page = page;
            
            page.add_initComplete(function() {
                this.controls.add(page);
            }, this);
		    
		    page.initFromUri(navObj.uri);
        }
    },

    _disposePage: function() {
        if (this._page) {
            this.controls.remove(this._page);
            this._page.free();
        }
    },

    initFromOptions: function (options) {
        this.addCssClass('frame_control');
        this._uri = options.uri;

        Phoenix.UI.Frame.callBase(this, "initFromOptions", [options]);
    },

    set_uri: function (value) {
        if (this._uri === value) {
            return;
        }

        this._uri = value;
        this._disposePage();

        if (value) {
            this._createPage();
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div", domElement);
        this._createPage();
        Phoenix.UI.Frame.callBase(this, "instantiateInDom", [domElement]);
    },

    free: function() {
        _page = null;
        Phoenix.UI.Frame.callBase(this, "free");
    }
};

Phoenix.UI.Frame.createClass('Phoenix.UI.Frame', Control);
ControlsFactory.registerControl('frame', Phoenix.UI.Frame);

Auto.Properties(Phoenix.UI.Frame.prototype, [
    { name: 'uri' }
]);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Form = function() {
    Phoenix.UI.Form.constructBase(this);
};

Phoenix.UI.Form.prototype = {
    initFromOptions: function (options) {
        this.addCssClass('form_control');
        this._labelWidth = options.labelWidth || '30%';
        options.vspacing = isDefined(options.spacing) ? options.spacing : 2;
        options.hspacing = 5;

        options.layout = 'grid';
        options.columns = [ this._labelWidth, '*' ];
        options.rows = [];

        var controls = [];

        for (var i = 0; i < options.controls.length; i++) {
            var control = options.controls[i];
            control['grid.column'] = 2;
            control['grid.row'] = i + 1;
            control.width = control.width || '*';

            options.rows.add(control['height'] || '?');

            if (control['form.label']) {
                var label = this._createLabel(control['form.label'], control['id']);
                label['grid.column'] = 1;
                label['grid.row'] = i + 1;
                controls.add(label);
            }
        }

        options.controls.add(controls);

        Phoenix.UI.Form.callBase(this, "initFromOptions", [options]);
    },

    hideRow: function(relatedId) {
        var label = this['label_' + relatedId];
        this[relatedId].set_height(0);
        label.set_height(0);
        var rowId = label.options['grid.row'];
        this.get_layoutEngine().get_rows()[rowId - 1] = 0;
        this.get_layoutEngine().updateRowsAndColumns();
        this.update();
    },

    focus: function() {
        var firstControl = this.findControl(function(control) {
            return control.isFocusable();
        });
        
        if (firstControl) {
            firstControl.focus();
        }
    },

    _createLabel: function(text, relatedId) {
        var thisObj = this;

        return {
            id: relatedId ? 'label_' + relatedId : undefined,
            type: 'label',
            width: '*',
            valign: 'middle',
            height: '18',
            cssClass: 'form_label',
            text: text
        };
    }
};

Phoenix.UI.Form.createClass('Phoenix.UI.Form', Phoenix.UI.Panel);
ControlsFactory.registerControl('form', Phoenix.UI.Form);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Pager = function() {
    Phoenix.UI.Pager.constructBase(this);
};

Phoenix.UI.Pager.prototype = {
    defaultOptions: {
        'height': '24px',
        'width': '*',
        'padding': '5 3 2 2',
        'border': '1'
    },

    initFromOptions: function(options) {
        var thisObj = this;
        
        this.addCssClass('pager_control');

        options.orientation = 'horizontal';
        options.valign = 'middle';

        this.add_onFree(function() {
            this.clearPagingTimeOut();
        }, this);
        
        options.customFunctions = {
            '_setCount': function(result) {
                var sender = this.get_dataSource();
                this.pageSize = sender.get_pageSize() || 10;
                this.pagesCount = Math.ceil(result / this.pageSize);
                this._updatePaging();

                if (this.get_page() > this.pagesCount) {
                    this.set_page(1);
                }
            },

            'clearPagingTimeOut': function() {
                if (this._updatePagingTimeOut) {
                    clearTimeout(this._updatePagingTimeOut);
                    this._updatePagingTimeOut = null;
                }
            },

            '_updatePaging': function() {
                var ds = this.pagesNavigation.get_dataSource(),
                    pagesCount = this.pagesCount,
                    page = this.get_dataSource().get_page()*1;

                this.set_page(page);

                if (!pagesCount || !page) {
                    if (ds) {
                        ds.clear();
                    }

                    return;
                }

                if (ds) {
                    ds.clear();
                } else {
                    ds = [].makeObservable();
                    this.pagesNavigation.set_dataSource(ds);
                }
                
                var pages = [];

                pages.add({ text: '< Prev', enabled: page > 1, delta: -1 });
                pages.add({ text: 'Next >', enabled: page < pagesCount, delta: 1 });

                var leftIdx = Math.max(page - 2, 1),
                    rightIdx = Math.min(page + 2, pagesCount);

                if (leftIdx > 1) {
                    pages.add(1);
                }

                if (leftIdx == 3) {
                    pages.add(2);
                } else if (leftIdx > 2) {
                    pages.add({ separator: true });
                }

                for (var i = leftIdx; i <= rightIdx; i++) {
                    pages.add(i);
                }

                if (rightIdx == pagesCount - 2) {
                    pages.add(pagesCount - 1);
                } else if (rightIdx < pagesCount - 1) {
                    pages.add({ separator: true });
                }

                if (rightIdx < pagesCount) {
                    pages.add(pagesCount);
                }

                ds.add(pages);
                
                var curSelectedPage = this.pagesNavigation.findByDataItem(page);

                if (curSelectedPage) {
                    curSelectedPage.addCssClass('pager_link_selected');
                };
            }
        };
        
        var newBindings = {
            '*': function(sender, args) {
                args.newValue.set_page(this.get_page());
            },
            'count': function(sender, args) {
                this._setCount(args.newValue);
            },
            'page': function(sender, args) {
                this.clearPagingTimeOut();
                this._updatePagingTimeOut = setTimeout(this._updatePaging.bind(this), 0);
            }
        };

        if (options.bindings) {
            Object.extend(options.bindings, newBindings);
        } else {
            options.bindings = newBindings;
        }
        
        options.controls = [
            {
                id: 'pagesNavigation',
                type: 'repeater',
                height: '18',
                width: '*',
                orientation: 'horizontal',
                cssClass: 'pager_nav',
                templateSelector: function(dataItem) {
                    if (isNumber(dataItem)) {
                        return {
                            bindings: {
                                '*': function (sender, args) {
                                    var value = args.newValue;
                                    this.set_text(value);
                                    this.removeCssClass('pager_link_selected');
                                }
                            },
                            type: 'link',
                            height: '*',
                            cssClass: 'pager_link',
                            margin: '0 0 3 0',
                            onClick: function() {
                                thisObj._pagingCommand(this.get_dataSource());
                            }
                        }
                    }

                    if (dataItem.separator) {
                        return {
                            type: 'label',
                            bindings: {
                                '*': undefined
                            },
                            text: '...',
                            height: '*',
                            width: 15,
                            margin: '0 0 3 0'
                        }
                    }

                    return {
                        bindings: {
                            'text': 'text',
                            'enabled': 'enabled'
                        },
                        type: 'link',
                        height: '*',
                        cssClass: 'pager_link',
                        margin: '0 0 10 0',
                        onClick: function() {
                            var ds = thisObj.get_dataSource();

                            if (ds) {
                                thisObj._pagingCommand(ds.get_page() + this.get_dataSource().delta);
                            }
                        }
                    }
                }
            }
        ];
        
        Phoenix.UI.Pager.callBase(this, "initFromOptions", [ options ]);
    },

    set_page: function(page) {
        var oldPage = this._page,
            ds = this.get_dataSource();

        if (oldPage === page) {
            return;
        }

        this._page = page;

        if (ds) {
            ds.set_page(page);
        }

        this.raise_pageChanged({ newValue: page, oldValue: oldPage });
    },
    
    _pagingCommand: function(pageNum) {
        this.set_page(pageNum);
    }
};

Auto.Properties(Phoenix.UI.Pager.prototype, [
    { name: 'page', autoEvent: true, defaultValue: 1 }
]);

Phoenix.UI.Pager.createClass('Phoenix.UI.Pager', Phoenix.UI.Panel);
ControlsFactory.registerControl('pager', Phoenix.UI.Pager);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Popup = function() {
    Phoenix.UI.Popup.constructBase(this);
    this._fadeSpeed = $.browser.msie ? 0 : 'fast';
};

Phoenix.UI.Popup.prototype = {
    _uri: null,
    _page: null,
    _buttons: null,
    _scrolling: null,
    _fadeSpeed: null,
    container: null,

    defaultOptions: {
        'visible': false
    },
    
    staticVars: {
        level: 0
    },

    focus: function() {
        var firstControl = this.findControl(function(control) {
            return control.isFocusable();
        });
        
        if (firstControl) {
            firstControl.focus();
        }
    },

    initFromOptions: function(options) {
        var thisObj = this;
        this._title = options.title || '';
        this._inDocumentFlow = false;
        var isAutoSizeHeight = options.height == '?';
        var isAutoSizeWidth = options.width == '?';
        this._scrolling = options.scrolling && (!isAutoSizeHeight || options.maxHeight);

        this.attachEventsFromOptions(options, ["onCommand", "onOpen", "onClose", "onUriLoaded"]);

        var oldControls = options.controls || [];

        options.cssClass = options.cssClass || 'popup';
        options.controls = [
            {
                id: 'innerBox', 
                type: 'box',
                width: isAutoSizeWidth ? '?' : '*',
                height: isAutoSizeHeight ? '?' : '*',
                onLoad: function() {
                    this._innerWidthDelta = 17;
                },
                controls: [
                    {
                        type: 'panel',
                        height: isAutoSizeHeight ? '?' : '*',
                        width: isAutoSizeWidth ? '?' : '*',
                        orientation: 'vertical',
                        id: 'innerPanel',
                        controls: [
                            {
                                type: 'container',
                                tag: 'h1',
                                height: '26px',
                                width: '100%',
                                padding: '4px',
                                margin: '0 0 0 1',
                                cssClass: 'popup_header',
                                orientation: 'horizontal',
                                onLoad: function() {
                                    DOM.disableSelection(this.domElement);
                                    thisObj._attachDragHandlers(this.domElement);
                                },
                                onFree: function() {
                                    thisObj._detachDragHandlers(this.domElement);
                                },
                                controls: [
                                    {
                                        type: 'label',
                                        width: '*',
                                        height: '*',
                                        text: this._title,
                                        onLoad: function() {
                                            thisObj.add_titleChanged(function(sender, args) {
                                                this.set_text(args.newValue);
                                            }, this)
                                        }
                                    },
                                    {
                                        type: 'link',
                                        width: '15',
                                        height: '15',
                                        focusable: false,
                                        text: 'Close popup',
                                        onClick: function() {
                                            this._sendCloseCommand();
                                            this.close();
                                        }.bind(this)
                                    }
                                ]
                            },
                            {
                                type: this._scrolling ? 'scrollablePanel' : 'panel',
                                id: 'popupContainer',
                                cssClass: 'popupContainer',
                                width: isAutoSizeWidth ? '?' : '*',
                                height: isAutoSizeHeight ? '?' : '*',
                                maxHeight: options.maxHeight ? options.maxHeight : null,
                                controls: oldControls
                            }
                        ]
                    }
                ]
            }
        ];
        
        if (isAutoSizeHeight) {
            delete options.maxHeight;
        }

        Phoenix.UI.Popup.callBase(this, "initFromOptions", [ options ]);
        this._opened = false;
        this._uri = options.uri;
        this._buttons = options.buttons || [];
        this.container = this['innerBox']['innerPanel']['popupContainer'];

        this._initButtons();
        
        for (var i = 0, len = this.container.controls.length; i < len; i++) {
            this.container.controls[i].popup = this;
        }
    },

    // #region Draggable

    _attachDragHandlers: function(domElement) {
        var thisObj = this;

        thisObj.__startDrag = function(e) {
            thisObj.__dragged = true;
            thisObj.__dragStartX = e.clientX;
            thisObj.__dragStartY = e.clientY;
            thisObj.__originalPos = thisObj.get_position();
            $(document.body).mousemove(thisObj.__processDrag);
            $(document.body).mouseup(thisObj.__stopDrag);
            e.preventDefault();
            e.stopPropagation();
        };

        thisObj.__stopDrag = function(e) {
            if (!thisObj.__dragged) {
                return;
            }

            delete thisObj.__dragStartX;
            delete thisObj.__dragStartY;
            delete thisObj.__dragged;
            delete thisObj.__originalPos;

            $(document.body).unbind('mousemove', thisObj.__processDrag);
            $(document.body).unbind('mouseup', thisObj.__stopDrag);
        }

        thisObj.__processDrag = function(e) {
            var pos = thisObj.__originalPos;
            thisObj.set_position(
                pos.x + e.clientX - thisObj.__dragStartX,
                pos.y + e.clientY - thisObj.__dragStartY
            );
            e.preventDefault();
            e.stopPropagation();
        };

        $(domElement).mousedown(thisObj.__startDrag);
    },

    _detachDragHandlers: function(domElement) {
        $(domElement)
            .unbind('mousedown', this.__startDrag)
            .unbind('mouseout', this.__stopDrag)
            .unbind('mouseup', this.__stopDrag);
        
        delete this.__processDrag;
        delete this.__stopDrag;
        delete this.__startDrag;
    },

    // #endregion
    
    set_buttons: function(value) {
        if (this._buttons === value) {
            return;
        }
        
        this._buttons = value;
        
        if (this.domElement) {
            var buttonsContainer = this['innerBox']['innerPanel']['buttonsContainer'];
            
            if (buttonsContainer) {
                buttonsContainer.free();
                this['innerBox']['innerPanel'].controls.remove(buttonsContainer);
            }
            
            this._initButtons();
        }
    },
    
    _initButtons: function() {
        if (this._buttons.length == 0) {
            return;
        }
        
        var options = {
            id: 'buttonsContainer',
            height: '40px',
            cssClass: 'popup_buttons',
            padding: '7 6 7 0',
            margin: '0 1 0 0',
            border: '0 1 0 0',
            controls: []
        };
        
        for (var i = this._buttons.length - 1; i >= 0; i--) {
            var popup = this;
            var button = this._buttons[i];
            
            var buttonControl = {
                type: 'button',
                onClick: function() {
                    popup.raise_onCommand({ button: this.get_text() }); // raise command with text from button
                }
            };
            
            if (button.text) {
                buttonControl.text = button.text;
            }
            
            if (button.cssClass) {
                buttonControl.cssClass = button.cssClass;
            }
            
            if (isString(button)) {
                buttonControl.text = button;
            }
            
            options.controls.add(buttonControl);
        }
        
        var buttonsContainer = new Phoenix.UI.Panel();
        buttonsContainer.initFromOptions(options);
        
        this['innerBox']['innerPanel'].controls.add(buttonsContainer);
    },
    
    instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div');
        this.domElement.style.cssText = "position: absolute; left: -3000px; top: -3000px; z-index: 10000; display: none";
        document.body.appendChild(this.domElement);
        
        Phoenix.UI.Popup.callBase(this, "instantiateInDom", [ domElement ]);
    },
    
    get_opened: function() {
        return this._opened;
    },
    
    get_parentClientWidth: function() {
        return Application.get_clientWidth();
    },
    
    get_parentClientHeight: function() {
        return Application.get_clientHeight();
    },

    get_parentInnerWidth: function() {
        return Application.get_clientWidth();
    },

    get_parentInnerHeight: function() {
        return Application.get_clientHeight();
    },

    $get_page: function() {
        return this.page;
    },
    
    open: function() {
        if (this._opened) {
            return;
        }
        
        this.staticVars.level++;
        this.__level = this.staticVars.level;
        
        if (this._scrolling) {
            this.container.scrollToTop();
        }
        
        Application.add_onKeyDown(this._appKeyDown, this);
            
        if (this._uri) {
            if (this.page) {
                this.container.controls.remove(this.page);
                this.page.free();
            }
            
            var page = new Page();
            this.page = page;
            page.popup = this;
            
            var onInitComplete = function() {
                this.container.controls.add(page);
                this.raise_onUriLoaded(page);
                page.remove_initComplete(onInitComplete, this);
            };
                        
            page.add_initComplete(onInitComplete, this);
            page.initFromUri(this._uri);
        }
                        
        this._opened = true;
        this.show();

        var lightBoxZIndex = DepthManager.getNewZIndex();

        $(this.domElement)
            .css('z-index', DepthManager.getNewZIndex())
            .stop(true)
            .fadeIn(this._fadeSpeed);

        this._lightBox = DOM.create("div");
        this._lightBox.className = "popup_lightBox";
        this._lightBox.style.zIndex = lightBoxZIndex;

        document.body.appendChild(this._lightBox);

        this.raise_onOpen();
        this.focus();
    },
    
    _appKeyDown: function(sender, args) {
        if (args.keyCode == Keys.Esc && this.__level === this.staticVars.level) {
            this._sendCloseCommand();
            this.close();
        }
    },
    
    _sendCloseCommand: function() {
        this.raise_onCommand({ button: 'Close' });
    },
    
    close: function() {
        if (!this._opened) {
            return;
        }

        DOM.remove(this._lightBox);
        this._lightBox = null;
        
        Application.remove_onKeyDown(this._appKeyDown, this);        
        this.raise_onClose();
        this._opened = false;
        this.__level = undefined;
        
        $(this.domElement).stop(true).fadeOut(this._fadeSpeed, function() {
            this.hide();
            
            if (this.page) {
                this.page.popup = null;
                this.container.controls.remove(this.page);
                this.page.free();
                this.page = null;
            }
        }.bind(this));
        
        this.staticVars.level--;
    },

    set_position: function(x, y) {
        var style = this.domElement.style;
        style.left = x + 'px';
        style.top = y + 'px';
        this._position = { x: x, y: y };
    },

    get_position: function() {
        return this._position;
    },
    
    updateDom: function() {
        if (!this._opened) {
            return;
        }
        
        var element = this.domElement;
        var elStyle = element.style;
            
        var clientWidth = this.get_clientWidth();
        var clientHeight = this.get_clientHeight();
        
        this.set_position(
            Math.round((Application.get_clientWidth() - clientWidth) / 2),
            Math.round((Application.get_clientHeight() - clientHeight) / 2)
        );
        elStyle.width = clientWidth ? clientWidth + 'px' : '';
        elStyle.height = clientHeight ? clientHeight + 'px' : '';
        
        Phoenix.UI.Popup.callBase(this, "updateDom");
    },

    free: function() {
        this.close();
        document.body.removeChild(this.domElement);
        Phoenix.UI.FancyBox.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.Popup.prototype, [
    { name: 'title', autoEvent: true },
    { name: 'uri' }
]);

Auto.Events(Phoenix.UI.Popup.prototype, [
    'onUriLoaded',
    'onCommand',
    'onOpen',
    'onClose'
]);

Phoenix.UI.Popup.createClass('Phoenix.UI.Popup', Control);
ControlsFactory.registerControl('popup', Phoenix.UI.Popup);

/**
* TODO:
* 1) Avoid of code duplicating
*/

Type.createNamespace('Phoenix.UI');

Phoenix.UI.ScrollablePanel = function() {
    Phoenix.UI.ScrollablePanel.constructBase(this);
};

Phoenix.UI.ScrollablePanel.prototype = {
    _innerContainer: null,
    _vscrollNode: null,
    _maxScrollY: null,
    _horizontal: null,
    _vertical: null,
    _measures: {
        width: 14,
        height: 14,
        vSliderWidth: 12,
        vSliderHeight: 36,
        hSliderWidth: 36,
        hSliderHeight: 12,
        vArrowHeight: 17,
        hArrowWidth: 17
    },

    defaultOptions: {
        width: '*',
        height: '*'
    },

    initFromOptions: function(options) {
        options.cssClass = (options.cssClass || '') + ' scrollable_panel';
        
        if (isFunction(options.onScrollChanged)) {
            this.add_scrollYChanged(options.onScrollChanged, this);
        }

        this._vertical = !isNullOrUndefined(options.vertical) ? options.vertical : true;
        this._horizontal = !!options.horizontal;

        this._vscrollVisible = this._vertical;
        this._hscrollVisible = this._horizontal;
        

        if (this._horizontal) {
            options.cssClass += " hscrollable_panel";
        }

        if (this._vertical) {
            options.cssClass += " vscrollable_panel";
        }

        Phoenix.UI.ScrollablePanel.callBase(this, "initFromOptions", [ options ]);
    },
    
    get_childsContainer: function() {
        return this._innerContainer;
    },
    
    focus: function() {
        if (this._focusDetector) {
            this._focusDetector.focus();
        }
    },

    blur: function() {
        if (this._focusDetector) {
            this._focusDetector.blur();
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create("div", domElement);
        this._innerContainer = DOM.create("div", this.domElement);
        this._innerContainer.className = "scroll_panel_container";
        
        var className = "scroll_panel_container";
        
        Phoenix.UI.ScrollablePanel.callBase(this, "instantiateInDom", [ domElement ]);

        this._focusDetector = DOM.create("a", this.domElement, { className: 'focus_detector', href: 'javascript:void(0)' });
        
        $(this.domElement).mousedown(function(ev) {
            setTimeout(function() {
                if (!DOM.isFocusable(ev.target))
                    this._focusDetector.focus();
            }.bind(this), 0);
        }.bind(this));

        $(this.domElement).keydown(this._onKeyDownHandler.bind(this));

        if (this._vertical) {
            this._initVScrollInDom();
        }
        
        if (this._horizontal) {
            this._initHScrollInDom();
        }
    },

    _onKeyDownHandler: function(ev) {
        var scrollSpeed = 10;

        if (ev.keyCode == Keys.Down) {
            this.scrollBy(0, scrollSpeed);
        }
        if (ev.keyCode == Keys.Up) {
            this.scrollBy(0, -scrollSpeed);
        }
        if (ev.keyCode == Keys.Left) {
            this.scrollBy(-scrollSpeed, 0);
        }
        if (ev.keyCode == Keys.Right) {
            this.scrollBy(scrollSpeed, 0);
        }
        if (ev.keyCode == Keys.PgDown) {
            this.scrollByPage(0, 1.9);
        }
        if (ev.keyCode == Keys.PgUp) {
            this.scrollByPage(0, -1.9);
        }
        if (ev.keyCode == Keys.Home) {
            this.scrollToTop();
        }
        if (ev.keyCode == Keys.End) {
            this.scrollToBottom();
        }
    },
    
    _initVScrollInDom: function() {
        this._vscrollNode = DOM.create("div", this.domElement);
        this._vscrollNode.className = "vscroll_container";

        $(this.domElement).bind("mousewheel", function(ev, delta) {
            if (this._vscrollVisible) {
                this.scrollBy(0, -delta*25);
                ev.stopPropagation();
            }
        }.bind(this));
        
        DOM.disableSelection(this._vscrollNode);
        
        this._topArrow = DOM.create("a", this._vscrollNode, { className: "top_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_topArrow');
        this._vslider = DOM.create("a", this._vscrollNode, { className: "slider_item", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_sliderItem');
        this._bottomArrow = DOM.create("a", this._vscrollNode, { className: "bottom_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_bottomArrow');
        
        $(this._vslider).bind("mousedown", function(ev) {
            this._startVSliderDrag(ev.clientY);
            ev.preventDefault();
            ev.stopPropagation();
        }.bind(this));
        
        $(this._vscrollNode).bind("mousedown", function(ev) {
            var sliderPos = this._convertVScrollToSliderPos(this._innerContainer.scrollTop);
            
            var y = ev.layerY || ev.offsetY;
            
            this.scrollByPage(0, y > sliderPos ? 1 : -1);
        }.bind(this));
        
        $(this._topArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(0, -1);
            ev.stopPropagation();
        }.bind(this));
        
        $(this._bottomArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(0, 1);
            ev.stopPropagation();
        }.bind(this));
    },

     _initHScrollInDom: function() {
        this._hscrollNode = DOM.create("div", this.domElement);
        this._hscrollNode.className = "hscroll_container";

        // if vertical scroll is not available then attach to domElement
        var wheelNode = this._vertical ? this._hscrollNode : this.domElement;
        $(wheelNode).bind("mousewheel", function(ev, delta) {
            if (this._hscrollVisible) {
                this.scrollBy(-delta*25, 0);
                ev.preventDefault();
                ev.stopPropagation();
            }
        }.bind(this));

        DOM.disableSelection(this._hscrollNode);
        
        this._leftArrow = DOM.create("a", this._hscrollNode, { className: "left_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_leftArrow');
        this._hslider = DOM.create("a", this._hscrollNode, { className: "slider_item", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_sliderItem');
        this._rightArrow = DOM.create("a", this._hscrollNode, { className: "right_arrow", innerHTML: "&nbsp;", href: 'javascript:void(0)' }, 'scrollablePanel_rightArrow');
        
        $(this._hslider).bind("mousedown", function(ev) {
            this._startHSliderDrag(ev.clientX);
            ev.preventDefault();
            ev.stopPropagation();
        }.bind(this));
        
        $(this._hscrollNode).bind("mousedown", function(ev) {
            var sliderPos = this._convertHScrollToSliderPos(this._innerContainer.scrollLeft);
            var x = ev.layerX || ev.offsetX;
            
            this.scrollByPage(x > sliderPos ? 1 : -1, 0);
        }.bind(this));
        
        $(this._leftArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(-1, 0);
            ev.stopPropagation();
        }.bind(this));
        
        $(this._rightArrow).bind("mousedown", function(ev) {
            this._startScrollByArrows(1, 0);
            ev.stopPropagation();
        }.bind(this));
    },
    
    _startScrollByArrows: function(dirX, dirY) {
        this._scrollInterval = window.setInterval(function() {
            this.scrollBy(dirX * 7, dirY * 7);
        }.bind(this), 20);
        
        var mouseUp = function(ev) {
            $(document.body).unbind("mouseup", mouseUp);
            window.clearInterval(this._scrollInterval);
            ev.stopPropagation();
        }.bind(this);
    
        $(document.body).bind("mouseup", mouseUp);       
    },
    
    _startVSliderDrag: function(startY) {
        var startScroll = this._innerContainer.scrollTop;
    
        var mouseMove = function(ev) {
            var delta = ev.clientY - startY;
            this.set_scrollY(startScroll + this._convertVSliderPosToScroll(delta + this._measures.vSliderHeight));
            ev.stopPropagation();
        }.bind(this);
    
        var mouseUp = function(ev) {
            $(document.body).unbind("mouseup", mouseUp);
            $(document.body).unbind("mousemove", mouseMove);
            ev.stopPropagation();
        }.bind(this);
    
        $(document.body).bind("mouseup", mouseUp);
        $(document.body).bind("mousemove", mouseMove);
    },
    
    _startHSliderDrag: function(startX) {
        var startScroll = this._innerContainer.scrollLeft;
    
        var mouseMove = function(ev) {
            var delta = ev.clientX - startX;
            this.set_scrollX(startScroll + this._convertHSliderPosToScroll(delta + this._measures.hSliderWidth));
            ev.stopPropagation();
        }.bind(this);
    
        var mouseUp = function(ev) {
            $(document.body).unbind("mouseup", mouseUp);
            $(document.body).unbind("mousemove", mouseMove);
            ev.stopPropagation();
        }.bind(this);
    
        $(document.body).bind("mouseup", mouseUp);
        $(document.body).bind("mousemove", mouseMove);
    },

    scrollBy: function(scrollX, scrollY) {
        if (scrollY != 0) {
            this.set_scrollY(this.get_scrollY() + scrollY);
        }
        if (scrollX != 0) {
            this.set_scrollX(this.get_scrollX() + scrollX);
        }
    },
    
    scrollByPage: function(pageX, pageY) {
        var xscroll = 0;
        var yscroll = 0;

        if (pageY != 0) {
            var innerHeight = this.get_innerHeight();
        
            if (innerHeight <= 0) {
                return;
            }
        
            yscroll = Math.round(innerHeight / 2 * pageY);
        }

        if (pageX != 0) {
            var innerWidth = this.get_innerWidth();
        
            if (innerWidth <= 0) {
                return;
            }
        
            xscroll = Math.round(innerWidth / 2 * pageX);
        }
        
        this.scrollBy(xscroll, yscroll);
    },
    
    get_scrollY: function() {
        return this.__cachedScrollY || 0;
    },
    
    set_scrollY: function(value) {
        var element = this._innerContainer;
    
        if (!element) {
            return;
        }
        
        if (this._maxScrollY > 0) {
            var delta = Math.abs(this.__cachedScrollY - value);
        
            if (value > this._maxScrollY - delta) {
                value = this._maxScrollY;
            } else if (value < delta) {
                value = 0;            
            }
        
            element.scrollTop = value;
            var old = this.__cachedScrollY;
            this.__cachedScrollY = value;
            
            this.raise_scrollYChanged({ newValue: value, oldValue: old, maxValue: this._maxScrollY });
            
            this.__setVSliderPosition();
        }
    },
    
    get_scrollX: function() {
        return this.__cachedScrollX || 0;
    },
    
    set_scrollX: function(value) {
        var element = this._innerContainer;
    
        if (!element) {
            return;
        }
        
        if (this._maxScrollX > 0) {
            var delta = Math.abs(this.__cachedScrollX - value);
        
            if (value > this._maxScrollX - delta) {
                value = this._maxScrollX;
            } else if (value < delta) {
                value = 0;            
            }
        
            element.scrollLeft = value;
            var old = this.__cachedScrollX;
            this.__cachedScrollX = value;
            
            this.raise_scrollXChanged({ newValue: value, oldValue: old, maxValue: this._maxScrollX });
            
            this.__setHSliderPosition();
        }
    },

    __setVSliderPosition: function() {
        if (!this._vslider || isNullOrUndefined(this.__cachedScrollY)) {
            return;
        }
        
        if (this.__cachedScrollY > this._maxScrollY) {
            this.__cachedScrollY = this._maxScrollY;
            this._innerContainer.scrollTop = this.__cachedScrollY;
        }
            
        this._vslider.style.top = this._convertVScrollToSliderPos(this.__cachedScrollY) + "px";
    },
    
    __setHSliderPosition: function() {
        if (!this._hslider || isNullOrUndefined(this.__cachedScrollX)) {
            return;
        }
        
        if (this.__cachedScrollX > this._maxScrollX) {
            this.__cachedScrollX = this._maxScrollX;
            this._innerContainer.scrollLeft = this.__cachedScrollX;
        }
            
        this._hslider.style.left = this._convertHScrollToSliderPos(this.__cachedScrollX) + "px";
    },

    scrollToTop: function() {
        this.set_scrollY(0);
    },
    
    scrollToBottom: function() {
        this.set_scrollY(this._maxScrollY);
    },

    scrollToLeft: function() {
        this.set_scrollX(0);
    },

    _convertVScrollToSliderPos: function(scrollY) {
        return Math.round(scrollY / this._maxScrollY * (this.get_innerHeight() - 2 * this._measures.vArrowHeight - this._measures.vSliderHeight) + this._measures.vArrowHeight);
    },
    
    _convertVSliderPosToScroll: function(sliderPos) {
        return Math.round(this._maxScrollY * (sliderPos - this._measures.vArrowHeight) / (this.get_innerHeight() - 2 * this._measures.vArrowHeight - this._measures.vSliderHeight));
    },
    
    _convertHScrollToSliderPos: function(scrollX) {
        return Math.round(scrollX / this._maxScrollX * (this.get_innerWidth() - 2 * this._measures.hArrowWidth - this._measures.hSliderWidth) + this._measures.hArrowWidth);
    },
    
    _convertHSliderPosToScroll: function(sliderPos) {
        return Math.round(this._maxScrollX * (sliderPos - this._measures.hArrowWidth) / (this.get_innerWidth() - 2 * this._measures.hArrowWidth - this._measures.hSliderWidth));
    },

    get_innerWidth: function(noCache) {
        var innerWidth = Phoenix.UI.ScrollablePanel.callBase(this, "get_innerWidth", [ noCache ]);
        
        return innerWidth - (this._vertical && this._vscrollVisible ? this._measures.width : 0);
    },
    
    get_innerHeight: function(noCache) {
        var innerHeight = Phoenix.UI.ScrollablePanel.callBase(this, "get_innerHeight", [ noCache ]);
        
        return innerHeight - (this._horizontal && this._hscrollVisible ? this._measures.height : 0);
    },
    
    isDependsOnChildWidth: function() {
        return this._horizontal || this.get_width().isAutoSize();
    },

    isDependsOnChildHeight: function() {
        return this._vertical || this.get_height().isAutoSize();
    },

    updateDom: function() {
        DOM.setBoundingRect(this._innerContainer, this.get_innerWidth(), this.get_innerHeight());
        Phoenix.UI.ScrollablePanel.callBase(this, "updateDom");
    },

    postUpdate: function() {
        Phoenix.UI.ScrollablePanel.callBase(this, "postUpdate");
        
        if (this._vertical) {
            this.updateVerticalScroll();
        }
            
        if (this._horizontal) {
            this.updateHorizontalScroll();
        }
    },

    updateVerticalScroll: function() {
        if (!this._vertical) {
            throw new Error('Vertical scroll is not available');
        }

        var scrollHeight = this.get_layoutEngine()._getChildsHeight();
        var offsetHeight = this.get_innerHeight();

        if (offsetHeight && (scrollHeight > offsetHeight)) {
            this._maxScrollY = scrollHeight - offsetHeight;
            this.__setVSliderPosition();
                
            if (offsetHeight <= this._measures.vSliderHeight + this._measures.height + this._measures.vArrowHeight * 2) {
                this._vslider.style.display = 'none';
            } else {
                this._vslider.style.display = '';
            }

            if (!this._vscrollVisible) {
                this._vscrollVisible = true;
                this._vscrollNode.style.display = "";
                this.update();
            }
        } else {
            if (this.get_scrollY() > 0) {
                this.scrollToTop();
            }
                
            if (this._vscrollVisible) {
                this._vscrollVisible = false;
                this._vscrollNode.style.display = "none";
                this.update();
            }
        }
    },

    updateHorizontalScroll: function() {
        if (!this._horizontal) {
            throw new Error('Horizontal scroll is not available');
        }

        var scrollWidth = this.get_layoutEngine()._getChildsWidth();
        var offsetWidth = this.get_innerWidth();
            
        if (offsetWidth && (scrollWidth > offsetWidth)) {
            this._maxScrollX = scrollWidth - offsetWidth;
            this.__setHSliderPosition();
                
            if (offsetWidth <= this._measures.hSliderWidth + this._measures.width + this._measures.hArrowWidth * 2) {
                this._hslider.style.display = 'none';
            } else {
                this._hslider.style.display = '';
            }

            if (!this._hscrollVisible) {
                this._hscrollVisible = true;
                this._hscrollNode.style.display = "";
                this.update();
            }
        } else {
            if (this.get_scrollX() > 0) {
                this.scrollToLeft();
            }
                
            if (this._hscrollVisible) {
                this._hscrollVisible = false;
                this._hscrollNode.style.display = "none";
                this.update();
            }
        }
    },

    removeVerticalScrollFromDom: function() {
        DOM.remove(this._vslider);
        DOM.remove(this._topArrow);
        DOM.remove(this._bottomArrow);
        DOM.remove(this._vscrollNode);
    },

    removeHorizontalScrollFromDom: function() {
        DOM.remove(this._hslider);
        DOM.remove(this._leftArrow);
        DOM.remove(this._rightArrow);
        DOM.remove(this._hscrollNode);
    },
    
    free: function() {
        if (this._vertical) {
            this.removeVerticalScrollFromDom()
        };

        if (this._horizontal) {
            this.removeHorizontalScrollFromDom()
        };

        DOM.remove(this._focusDetector);
        DOM.remove(this._innerContainer);
        Phoenix.UI.ScrollablePanel.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.ScrollablePanel.prototype, [
    { name: 'scrollY', autoEvent: true },
    { name: 'scrollX', autoEvent: true }
]);

Phoenix.UI.ScrollablePanel.createClass('Phoenix.UI.ScrollablePanel', Control);
ControlsFactory.registerControl('scrollablePanel', Phoenix.UI.ScrollablePanel);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Silverlight = function() {
    Phoenix.UI.Silverlight.constructBase(this);
};

Phoenix.UI.Silverlight.prototype = {
    initFromOptions: function(options) {
        if (isFunction(options.onSlLoad)) {
            this.add_onSlLoad(options.onSlLoad, this);
        }
    
        if (isFunction(options.onSlPreLoad)) {
            this.add_onSlPreLoad(options.onSlPreLoad, this);
        }
        
        if (isFunction(options.onSlError)) {
            this.add_onSlError(options.onSlError, this);
        }
        
        this._initialParams = options.params || "";
        
        this._source = Application.resolveUrl(options.url || '');
        Phoenix.UI.Silverlight.callBase(this, "initFromOptions", [ options ]);
    },
    
    _onSlLoad: function() {
        if (!this.silverlightObject || isNullOrUndefined(this.silverlightObject.Content)) {
            setTimeout(this._onSlLoad.bind(this), 1000);
            return;
        }

        this.loaded = true;
        
        this.raise_onSlLoad();
    },
    
    get_content: function() {
        return this.loaded ? this.silverlightObject.Content : null;
    },

    reload: function () {            
        if (this._instantiatedInDom && this.domElement) {
            var sibling = this.domElement.nextSibling;
            var parent = this.domElement.parentNode;
            DOM.remove(this.domElement);
            this.instantiateInDom(parent);
            parent.insertBefore(this.domElement, sibling);
            this.updateDom();
        }
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div", domElement);
        
        var initParams = { params: this._initialParams };
        this.raise_onSlPreLoad(initParams);
        this._initialParams = initParams.params;
        
        var windowlessValue = this.options.isWindowless ? this.options.isWindowless : 'true';
        //var windowlessValue = 'true';

        Silverlight.createObject(this._source, this.domElement, null, {
                version: "4.0.50826.0",
                isWindowless: windowlessValue,
                minRuntimeVersion: '4.0.50826.0',
                autoUpgrade: 'true'
            },
            {
                onError: this.raise_onSlError,
                onLoad: this._onSlLoad.bind(this)
            }, this._initialParams
        );
        
        this.silverlightObject = this.domElement.firstChild;
        this.silverlightObject.style.width = '100%';
        this.silverlightObject.style.height = '100%';
        Phoenix.UI.Silverlight.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Events(Phoenix.UI.Silverlight.prototype, [
    'onSlLoad',
    'onSlError',
    'onSlPreLoad'
]);


Phoenix.UI.Silverlight.createClass('Phoenix.UI.Silverlight', Control);
ControlsFactory.registerControl('silverlight', Phoenix.UI.Silverlight);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Slider = function() {
    Phoenix.UI.Slider.constructBase(this);
    this._sliders = new Hashtable();
};

Phoenix.UI.Slider.prototype = {
    _domList: null, // ul
    _sliders: null,

    defaultOptions: {
        width: '*',
        height: '61',
        padding: '0 0 18 0'
    },

    initFromOptions: function(options) {
        this._selectedValue = options.selectedValue;
        this._textProperty = options.textProperty;
        this._textPropertyFormatter = options.textPropertyFormatter;
        this._title = options.title || "";
        
        if (options.onChanged && isFunction(options.onChanged)) {
            this.add_selectedValueChanged(function(sender, args) {
                options.onChanged.bind(this)(args);
            }, this)
        }
        
        options.cssClass = options.cssClass || 'slider';
        
        if (!this._textProperty) {
            throw new Error('[Slider]: attribute "textProperty" cannot be null.');
        }

        Phoenix.UI.Slider.callBase(this, "initFromOptions", [ options ]);
    },
    
    set_dataSource: function(value) {
        if (this._dataSource === value) {
            return;
        }

        if (this._dataSource) {
            this._detachHandlers();
            this._clearItems();
        }
        
        this._selectedValue = this.options.selectedValue;
        this._dataSource = value;
        this._attachHandlers();
        
        this.initDomFromDataSource();
    },
    
    _attachHandlers: function() {
        if (this._dataSource && this._dataSource.__observable) {
            //this._dataSource.add_added(this._dataSource_itemAdded, this);
            //this._dataSource.add_removed(this._dataSource_itemRemoved, this);
        }
    },

    _detachHandlers: function() {
        if (this._dataSource && this._dataSource.__observable) {
            //this._dataSource.remove_added(this._dataSource_itemAdded, this);
            //this._dataSource.remove_removed(this._dataSource_itemRemoved, this);
        }
    },
    
    initDomFromDataSource: function() {
        if (this._domList && this._dataSource) {
            this._domList.innerHTML = '';
        
            var textGetter = 'get_' + this._textProperty;
            
            for (var i = 0; i < this._dataSource.length; i++) {
                var dataItem = this._dataSource[i];
                var value, text;
            
                if (dataItem[textGetter]) {
                    text = dataItem[textGetter]();
                } else {
                    text = dataItem[this._textProperty];
                }
                
                value = dataItem;
                
                if (!this._selectedValue) {
                    this._selectedValue = value;
                }
                
                this.createItemInDom(value, text);
            }
        }
    },
    
	instantiateInDom: function(domElement) {
	    this.domElement = DOM.create('div', domElement);
	    
	    this.createHeaderInDom();
	    this._domList = DOM.create('ul', this.domElement);
	    
	    this.initDomFromDataSource();
	
        Phoenix.UI.Slider.callBase(this, "instantiateInDom", [ domElement ]);
	},
	
	createHeaderInDom: function() {
	    var domHeader = DOM.create('h3', this.domElement);
	    var domHeaderSpan = DOM.create('span', domHeader);
	    domHeaderSpan.appendChild(document.createTextNode(this._title));
	},
	
	createItemInDom: function(value, text) {
	    if (this._textPropertyFormatter) {
	        text = this._textPropertyFormatter(text);
	    }
	
	    var domItem = DOM.create('li');
	    domItem.appendChild(document.createTextNode(text));
	    domItem.__sliderValue = value;
	    this._sliders.put(value, domItem);
	    
	    if (this._selectedValue == value) {
	        domItem.className = 'selected';
	    }
	    
	    var $domItem = $(domItem);
	    
	    $domItem
	        .click(function() { this._itemClicked(domItem); }.bind(this))
	        .hover(
	            function() { $(this).addClass('hovered'); },
	            function() { $(this).removeClass('hovered'); }
    	    );
    	
	    this._domList.appendChild(domItem);
	    
	    var itemsCount = this._domList.childNodes.length;
	    
	    var firstWidth, itemWidth, lastWidth;
	    
	    /*if (itemsCount > 2) {
	        firstWidth = Math.floor(50 / itemsCount);
	        itemWidth = Math.floor((100 - 100 / itemsCount) / (itemsCount - 2));
	        lastWidth = 100 - (firstWidth + itemWidth * (itemsCount - 2))
	    } else {
	        firstWidth = itemsCount == 2 ? 50 : 100;
	        lastWidth = 100 - firstWidth;
	    }*/
	    
	    if (itemsCount > 2) {
	        firstWidth = Math.round(50 / (itemsCount - 1));
	        itemWidth = Math.round(100 / (itemsCount - 1));
	        lastWidth = ($.browser.msie ? 99 : 100) - firstWidth - itemWidth * (itemsCount - 2);
	    } else {
	        firstWidth = itemsCount == 2 ? 50 : 100;
	        lastWidth = ($.browser.msie ? 99 : 100) - firstWidth;
	    }
	    
    	// calculate widths
    	for (var i = 0; i < itemsCount; i++) {
    	    var node = this._domList.childNodes.item(i);
    	    
    	    if (i == 0) {
    	        $(node).addClass('first');
    	        node.style.width = firstWidth + '%';
    	    } else if (i == itemsCount - 1) {
    	        $(node).addClass('last');    	    
    	        node.style.width = lastWidth + '%';
    	    } else {
    	        $(node).removeClass('last');    	    
    	        node.style.width = itemWidth + '%';
    	    }
    	}
	},
	
	set_selectedValue: function(value) {
	    if (this._selectedValue === value) {
	        return;
	    }
	    
	    this.raise_selectedValueChanged({ oldValue: this._selectedValue, newValue: value });
	    
	    this._selectedValue = value;
	    
	    $('li', this._domList).removeClass('selected');
	    $(this._sliders.get(value)).addClass('selected');
	},
	
	_clearItems: function() {
	    for (var key in this._sliders.keys()) {
            DOM.remove(this._sliders.get(key));
	    }
	    
	    this._sliders.clear();
	},
	
	_itemClicked: function(sender) {
	    this.set_selectedValue(sender.__sliderValue);
	},
	
	free: function() {
	    this._clearItems();
	    Phoenix.UI.Slider.callBase(this, "free");
	}
};

Auto.Properties(Phoenix.UI.Slider.prototype, [
    { name: 'selectedValue', autoEvent: true },
    { name: 'textProperty' },
    { name: 'title' }
]);


Phoenix.UI.Slider.createClass('Phoenix.UI.Slider', Control);
ControlsFactory.registerControl('slider', Phoenix.UI.Slider);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Table = function() {
    Phoenix.UI.Table.constructBase(this);
};

Phoenix.UI.Table.prototype = {
    initFromOptions: function(options) {
        if (!options.columns) {
            throw new Error('[Table]: Columns meta information isn\'t passed');
        }
        
        // check columns and create templates
        for (var i = 0; i < options.columns.length; i++) {
            var column = options.columns[i];
            
            if (!column.template) {
                throw new Error('[Table.Columns]: Template isn\'t passed');                
            }
            
            column.template = new Template(column.template);
        }
        
        this.set_columns(options.columns);
    
        Phoenix.UI.Table.callBase(this, "initFromOptions", [ options ]);
        
        this._appendHeader();
    },
    
    _appendHeader: function() {
        var row = ControlsFactory.create('container');
        row.initFromOptions({ tag: 'tr', cssClass: 'table_header_row' });
        
        for (var i = 0; i < this.options.columns.length; i++) {
            var column = this.options.columns[i];
            var cellContainer = ControlsFactory.create('container');
            cellContainer.initFromOptions({ tag: 'th', cssClass: 'table_header_cell' });
            
            var label = ControlsFactory.create('label');
            label.initFromOptions({ text: column.title });
            
            cellContainer.appendControl(label);
            row.appendControl(cellContainer);           
        }
        
        this.controls.add(row);
    },
    
	instantiateInDom: function(domElement) {
	    this.domElement = DOM.create('table', domElement, { cellPadding: 0, cellSpacing: 0 });
	    this._tbody = DOM.create('tbody', this.domElement);
        Phoenix.UI.Table.callBase(this, "instantiateInDom", [ domElement ]);
	},
    
    get_childsContainer: function() {
        return this._tbody;
    },
    
	createItem: function(dataItem) {
        var row = ControlsFactory.create('container');
        row.initFromOptions({ tag: 'tr', cssClass: 'table_row' });
        
        // instantiate cells templates in columns
        var columns = this.get_columns();
        
        for (var i = 0, len = columns.length; i < len; i++) {
            var column = columns[i];
            var cellContainer = ControlsFactory.create('container');
            cellContainer.initFromOptions({ tag: 'td', cssClass: 'table_cell' });
            
            if (column.cssClass) {
                cellContainer.addCssClass(column.cssClass);
            }
            
            row.controls.add(cellContainer);
            
            var cell = column.template.instantiate(dataItem, cellContainer);
            cell.container = this;
            
            cellContainer.controls.add(cell);
        }
	    
	    return row;
	}
};

Trait.Apply(Phoenix.UI.Table.prototype, BaseListDataControl);

Auto.Properties(Phoenix.UI.Table.prototype, [
    { name: 'columns' }
]);

Phoenix.UI.Table.createClass('Phoenix.UI.Table', Control);
ControlsFactory.registerControl('table', Phoenix.UI.Table);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.TabPanel = function() {
    Phoenix.UI.TabPanel.constructBase(this);
};

Phoenix.UI.TabPanel.prototype = {
    _activeTab: null,   
    _headerDomElement: null,

    defaultOptions: {
        'border': '0',
        'height': '*',
        'width': '*'
    },
    
    get_innerHeight: function(noCache) {
        return Math.max(Phoenix.UI.TabPanel.callBase(this, "get_innerHeight", [ noCache ]) - (this.__headerHeight || 26) || 0, 0);
    },

    get_activeTab: function() { return this._activeTab; },
    set_activeTab: function(value) {
        if (this._activeTab) {
            this._activeTab.hide();
        }
        
        this._activeTab = value;
        this._activeTab.show();
    },
    
    initFromOptions: function(options) {
        if (options.controls) {
            throw new Error('You can\'t add controls to a tab panel. Use the \'tabs\' property instead of.');
        }
        
        if (options.tabs) {
            options.controls = [];
            
            for (var i = 0; i < options.tabs.length; i++) {
                var tabOptions = options.tabs[i];
                tabOptions.type = '_tab';
                tabOptions.width = '100%';
                tabOptions.height = '100%';
                tabOptions.cssClass = 'tab_page';
                options.controls.add(tabOptions);
            }
        }
        
        this.addCssClass('tabs_panel');
    
        Phoenix.UI.TabPanel.callBase(this, "initFromOptions", [ options ]);
    },
    
	instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div', domElement);
        this.instantiateHeader(this.domElement);
        
        Phoenix.UI.TabPanel.callBase(this, "instantiateInDom", [ domElement ]);
        
        if (this.controls.length > 0) {
            for (var i = 0, len = this.controls.length; i < len; i++) {
                this.controls[i].hide();
            }
            this.set_activeTab(this.controls[0]);
        }
        
        this.__headerHeight = this._headerDomElement.offsetHeight;
	},
	
	instantiateHeader: function(domElement) {
	    var headerUl = DOM.create('ul');
	    this._headerDomElement = headerUl;
	    headerUl.className = 'tab_panel_header';
	    
	    for (var i = 0, len = this.controls.length; i < len; i++) {
	        var control = this.controls[i];
	        var tabLi = DOM.create('li');
	        
	        var tabLink = DOM.create('a');
	        var tabText = document.createTextNode(control.get_title());
	        
	        $(tabLi).hover(this.tabHeaderMouseOver.bind(this), this.tabHeaderMouseOut.bind(this))
	        
	        tabLink.href = 'javascript:void(0);';
	        tabLink.__tab = control;
	        control._headerDomItem = tabLi;
	        $(tabLink).click(this.tabHeaderItemClick.bind(this));

	        tabLink.appendChild(tabText);
	        tabLi.appendChild(tabLink);
	        headerUl.appendChild(tabLi);
	    }
	    
	    domElement.appendChild(headerUl);
	},
	
	updateDom: function() {
	    if (this.controls.length > 0) {
	        var headerUl = this._headerDomElement;
	        var innerWidth = this.get_innerWidth();
	            
	        var paddings = DOM.getPaddingWidth(headerUl.childNodes[0]);
        	    
    	    var charsCount = 0;
        	    
    	    for (var i = 0, len = this.controls.length; i < len; i++) {
	            charsCount += this.controls[i].get_title().length;
    	    };
    	        
	        for (var i = 0, len = this.controls.length; i < len; i++) {
	            var control = this.controls[i];
	            var li = headerUl.childNodes[i];
                var titleLength = control.get_title().length;
	            var width = Math.floor(titleLength / charsCount * innerWidth) - paddings;

                if (width > titleLength * 10) {
                    width = titleLength * 10;
                }
	                
	            li.style.width = width + 'px';
	        }
	    }
        
        Phoenix.UI.TabPanel.callBase(this, "updateDom");
	},
	
	tabHeaderMouseOver: function(ev) {
	    var $target = $(ev.currentTarget);
	    $target.addClass('hovered');
	},
	
	tabHeaderMouseOut: function(ev) {
	    var $target = $(ev.currentTarget);
	    $target.removeClass('hovered');
	},
	
	tabHeaderItemClick: function(ev) {
	    this.set_activeTab(ev.currentTarget.__tab);
	},
	
	free: function() {
	    DOM.discardElement(this._headerDomElement);
	    this._activeTab = null;
	    Phoenix.UI.TabPanel.callBase(this, "free");
	}
};

Phoenix.UI.Tab = function() {
    Phoenix.UI.Tab.constructBase(this);
};

Phoenix.UI.Tab.prototype = {
    tabPanel: null,
    _headerDomItem: null,

    defaultOptions: {
        'border': '1',
        'height': '100%',
        'width': '100%'
    },
    
    get_isActive: function() {
        return this.tabPanel._activeTab === this;
    },
    
    set_isActive: function() {
        this.tabPanel.setActiveTab(this);    
    },
    
    show: function() {
        this.domElement.style.display = '';
        
        if (this._headerDomItem) {
            $(this._headerDomItem).addClass('active');
        }
        
        this.update(this);
        this.raise_onActivate();
    },
    
    hide: function() {
        this.domElement.style.display = 'none';
        
        if (this._headerDomItem) {
            $(this._headerDomItem).removeClass('active');
        }
    },

    initFromOptions: function(options) {
        if (isFunction(options.onActivate)) {
            this.add_onActivate(options.onActivate, this);
        }

        Phoenix.UI.TabPanel.callBase(this, "initFromOptions", [ options ]);
        this._title = options.title;
    },
    
	instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div');
        this.className = 'tab';
        domElement.appendChild(this.domElement);
        
        Phoenix.UI.TabPanel.callBase(this, "instantiateInDom", [ domElement ]);
	}
};

Auto.Events(Phoenix.UI.Tab.prototype, [
    'onActivate'
]);

Auto.Properties(Phoenix.UI.Tab.prototype, [
    { name: 'title', autoEvent: true }
]);

Phoenix.UI.TabPanel.createClass('Phoenix.UI.TabPanel', Control);
Phoenix.UI.Tab.createClass('Phoenix.UI.Tab', Control);
ControlsFactory.registerControl('tabPanel', Phoenix.UI.TabPanel);
ControlsFactory.registerControl('_tab', Phoenix.UI.Tab);
Phoenix.UI.TextBox.Validation = {
    validate: function() {
        this._isValid = true;        
        
        var options = this._validationOptions;
        
        if(!options)
            return;
        
        var text = this.get_text().toString();
        
        if(options.notEmpty && String.isNullOrEmpty(text))
            this._isValid = false;
        
        if (options.expression && text && !text.match(options.expression)) {
            this._isValid = false;
        }
        
        if (!this._isValid) {
            this._validationIsFailed();
            return false;
        }
        else {
            this._validationIsSuccess();
            return true;
        }
    },

    _validationIsSuccess: function() {
        this.removeCssClass('notValid');
        this._hideValidationTooltip();
    },

    _validationIsFailed: function() {
        this.addCssClass('notValid');
        this._showValidationTooltip();
    },

    _showValidationTooltip: function() {
        var domElement = this.domElement;

        if (this._validationTooltip) {
            return;
        }

        if (!domElement) {
            return;
        }

        var validationText = this.get_validationOptions().errorText || 'Wrong format';
        var offset = $(domElement).offset();

        var tooltip = DOM.create('div', document.body, { className: 'validation_tooltip' });
        this._validationTooltip = tooltip;
        $(tooltip).text(validationText);
        var tooltipStyle = tooltip.style;
        tooltipStyle.position = 'absolute';
        tooltipStyle.left = (offset.left) + 'px';
        tooltipStyle.top = (offset.top - 21) + 'px';
        tooltip.__height = $(tooltip).height();
        tooltip.__top = offset.top - 21;

        tooltipStyle.width = (this.get_clientWidth() - 6) + 'px';
        tooltipStyle.zIndex = DepthManager.getNewZIndex();
        
        $(tooltip)
            .animate({
                height: 0,
                top: offset.top,
                opacity: 0
            }, 0)
            .animate({
                height: tooltip.__height,
                top: tooltip.__top,
                opacity: 1
            }, 150);

        setTimeout(this._hideValidationTooltip.bind(this), 5000);
        this.add_onFree(this._hideValidationTooltip, this);
    },
    
    _hideValidationTooltip: function() {
        if (this._validationTooltip) {
            this.remove_onFree(this._hideValidationTooltip, this);
            var tooltip = this._validationTooltip;
            $(tooltip)
                .animate({
                    height: 0,
                    top: tooltip.__top + 27,
                    opacity: 0
                }, 150, null, function() {
                    DOM.remove(tooltip);
                });
            this._validationTooltip = null;
        }
    }
};

Auto.Properties(Phoenix.UI.TextBox.Validation, [
    'validationOptions',
    'isValid'
]);

Phoenix.UI.TextBox.prototype.initValidation = function(validationOptions) {
    if(!this.__validationInitialized) {
        Trait.Apply(this, Phoenix.UI.TextBox.Validation);
        this.__validationInitialized = true;
    }
    
    this._isValid = null;
    
    this.set_validationOptions(validationOptions);    
};
Type.createNamespace('Phoenix.UI');

Phoenix.UI.TextBox = function() {
    Phoenix.UI.TextBox.constructBase(this);
};

Object.extend(Phoenix.UI.TextBox.prototype, {
    _activeClassName: null,
    _watermarkText: null,
    _watermarkShowed: null,
    _mode: null,
    _format: null,
    
    defaultOptions: {
        bindings: {
            '*' : 'text'
        },
        border: '1',
        width: '*',
        height: '24px',
        padding: '2',
        disabledCssClass: 'textBox_disabled'
    },

    initFromOptions: function(options) {
        Phoenix.UI.TextBox.callBase(this, "initFromOptions", [ options ]);
        
        this._text = isDefined(this._text) ? this._text : (options.text || '');
        this._activeClassName = options.activeCssClass;
        this._watermarkText = options.watermark;
        this._mode = options.mode || 'single';
        this._format = options.format || 'text';

        if(options.validation)
            this.initValidation(options.validation);
        
        if (options.onChanged && isFunction(options.onChanged)) {
            this.add_textChanged(options.onChanged, this);
        }
        
        if (options.onKeyPress && isFunction(options.onKeyPress)) {
            this.add_keyPressed(options.onKeyPress, this);
        }
        
        if (options.onEnterPressed && isFunction(options.onEnterPressed)) {
            this.add_enterPressed(options.onEnterPressed, this);
        }

        if (this._format == 'double' || this._format == 'float') {
            this.initValidation({
                expression: /^[0-9]+?(\.[0-9]+?)?$/,
                errorText: 'Please enter a numeric value',
                notEmpty: true
            });
        }
        
        if (this._format == 'int') {
            this.initValidation({
                expression: /^[0-9]+?$/,
                errorText: 'Please enter an integer value',
                notEmpty: true
            });
        }
    },

    _onChange: function(args) {
        this.set_text(this.domElement.value, false);
    },
    
    get_dataSource: function() {
        return this.get_text();
    },
    
    get_text: function() {
        return this._watermarkShowed ? "" : this._text;
    },
    
    set_text: function(value, updateDom) {
        var updateDom = isNullOrUndefined(updateDom) ? true : updateDom;
    
        if (isNullOrUndefined(value))
            value = '';
        
        if (this.domElement) {
            if (this.domElement.value !== value && updateDom) {
                this.domElement.value = value;
                this._checkWatermark();
            }
        
            if (this.validate) {
                if (this.__validationTimeout) {
                    clearTimeout(this.__validationTimeout);
                }

                this.__validationTimeout = setTimeout(function() {
                    this.validate();
                }.bind(this), 300);
            }
        }

        if (this._text !== value) {
            var oldValue = this._text;
            this._text = value;
    
            this.raise_textChanged({oldValue: oldValue, newValue: value});
        }
    },
    
    set_enabled: function(value) {
        if (this._enabled === value)
            return false;
            
        this._enabled = value;
        
        if (this.domElement) {
            this.domElement.disabled = !value;
        };
        
        if (this._disabledCssClass) {
            if (!value) {
                this.addCssClass(this._disabledCssClass);
            } else {
                this.removeCssClass(this._disabledCssClass);
            }
        }
    },
    
    _onFocus: function() {
        var element = this.domElement;
        
        if (this._activeClassName) {
            $(element).addClass(this._activeClassName);
        }
        
        if (this._watermarkText && element.value == this._watermarkText && this._watermarkShowed) {
            element.value = "";
            this.removeCssClass('watermark');
            this._watermarkShowed = false;
        }
    },
    
    _onBlur: function() {
        var element = this.domElement;
        
        if(!element)
            return;
    
        if (this._activeClassName) {
            $(element).removeClass(this._activeClassName);
        }
        
        if (!element.value && this._watermarkText) {
            element.value = this._watermarkText;
            this.addCssClass('watermark');
            this._watermarkShowed = true;
        }
    },

    _checkWatermark: function() {
        if (this._watermarkText && this.domElement) {
            var element = this.domElement;

            if (!element.value) {
                element.value = this._watermarkText;
                this.addCssClass('watermark');
                this._watermarkShowed = true;
            } else if (this._watermarkShowed && element.value != this._watermarkText) {
                this.removeCssClass('watermark');
                this._watermarkShowed = false;
            }
        }
    },

    _onKeyPressed: function(ev) {
        if (ev.keyCode == 13 || ev.keyCode == 10) {
            this.raise_enterPressed();
        }
        
        this.raise_keyPressed();
    },

    selectAll: function() {
        if (this.domElement) {
            this.domElement.select();
        }
    },
    
    instantiateInDom: function(domElement) {
        this.domElement = DOM.create(this._mode == 'multiline' ? 'textarea' : 'input', domElement);
        this.domElement.value = this._text;
        
        var $element = $(this.domElement);
        
        $element
            .bind('keyup', this._onChange.bind(this))
            .bind('mousedown', function() {
                this.__mouseDown = true;
            }.bind(this))
            .bind('mouseup', function() {
                this.__mouseDown = false;
            }.bind(this))
            .bind('mouseout', function(e) {
                if (this.__mouseDown) {
                    this.selectAll();
                    this.__mouseDown = false;
                    e.stopPropagation();
                    e.preventDefault();
                }
            }.bind(this))
            .bind('keyup', this._onKeyPressed.bind(this));
        
        if (this._activeClassName || this._watermarkText) {
            $element
                .bind('focus', this._onFocus.bind(this))
                .bind('blur', this._onBlur.bind(this));
        };
        
        if (this._watermarkText && !this._text) {
            this.domElement.value = this._watermarkText;
            this._watermarkShowed = true;
            this.addCssClass('watermark');
        }

        Phoenix.UI.TextBox.callBase(this, "instantiateInDom", [ domElement ]);
    }
});

Auto.Properties(Phoenix.UI.TextBox.prototype, [
    { name: 'text', autoEvent: true }
]);

Auto.Events(Phoenix.UI.TextBox.prototype, [ 'keyPressed', 'enterPressed' ]);

Phoenix.UI.TextBox.createClass('Phoenix.UI.TextBox', Control);
ControlsFactory.registerControl('textBox', Phoenix.UI.TextBox);
ControlsFactory.registerControl('textbox', Phoenix.UI.TextBox);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.TextEditor = function() {
    Phoenix.UI.TextEditor.constructBase(this);
};

Object.extend(Phoenix.UI.TextEditor.prototype, {
    defaultOptions: {
        bindings: {
            '*' : 'text'
        }
    },

    initFromOptions: function(options) {
        Phoenix.UI.TextEditor.callBase(this, "initFromOptions", [ options ]);
        
        this._text = options.text || '';
    },

    _getUrl: function(resource) {
        return Application.resolveUrl('~/scripts/ckeditor/' + resource);
    },

    get_text: function() {
        if (this._ckeditor) {
            return this._ckeditor.getData();
        } else {
            return this._text;
        }
    },

    set_text: function(value) {
        if (this._ckeditor) {
            this._ckeditor.setData(value);
        } else {
            this._text = value;
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create('textarea', domElement);
        this.domElement.value = this._text;
        
        Phoenix.UI.TextEditor.callBase(this, "instantiateInDom", [ domElement ]);
        
        this._ckeditor = CKEDITOR.replace(this.domElement, { width: this.get_innerWidth(), height: this.get_innerHeight() - 130 });
    },
    
    updateDom: function() {
        Phoenix.UI.TextEditor.callBase(this, "updateDom");
        try {
            this._ckeditor.resize(this.get_innerWidth(), this.get_innerHeight());
        }
        catch(e) {
            setTimeout(this.updateDom.bind(this), 500);
        }
    },

    free: function() {
        CKEDITOR.remove(this._ckeditor);
        Phoenix.UI.TextEditor.callBase(this, "free");
    }
});

Auto.Properties(Phoenix.UI.TextEditor.prototype, [
    { name: 'text', autoEvent: true }
]);

//Auto.Events(Phoenix.UI.TextEditor.prototype, [ 'keyPressed', 'enterPressed' ]);

Phoenix.UI.TextEditor.createClass('Phoenix.UI.TextEditor', Control);
ControlsFactory.registerControl('textEditor', Phoenix.UI.TextEditor);
Type.createNamespace('Phoenix.UI');

Phoenix.UI.Tree = function(options) {
    Phoenix.UI.Tree.constructBase(this);
    this._level = 0;
};

Phoenix.UI.Tree.prototype = {
    _level: null,

    defaultOptions: {
        'width': '*',
        'height': '?'
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create('UL');
        domElement.appendChild(this.domElement);

        Phoenix.UI.Tree.callBase(this, "instantiateInDom", [domElement]);
    },

    initFromOptions: function (options) {
        Phoenix.UI.Tree.callBase(this, "initFromOptions", [options]);

        if (isFunction(options.onInnerTreeOptionsCreated)) {
            this.add_onInnerTreeOptionsCreated(options.onInnerTreeOptionsCreated, this);
        }

        if (!options.nodeTemplate) {
            options.nodeTemplate = {
                /*bindings: {
                },*/
                controls: [
                    {
                        id: 'nodeText',
                        width: '*',
                        type: 'label'
                    }
                ]
            };
        }

        this.addCssClass('treeview');

        if (this._level > 0) {
            this.addCssClass('treeview_' + this._level);
        }

        if (!(options.nodeTemplate instanceof Array))
            options.nodeTemplate = [options.nodeTemplate];

        if (options.nodeTemplate[0].type && options.nodeTemplate[0].type !== 'treeNode') {
            throw new Error('You cannot change type of a node template');
        }

        if (!options.nodeTemplate[0].bindings) {
            var bindingProp = options.textProperty || '*';
            options.nodeTemplate[0].bindings = {};
            options.nodeTemplate[0].bindings[bindingProp] = '*';
        }

        options.nodeTemplate[0].type = "treeNode";
        options.nodeTemplate[0].id = "node";

        if (options.emptyDataTemplate) {
            if (!(options.emptyDataTemplate instanceof Array)) {
                options.emptyDataTemplate = [options.emptyDataTemplate];
            }

            this._emptyDataTemplate = new Template(options.emptyDataTemplate[0]);
        } else {
            this.options.emptyDataTemplate = [];
        }

        this._textProperty = options.textProperty;
        this.set_nodeTemplate(new Template(options.nodeTemplate[0]));
        this.set_childProperty(options.childProperty);
    },

    createItem: function (dataItem) {
        var li = ControlsFactory.create('container');
        var shouldCreateInnerTree = this.get_childProperty() && Object.getPropertyValue(dataItem, this.get_childProperty());

        var bindings = {};
        bindings['*'] = 'node';

        if (shouldCreateInnerTree) {
            bindings[this.get_childProperty()] = function (sender, args) {
                this.innerTree.set_dataSource(args.newValue);

                if (this.node) {
                    this.node._innerTreeModified();
                }
            };
            bindings[this.get_childProperty() + '.changed'] = function (sender, args) { // TODO: sometimes datasource wouldn't contain "childProperty".changed 
                if (this.node) {
                    this.node._innerTreeModified();
                }
            };
        }

        li.initFromOptions({
            tag: 'LI',
            cssClass: 'tree_node tree_node_' + this._level,
            bindings: bindings,
            width: '100%',
            height: '?',
            onFree: function () {
                this.parentNode = null;
            }
        });

        var control = this._nodeTemplate.instantiate();
        control.parentNode = this.parent;
        li.appendControl(control);

        if (shouldCreateInnerTree) {
            var innerTree = this._createInnerTree();

            if (innerTree) {
                li.appendControl(innerTree);

                if (control instanceof Phoenix.UI.TreeNode) {
                    control.addCssClass('tree_node_' + this._level);
                    control.set_innerTree(innerTree);
                }
            }
        }

        this.get_childsHash().put(dataItem, li);

        li.set_dataSource(dataItem);

        return li;
    },

    _createInnerTree: function () {
        var tree = ControlsFactory.create('tree');
        tree._level = this._level + 1;

        var innerTreeOptions = {
            id: 'innerTree',
            width: '*',
            height: '?',
            margin: '10 0 0 0',
            cacheDisabled: this.options.cacheDisabled,
            childProperty: this._childProperty,
            textProperty: this._textProperty,
            nodeTemplate: (this.options.nodeTemplate.length == 1) ? this.options.nodeTemplate[0] : this.options.nodeTemplate.slice(1),
            emptyDataTemplate: (this.options.emptyDataTemplate.length > 1) ? this.options.emptyDataTemplate.slice(1) : undefined
        };

        this.raise_onInnerTreeOptionsCreated({ options: innerTreeOptions });

        tree.initFromOptions(innerTreeOptions);

        return tree;
    },

    free: function () {
        this.clear();
        this.__emptyDataControl = null;

        Phoenix.UI.Tree.callBase(this, 'free');
    }
};

Auto.Events(Phoenix.UI.Tree.prototype, [
    'onClick',
    'onInnerTreeOptionsCreated'
]);

Auto.Properties(Phoenix.UI.Tree.prototype, [
    'childProperty',
    'nodeTemplate'
]);

Phoenix.UI.Tree.createClass('Phoenix.UI.Tree', Control);
Trait.Apply(Phoenix.UI.Tree.prototype, BaseListDataControl);

ControlsFactory.registerControl('tree', Phoenix.UI.Tree);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.TreeNode = function() {
    Phoenix.UI.TreeNode.constructBase(this);
    this._innerTreeShowed = true;
};

Phoenix.UI.TreeNode.prototype = {
    _collapser: null,
    _innerTree: null,
    _innerTreeShowed: null,

    defaultOptions: {
        'width': '*',
        height: '?'
    },

    get_innerWidth: function() {
        return Math.max(Phoenix.UI.TreeNode.callBase(this, "get_innerWidth") - 13, 0);
    },

    initFromOptions: function (options) {
        Phoenix.UI.TreeNode.callBase(this, "initFromOptions", [options]);
        this.addCssClass('tree_node');
    },

    instantiateInDom: function (domElement) {
        this.domElement = DOM.create("div", domElement);
        this._initCollapser();

        Phoenix.UI.TreeNode.callBase(this, "instantiateInDom", [domElement]);

        this._innerTreeModified();
    },

    _initCollapser: function () {
        if (this._innerTree && !this._collapser && this.domElement) {
            this._collapser = DOM.create("span", this.domElement, { innerHTML: "&nbsp;", className: 'tree_collapser' });
            $(this._collapser).bind("click", this._collaperClick.bind(this));
            DOM.disableSelection(this._collapser);
        }
    },

    _collaperClick: function () {
        this._innerTreeShowed = !this._innerTreeShowed;

        if (this._innerTreeShowed) {
            this._innerTree.show(true);
            $(this._collapser).removeClass('tree_collapsed');
        } else {
            this._innerTree.hide(true);
            $(this._collapser).addClass('tree_collapsed');
        }
    },

    set_isSelected: function (value) {        
        if (this._isSelected === value) {
            return;
        }

        this._isSelected = value;

        if (value) {
            this.addCssClass('tree_node_selected');
        } else {
            this.removeCssClass('tree_node_selected');
        }
    },

    _innerTreeModified: function (sender, args) {
        this._initCollapser();

        if (this._collapser) {
            this._collapser.style.display = !this._innerTree.isEmpty() ? '' : 'none';
        }
    },

    set_innerTree: function (value) {
        if (this._innerTree === value) {
            return;
        }

        this._innerTree = value;
        this._innerTreeModified();
    },

    free: function () {
        Phoenix.UI.TreeNode.callBase(this, "free");
    }
};

Auto.Properties(Phoenix.UI.TreeNode.prototype, [
    { name: 'innerTree' },
    { name: 'isSelected' }
]);

Phoenix.UI.TreeNode.createClass('Phoenix.UI.TreeNode', Control);
ControlsFactory.registerControl('treeNode', Phoenix.UI.TreeNode);

Type.createNamespace('Phoenix.UI');

Phoenix.UI.Tooltip = function() {
    Phoenix.UI.Tooltip.constructBase(this);
};

Phoenix.UI.Tooltip.prototype = {
    /**
     * Declaring inner variables
     */
    _position: {x: 0, y: 0},

    defaultOptions: {
        'visible': false,
        'width': '?',
        'height': '?',
        'layout': 'stack',
        'fadeSpeed': 0
    },

    /**
     * Declaring inner controls
     */

    initFromOptions: function(options) {
        options.cssClass = options.cssClass || 'tooltip';
        this._inDocumentFlow = false;
        var tooltipArea =   { type: 'panel', width: '?', height: '?' };
        Object.extend(tooltipArea, options.template);

        options.controls = [
            tooltipArea
        ];
        Phoenix.UI.Tooltip.callBase(this, "initFromOptions", [ options ]);
    },

    /**
     * Manual instantiation
     */
    instantiateInDom: function(domElement) {
        this.domElement = DOM.create('div');
        this.domElement.style.cssText = "position: absolute; left: " + this._position.x + "px; top: " + this._position.y + "px; z-index: 10000; display: none";
        document.body.appendChild(this.domElement);
        Phoenix.UI.Tooltip.callBase(this, "instantiateInDom", [ domElement ]);
    },

    updateDom: function() {
        if (!this._opened) {
            return;
        }
        
        var element = this.domElement;
        var elStyle = element.style;
            
        var clientWidth = this.get_clientWidth();
        var clientHeight = this.get_clientHeight();

        elStyle.width = clientWidth ? clientWidth + 'px' : '';
        elStyle.height = clientHeight ? clientHeight + 'px' : '';
        
        Phoenix.UI.Popup.callBase(this, "updateDom");
    },

    set_position: function(position) {
        this._position = position;
    },

    get_position: function() {
        return this._position;
    },

    open: function() {
        if (this._opened) {
            return;
        }
        
        this._opened = true;
        this.show();
        $(this.domElement)
            .css('z-index', DepthManager.getNewZIndex())
            .stop(true)
            .fadeIn(this.options.fadeSpeed);
    }
};

Phoenix.UI.Tooltip.createClass('Phoenix.UI.Tooltip', Control);
ControlsFactory.registerControl('tooltip', Phoenix.UI.Tooltip);
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

Type.createNamespace('Phoenix.UI');

Phoenix.UI.ColorPicker = function() {
    Phoenix.UI.ColorPicker.constructBase(this);
    this._isShowed = false;
};

Phoenix.UI.ColorPicker.prototype = {
    defaultOptions: {
        bindings: {
            '*' : 'color'
        }
    },

    _jscolor: null,
    _isShowed: null,

    set_color: function(value) {
        if (this._color === value) {
            return;
        }

        this._color = value;
        
        if (this._jscolor) {
            var result = this._jscolor.fromString(value);

            if (!result) {
                throw new Error(value + ' is not valid color');
            }
        }
    },

    initFromOptions: function (options) {
        Phoenix.UI.ColorPicker.callBase(this, "initFromOptions", [options]);

        this.addCssClass('color_picker_control');

        if (isFunction(options.onChanged)) {
            this.add_onChanged(options.onChanged, this);
        }
    },

    instantiateInDom: function(domElement) {
        this.domElement = DOM.create("a", domElement);

        this._jscolor = new jscolor.color(this.domElement, {
            valueElement: null,
            styleElement: this.domElement,
            pickerMode: 'HVS',
            pickerZIndex: DepthManager.getNewZIndex()
        });

        this._jscolor.fromString(this.get_color());

        if (!this._globalClickCallback) {
            this._globalClickCallback = function(ev) {
                var element = jscolor.picker.boxB;

                if (element !== ev.target && !$.contains(element, ev.target)) {
                    var color = '#' + this._jscolor.toString();
                    this.set_color(color);
                    this.raise_onChanged({ newValue: color });

                    this._jscolor.hidePicker();
                    $(document.body).unbind('click', this._globalClickCallback);
                }
            }.bind(this);
        }

        $(this.domElement).click(function() {
            window.setTimeout(function() {
                $(document.body).click(this._globalClickCallback);
            }.bind(this), 200);

            this._isShowed = true;
            this._jscolor.pickerZIndex = DepthManager.getNewZIndex();
            this._jscolor.showPicker();
        }.bind(this));

        Phoenix.UI.ColorPicker.callBase(this, "instantiateInDom", [ domElement ]);
    }
};

Auto.Properties(Phoenix.UI.ColorPicker.prototype, [
    { name: 'color' }
]);

Auto.Events(Phoenix.UI.ColorPicker.prototype, [
    'onChanged'
]);

Phoenix.UI.ColorPicker.createClass('Phoenix.UI.ColorPicker', Control);
ControlsFactory.registerControl('colorPicker', Phoenix.UI.ColorPicker);