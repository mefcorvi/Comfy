({
    title: 'Master Page',
    layout: 'stack',
    orientation: 'vertical',
    cssClass: 'default_master',
    valign: 'stretch',

    customFunctions: {},

    controls: [
        {
            type: 'popup',
            id: 'confirmPopup',
            minWidth: '100',
            width: '30%',
            height: '?',
            scrolling: true,
            buttons: ['Yes', 'No'],
            controls: [
                {
                    type: 'label',
                    padding: '5',
                    width: '*',
                    height: '?',
                    id: 'popupText'
                }
            ]
        },
        {
            type: 'popup',
            id: 'errorPopup',
            minWidth: 100,
            width: '30%',
            height: '?',
            scrolling: true,
            buttons: ['OK'],
            controls: [
                {
                    type: 'label',
                    padding: '5',
                    width: '*',
                    height: '?',
                    id: 'popupText'
                }
            ]
        },

	{
	    id: 'page_header',
	    type: 'panel',
	    cssClass: 'top_panel',
	    height: 40,
	    padding: '5 0 5 6',
	    controls: [
		{
		    type: 'panel',
		    orientation: 'horizontal',
		    halign: 'center',
		    maxWidth: 1000,
		    controls: [
			{ type: 'link', id: 'logo', width: 220, height: '*', url: 'page:default' },
			{ type: 'label', id: 'hints', cssClass: 'hints', width: '*', height: '?', valign: 'middle', text: 'Press "?" to see the hot keys' },
			{
			    type: 'panel',
			    id: 'user_panel',
			    cssClass: 'user_panel',
			    width: '?',
			    orientation: 'horizontal',
			    controls: [
				{ type: 'link', text: 'Settings', url: 'page:settings', valign: 'middle', margin: '0 0 20 0' },
				{ type: 'link', text: 'Help', valign: 'middle', margin: '0 0 20 0' },
				{ type: 'link', text: 'Sign out', valign: 'middle' }
			    ]
			}
		    ]
		}
	    ]
	},
	{
	    type: 'panel',
	    height: '25px',
	    orientation: 'horizontal',
	    padding: '5px 0',
	    margin: '0 15px 0 0',
	    maxWidth: 1000,
	    halign: 'center',
	    controls: [
		{
		    type: 'label',
		    text: 'Summary',
		    width: 185,
		    height: '*',
		    padding: '0 0 25px 0',
		    cssClass: 'page_title',
		    border: '0 0 1 0'
		},
		{
		    type: 'panel',
		    orientation: 'horizontal',
		    cssClass: 'page_menu',
		    controls: [
			{ type: 'link', text: 'Add task', height: '*', padding: '25px 0', border: '0 0 1 0' },
			{ type: 'link', text: 'Show categories', height: '*', padding: '25px 0', border: '0 0 1 0' }
		    ]
		}
	    ]
	},
	{
	    type: 'textBox',
	    width: '*',
	    height: '30px',
	    cssClass: 'top_search_panel',
	    margin: '5px 15px 5px 0',
	    padding: '10px',
	    watermark: 'Search...',
	    maxWidth: '1000',
	    halign: 'center'
	},

	// top panel
        {
            id: "page_container",
            type: "panel",
	    maxWidth: 1000,
	    halign: 'center',
	    margin: '5px 15px 5px 0'
        }
    ]
})