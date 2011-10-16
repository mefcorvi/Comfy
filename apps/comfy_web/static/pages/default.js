({
    title: "Main Page",
    orientation: 'horizontal',
    controls: [
	{ type: 'label', id: 'hello', text: 'Hello world!' },
	{
	    type: 'button',
	    text: 'Click me!',
	    onClick: function() {
		this.parent.hello.set_text("Hello from the button!");
	    }
	}
    ]
})

