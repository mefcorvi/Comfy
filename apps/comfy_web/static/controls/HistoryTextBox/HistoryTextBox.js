Type.createNamespace('Phoenix.UI');

Phoenix.UI.HistoryTextBox = function() {
    Phoenix.UI.HistoryTextBox.constructBase(this);
};

Phoenix.UI.HistoryTextBox.prototype = {
    initFromOptions: function(options) {
	Object.extend(options,
		      {
			  mode: 'multiline',
			  onKeyDown: function(sender, args) {
			      if (args.keyCode == Keys.Up && this.getCaret() < 1 && this._history.length > 0 && this._historyPos > 0) {
				  this._historyPos--;
				  var lastMsg = this._history[this._historyPos];
				  this.set_text(lastMsg);
			      }
			      if (args.keyCode == Keys.Down && (this.getCaret() >= this.get_text().length)) {
				  if (this._historyPos < this._history.length - 1) {
				      this._historyPos++;
				      var lastMsg = this._history[this._historyPos];
				      this.set_text(lastMsg);
				  } else {
				      this.set_text("");
				      this._historyPos = this._history.length;
				  }
			      }
			  },
			  onEnterPressed: function(sender, args) {
			      args.preventDefault();
			      args.stopPropagation();
			      var msg = this.get_text();
			      this.get_window().sendMsg(msg);
			      this.set_text('');
			      this._history.push(msg);
			      this._historyPos = this._history.length;
			  },
			  onLoad: function() {
			      this._historyPos = -1;
			      this._history = [];
			  }
		      });

        Phoenix.UI.HistoryTextBox.callBase(this, "initFromOptions", [ options ]);
    }
};

Phoenix.UI.HistoryTextBox.createClass('Phoenix.UI.HistoryTextBox', Phoenix.UI.TextBox);
ControlsFactory.registerControl('historyTextBox', Phoenix.UI.HistoryTextBox);