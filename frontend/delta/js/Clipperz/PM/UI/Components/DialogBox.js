/*

Copyright 2008-2013 Clipperz Srl

This file is part of Clipperz, the online password manager.
For further information about its features and functionalities please
refer to http://www.clipperz.com.

* Clipperz is free software: you can redistribute it and/or modify it
  under the terms of the GNU Affero General Public License as published
  by the Free Software Foundation, either version 3 of the License, or 
  (at your option) any later version.

* Clipperz is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Clipperz. If not, see http://www.gnu.org/licenses/.

*/

Clipperz.Base.module('Clipperz.PM.UI.Components');

Clipperz.PM.UI.Components.DialogBox = React.createClass({

	propTypes: {
		'level':	React.PropTypes.oneOf(['HIDE', 'INFO', 'WARNING', 'ERROR']).isRequired,
		'message':	React.PropTypes.string.isRequired,
	},

	ask: function (someInfo) {
		var	deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred('DialogBox.ask', {trace:false});
		deferredResult.addCallback(someInfo.['possibleAnswers']['cancel']['answer']);
		deferredResult.callback();
//		deferredResult.cancel();
		
		return deferredResult;
	},
	
	//=========================================================================

	render: function () {
		return	React.DOM.div({className:'messageBox ' + this.props['level']}, this.props['message']);
	}

	//=========================================================================
});
