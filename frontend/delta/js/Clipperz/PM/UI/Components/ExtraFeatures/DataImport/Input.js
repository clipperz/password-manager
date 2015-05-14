/*

Copyright 2008-2015 Clipperz Srl

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

"use strict";
Clipperz.Base.module('Clipperz.PM.UI.Components.ExtraFeatures.DataImport');

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.InputClass = React.createClass({
	handleUploadFiles: function(someFiles) {
		var file;
		var reader;
		
		if (someFiles.length == 1) {
			file = someFiles[0];
				reader = new FileReader();
				
				// TODO: check what happens with binary files
				reader.onloadend = MochiKit.Base.bind(function() {
					var extractedJson = this.props.extractJsonFromClipperzExportCallback(reader.result);
					
					if (extractedJson) {
						this.props.setImportStateCallback({'importData': {'input': extractedJson}});
					} else {
						this.props.setImportStateCallback({'importData': {'input': reader.result}});
					}
				},this,reader);
				
				reader.readAsText(file);
		} else {
			alert("Error: expecting a file as input.");
		}
	},
	
	handleOnDrop: function(e) {
		e.preventDefault();
		
		this.handleUploadFiles(e.dataTransfer.files)
	},
	
	handleInputFiles: function(e) {
		e.preventDefault();
		
		this.handleUploadFiles(e.target.files)
	},
	
	handleOnDragOver: function(e) {
		// Somehow necessary:
		// http://enome.github.io/javascript/2014/03/24/drag-and-drop-with-react-js.html
		// https://code.google.com/p/chromium/issues/detail?id=168387
		// http://www.quirksmode.org/blog/archives/2009/09/the_html5_drag.html
		e.preventDefault();
	},
	
	handleSubmit: function(event) {
		var jsonData;
		var newState;
		
		var inputString = this.refs['input-textarea'].getDOMNode().value.trim();
		
		event.preventDefault();
		
		if (newState = this.props.parseJsonCallback(inputString)) {
			this.props.setImportStateCallback(newState);
		} else if (newState = this.props.parseCsvCallback(inputString)) {
			this.props.setImportStateCallback(newState);
		} else {
			alert("Unrecognized input format...");
		}
	},
	
	handleTextareaChange: function() {
		this.props.setImportStateCallback({'importData': {'input': this.refs['input-textarea'].getDOMNode().value}});
	},

	render: function() {
		return React.DOM.div({},[
			React.DOM.h2({},"Input"),
			Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
				'format': 'json',
				'stepId': 'input'
			}),
			React.DOM.form({'key':'form', 'className':'importForm', 'onSubmit': this.handleSubmit }, [
				React.DOM.button({'key':'input-next', 'type':'submit', 'className':'button'}, "Next"),
				React.DOM.input({
					'type': 'file',
					'ref': 'upload-input',
					'onClick': function(e) { e.target.value = null },
					'onChange': this.handleInputFiles,
					'style': {'display': 'none'}
				}),
				React.DOM.div({
					'style': { // TODO: replace with proper CSS
						'width': '90%',
						'textAlign': 'center',
						'lineHeight': '3em',
						'border': '3px dashed white'
					},
					'onDragOver': this.handleOnDragOver,
					'onDrop': this.handleOnDrop,
					'onClick': MochiKit.Base.bind(function() { this.refs['upload-input'].getDOMNode().click() }, this)
				}, "Drag your Clipperz export file here or click select it manually."),
				React.DOM.p({}, "or"),
				React.DOM.div({'key':'fields'},[
					React.DOM.textarea({
						'key':'input-textarea',
						'name':'input-textarea',
						'ref':'input-textarea',
						'placeholder':"Open the JSON file exported from Clipperz in a text editor. Then copy and paste its content here.",
						'value': this.props.importState.importData.input,
						'onChange': this.handleTextareaChange,
						'onDragOver': this.handleOnDragOver,
						'onDrop': this.handleOnDrop,
					}),
				])
			])
		]);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Input = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.InputClass);