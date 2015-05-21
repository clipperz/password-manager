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
	
	getInitialState: function() {
		return {
			'inputString': (this.props.importContext.inputString) ? this.props.importContext.inputString : null,
			'format': (this.props.importContext.format) ? this.props.importContext.format : null,
			//'parsedInput': (this.props.importContext.parsedInput) ? this.props.importContext.parsedInput : null,
		};
	},
	
	componentDidMount: function() {
		this.updateNextStatus(this.state.inputString);
	},

	//-------------------------------------------------------------------------
	
	handleNextStep: function() {
		var result;
		var jsonData;
		var parsedInput;
		
		var inputString = this.refs['input-textarea'].getDOMNode().value.trim();
		
		result = {'inputString': inputString};
		
		parsedInput = this.parseJson(inputString);
		if (parsedInput) {
			MochiKit.Base.update(result,this.props.importContext.getInitialJsonContext(parsedInput));
		} else {
			parsedInput = this.parseCsv(inputString);
			if (parsedInput) {
				MochiKit.Base.update(result, this.props.importContext.getInitialCsvContext(parsedInput));
			} else {
				result = false;
			}
		}
		
		return result;
	},
	
	updateNextStatus: function(newInputString) {
		this.props.setNextStepCallback((newInputString) ? this.handleNextStep : null);
	},
	
	//=========================================================================
	
	extractJsonFromClipperzExport: function(someHtml) {
		var textarea;
		var regexMatch;
		var result;
		
		var re = new RegExp('.*<textarea>(.*)<\/textarea>.*','g');
		
		if (re.test(someHtml)) {
			textarea =  this.refs['input-textarea'].getDOMNode();
			textarea.innerHTML = someHtml.replace(re, '$1');
			result = textarea.innerHTML;
		} else {
			result = false;
		}

		return result;
	},
	
	addImportIds: function (someJson) {
		var count;
		
		for (count=0; count < someJson.length; count++) {
			someJson[count]['_importId'] = count;
		}
	},
	
	parseJson: function(aJsonString) {
		var result;
		var jsonData;
		
		try {
			jsonData = JSON.parse(aJsonString);
			this.addImportIds(jsonData);
			result = jsonData;
		} catch(e) {
			result = false;
		}
		
		return result;
	},
	
	parseCsv: function(aCsvString) {
		var result;
		var i;
	
		var parsedCsv = Papa.parse(aCsvString);
		
		if (parsedCsv.errors.length != 0) {
			result = false;
		} else {
			result = this.csvFillEmptyCells(parsedCsv.data);
		}

		return result;
	},

	csvFillEmptyCells: function(table) {
		var i,j;
		
		var result = [];
		
		var maxColumns = MochiKit.Iter.reduce(function(prev,next) {
			return Math.max(prev,next)
		}, MochiKit.Base.map(function(row) {return row.length;}, table) );
		
		for (i=0; i<table.length; i++) {
			
			result[i] = [];
			for (j=0; j<maxColumns; j++) {
				result[i][j] = (typeof(table[i][j]) != "undefined") ? table[i][j] : "";
			}
		}
		
		return result;
	},
	
	//=========================================================================
	
	handleUploadFiles: function(someFiles) {
		var file;
		var reader;
		
		if (someFiles.length == 1) {
			file = someFiles[0];
				reader = new FileReader();
				
				// Binary files are just thrown in the textarea as weird UTF-8 characters: should we do something about it?
				reader.onloadend = MochiKit.Base.bind(function() {
					var extractedJson = this.extractJsonFromClipperzExport(reader.result);
					var newInputString;
					
					if (extractedJson) {
						newInputString = extractedJson;
					} else {
						newInputString = reader.result;
					}

					this.setState({'inputString': newInputString});
					this.updateNextStatus(newInputString);
				},this,reader);
				
				reader.readAsText(file);
		} else {
			// Should this be removed?
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
	
	handleTextareaChange: function() {
		var newInputString = this.refs['input-textarea'].getDOMNode().value;
		this.setState({'inputString': newInputString});
		this.updateNextStatus(newInputString);
	},

	//=========================================================================
	
	render: function() {
		return React.DOM.div({},[
			React.DOM.form({'key':'form', 'className':'importForm' }, [
				React.DOM.input({
					'type': 'file',
					'ref': 'upload-input',
					'onClick': function(e) { e.target.value = null },
					'onChange': this.handleInputFiles,
					'style': {'display': 'none'}
				}),
				React.DOM.div({
					'onDragOver': this.handleOnDragOver,
					'onDrop': this.handleOnDrop,
					'onClick': MochiKit.Base.bind(function() { this.refs['upload-input'].getDOMNode().click() }, this),
					'className': 'dropArea'
				}, "Drag your Clipperz export file here or click select it manually."),
				React.DOM.p({}, "or"),
				React.DOM.div({'key':'fields'},[
					React.DOM.textarea({
						'key':'input-textarea',
						'name':'input-textarea',
						'ref':'input-textarea',
						'placeholder':"Open the JSON file exported from Clipperz in a text editor. Then copy and paste its content here.",
						'value': this.state.inputString,
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