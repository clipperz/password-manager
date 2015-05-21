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

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.PreviewClass = React.createClass({

	getInitialState: function() {
		if (this.props.importContext.format == 'csv') {
			return this.props.importContext.processCsv()
		} else {
			return {
				'jsonToImport': this.props.importContext.jsonToImport,
				'recordsToImport': this.props.importContext.recordsToImport,
			}
		}
	},
	
	componentDidMount() {
		this.props.setNextStepCallback(this.handleImport);
	},
	
	//-------------------------------------------------------------------------

	handleImport: function() {
		MochiKit.Base.update(this.props.importContext, this.state);

		var filteredImportData = MochiKit.Base.filter(
			MochiKit.Base.bind(function(r) {
				return this.isRecordToImport(r);
			}, this),
			this.state.jsonToImport
		);
		
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'importCards', filteredImportData);
		
		return true;
	},
	
	//=========================================================================
	
	toggleRecordToImport: function(record) {
		var newRecordsToImport;
		var recordPosition;

		newRecordsToImport = this.state.recordsToImport;
		recordPosition = newRecordsToImport.indexOf(record._importId);
		
		if (recordPosition === -1) {
			newRecordsToImport.push(record._importId);
		} else {
			newRecordsToImport.splice(recordPosition,1);
		}
		
		this.setState({'recordsToImport': newRecordsToImport});
	},
	
	isRecordToImport: function(record) {
		return (this.state.recordsToImport.indexOf(record._importId)>=0) ? true : false;
	},

	getTags: function (aTitle) {
		var result;
		var tagList;
		
		var tagObject = Clipperz.PM.DataModel.Record.extractTagsFromFullLabel(aTitle);
		
		tagList = MochiKit.Base.keys(tagObject);
		tagList = MochiKit.Base.filter(function(aTag) { return tagObject[aTag] }, tagList);
		
		if (tagList.length > 0) {
			result = React.DOM.ul({'className': 'tagList'},
				MochiKit.Base.map(function(aTag){
					return React.DOM.li({}, aTag);
				}, tagList)
			);
		} else {
			result = null;
		}
		
		return result;
	},
	
	renderCardFields: function(someFields) {
		return MochiKit.Base.map(function(key) {
			var field = someFields[key];
			
			return [
				React.DOM.dt({},field.label),			
				React.DOM.dd({},field.value),			
			];
		} ,MochiKit.Base.keys(someFields));
	},
	
	renderCard: function(aCard) {
		var notesParagraph = (aCard.data.notes) ? React.DOM.p({'className': 'notes'}, aCard.data.notes) : null;
		return React.DOM.li({'className': 'card'}, [
			React.DOM.input({
				'type': 'checkbox',
				'checked': this.isRecordToImport(aCard),
				'onChange': MochiKit.Base.partial(this.toggleRecordToImport,aCard)
			}),
			React.DOM.h3({}, Clipperz.PM.DataModel.Record.extractLabelFromFullLabel(aCard.label)),
			this.getTags(aCard.label),
			React.DOM.dl({'className': 'fields'}, this.renderCardFields(aCard.currentVersion.fields)),
			notesParagraph
		]);
	},

	render: function() {
		var result;
		
		if (typeof(this.state.jsonToImport)=='undefined' || !this.state.jsonToImport) {
			result = "Error";
		} else {
			var renderedPreview = React.DOM.ul({},
				MochiKit.Base.map(this.renderCard, this.state.jsonToImport)
			);
			
			result =
				React.DOM.div({'className': 'jsonPreview'}, React.DOM.ul({},
					MochiKit.Base.map(this.renderCard, this.state.jsonToImport)
				) );
		}
			
		return React.DOM.div({},result);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Preview = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.PreviewClass);