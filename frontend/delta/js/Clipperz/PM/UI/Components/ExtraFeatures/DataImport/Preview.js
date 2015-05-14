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

//	UNCOMMENT AFTER MERGE (uses methods in Record that were added in another branch)	
//	getTags: function (aTitle) {
//		var result;
//		var tagList;
//		
//		var tagObject = Clipperz.PM.DataModel.Record.extractTagsFromFullLabel(aTitle);
//		
//		tagList = MochiKit.Base.keys(tagObject);
//		tagList = MochiKit.Base.filter(function(aTag) { return tagObject[aTag] }, tagList);
//		
//		if (tagList.length > 0) {
//			result = React.DOM.ul({'className': 'tagList'},
//				MochiKit.Base.map(function(aTag){
//					return React.DOM.li({}, aTag);
//				}, tagList)
//			);
//		} else {
//			result = null;
//		}
//		
//		return result;
//	},
	
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
				'checked': this.props.isRecordToImportCallback(aCard),
				'onChange': MochiKit.Base.partial(this.props.toggleRecordToImportCallback,aCard)
			}),
			React.DOM.h3({}, Clipperz.PM.DataModel.Record.filterOutTags(aCard.label)),
//			REMOVE THE PREVIOUS LINE AND UNCOMMENT THE FOLLOWING 2 AFTER MERGE
//			React.DOM.h3({}, Clipperz.PM.DataModel.Record.extractLabelFromFullLabel(aCard.label)),
//			this.getTags(aCard.label),
			React.DOM.dl({'className': 'fields'}, this.renderCardFields(aCard.currentVersion.fields)),
			notesParagraph
		]);
	},

	render: function() {
		var result;
		
		if (! this.props.importState.importData || typeof(this.props.importState.jsonToImport)=='undefined' || !this.props.importState.jsonToImport) {
			result = "Error";
		} else {
			var renderedPreview = React.DOM.ul({},
				MochiKit.Base.map(this.renderCard, this.props.importState.jsonToImport)
			);
			
			result = [
				React.DOM.h2({},"Preview"),
				Clipperz.PM.UI.Components.ExtraFeatures.DataImport.StepsNavigation({
					'format': this.props.importState.importData.format,
					'stepId': 'preview'
				}),
				React.DOM.button({
					'onClick': MochiKit.Base.partial(this.props.goToStepCallback, this.props.importState.previousStep)}, "Back"),
				React.DOM.span({}, " - "),
				React.DOM.button({
					'onClick': MochiKit.Base.bind(function() {
						var filteredImportData = MochiKit.Base.filter(
							MochiKit.Base.bind(function(r) {
								return this.props.isRecordToImportCallback(r);
							}, this),
							this.props.importState.jsonToImport
						);
						
						MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'importCards', filteredImportData);
						
						this.props.resetImportStateCallback();
					}, this)
				}, "Import"),
				React.DOM.div({'className': 'jsonPreview'},renderedPreview),
			];
		}
			
		return React.DOM.div({},result);
	}

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Preview = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.PreviewClass);