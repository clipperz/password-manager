/*

Copyright 2008-2018 Clipperz Srl

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

	displayName: 'Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Preview',

	getInitialState: function() {
		var	recordsToImport;

		recordsToImport = MochiKit.Iter.reduce(
			function (acc, item) { acc[item['reference']] = item; return acc; },
			MochiKit.Base.filter(
				function (aRecord) { return !Clipperz.PM.DataModel.Record.labelContainsArchiveTag(aRecord['label']); },
				this.props.importContext.state('recordsToImport')
			),
			{}
		);

		this.props.importContext.setState('recordsToImport', MochiKit.Base.values(recordsToImport));

		return {
			'recordsToImport': recordsToImport
		};
	},

	//=========================================================================

	selectRecordsToImport: function(records) {
		var newRecordsToImport;
		var i;

		newRecordsToImport = {};
		for (i in records) {
			newRecordsToImport[records[i]['reference']] = records[i];
		}

		this.setState({'recordsToImport': newRecordsToImport});
		this.props.importContext.setState('recordsToImport', MochiKit.Base.values(newRecordsToImport));
	},

	selectAll: function() {
		this.selectRecordsToImport(this.props.importContext.state('jsonData'));
	},

	selectNone: function() {
		this.selectRecordsToImport({});
	},

	selectNotArchived: function() {
		this.selectRecordsToImport(MochiKit.Base.filter(function (aRecord) {
			return !Clipperz.PM.DataModel.Record.labelContainsArchiveTag(aRecord['label']);
		}, this.props.importContext.state('jsonData')));
	},

	selectArchived: function() {
		this.selectRecordsToImport(MochiKit.Base.filter(function (aRecord) {
			return Clipperz.PM.DataModel.Record.labelContainsArchiveTag(aRecord['label']);
		}, this.props.importContext.state('jsonData')));
	},

	//-------------------------------------------------------------------------

	handleImportTagTextChange: function() {
		var newTag = this.refs['importTagText'].value;

		this.props.importContext.setState('importTag', newTag);
	},

	handleImportTagCheckboxChange: function() {
		this.props.importContext.setState('useImportTag', ! this.props.importContext.state('useImportTag'));
	},

	//-------------------------------------------------------------------------

	toggleRecordToImport: function(record) {
		var newRecordsToImport;
		var recordPosition;

		newRecordsToImport = this.state.recordsToImport;

		if (this.isRecordToImport(record)) {
			delete newRecordsToImport[record['reference']];
		} else {
			newRecordsToImport[record['reference']] = record;
		}

		this.setState({'recordsToImport': newRecordsToImport});
		this.props.importContext.setState('recordsToImport', MochiKit.Base.values(newRecordsToImport));
	},

	isRecordToImport: function(record) {
		return (MochiKit.Base.keys(this.state.recordsToImport).indexOf(record['reference']) != -1) ? true : false;
	},

	getTags: function (aTitle) {
		var result;
		var tagList;
		
		var tagObject = Clipperz.PM.DataModel.Record.extractTagsFromFullLabel(aTitle);

		tagList = MochiKit.Base.keys(tagObject);
		tagList = MochiKit.Base.filter(function(aTag) { return !Clipperz.PM.DataModel.Record.isSpecialTag(aTag); }, tagList);

		if (this.props.importContext.state('useImportTag') && this.props.importContext.state('importTag')) {
			tagList.push(this.props.importContext.state('importTag'));
		}

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
	
	renderCardFields: function (someFields) {
		return MochiKit.Base.map(function (key) {
			var field = someFields[key];
			return [
				React.DOM.dt({}, field['label']),
				React.DOM.dd({'className': field['actionType'] + (field['hidden'] ? ' password' : '')}, field['value']),
			];
		}, MochiKit.Base.keys(someFields));
	},

	renderCard: function (aCard) {
		var	classes;
		
		classes = {
			'card': true,
			'archived': Clipperz.PM.DataModel.Record.labelContainsArchiveTag(aCard['label'])
		}
		
		return React.DOM.li({'className':Clipperz.PM.UI.Components.classNames(classes)}, [
			React.DOM.input({
				'type': 'checkbox',
				'checked': this.isRecordToImport(aCard),
				'onChange': MochiKit.Base.partial(this.toggleRecordToImport, aCard)
			}),
			React.DOM.div({'className': 'cardContent'}, [
				React.DOM.h3({}, Clipperz.PM.DataModel.Record.extractLabelFromFullLabel(aCard['label'])),
				this.getTags(aCard['label']),
				React.DOM.dl({'className': 'fields'}, this.renderCardFields(aCard['currentVersion']['fields'])),
				(aCard['data']['notes']) ? React.DOM.p({'className': 'notes'}, aCard['data']['notes']) : null
			])
		]);
	},

	render: function() {
		var inputFormat = this.props.importContext.inputFormat();

		return React.DOM.div({'className': 'preview'},
			React.DOM.div({'className': 'selectButtons'},
				React.DOM.span({}, "Select:"),
				React.DOM.a({'onClick': this.selectAll}, "All"),
				React.DOM.a({'onClick': this.selectNone}, "None"),
				(inputFormat == 'JSON') ? React.DOM.a({'onClick': this.selectNotArchived}, "Not Archived") : null,
				(inputFormat == 'JSON') ? React.DOM.a({'onClick': this.selectArchived}, "Archived") : null
			),
			React.DOM.div({'className': 'tagButtons'},
				React.DOM.input({
					'type': 'checkbox',
					'id': 'tagCheckbox',
					'checked': this.props.importContext.state('useImportTag'),
					'onChange': this.handleImportTagCheckboxChange
				}),
				React.DOM.label({'htmlFor': 'tagCheckbox'}, "Apply the following tag to imported cards:"),
				React.DOM.input({
					'ref': 'importTagText',
					'type': 'text',
					'value': this.props.importContext.state('importTag'),
					'disabled': ! this.props.importContext.state('useImportTag'),
					'onChange': this.handleImportTagTextChange
				})
			),
			React.DOM.ul({}, MochiKit.Base.map(MochiKit.Base.method(this, 'renderCard'), this.props.importContext.state('jsonData')))
		);
	},

});

Clipperz.PM.UI.Components.ExtraFeatures.DataImport.Preview = React.createFactory(Clipperz.PM.UI.Components.ExtraFeatures.DataImport.PreviewClass);