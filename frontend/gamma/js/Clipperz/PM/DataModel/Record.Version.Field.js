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

try { if (typeof(Clipperz.PM.DataModel.Record.Version) == 'undefined') { throw ""; }} catch (e) {
	throw "Clipperz.PM.DataModel.Record.Version.Field depends on Clipperz.PM.DataModel.Record.Version!";
}  

Clipperz.PM.DataModel.Record.Version.Field = function(args) {
	Clipperz.PM.DataModel.Record.Version.Field.superclass.constructor.apply(this, arguments);

	this._recordVersion	= args.recordVersion	|| Clipperz.Base.exception.raise('MandatoryParameter');
	this._reference		= args.reference		|| Clipperz.PM.Crypto.randomKey();

	return this;
}


Clipperz.Base.extend(Clipperz.PM.DataModel.Record.Version.Field, Object, {

	'toString': function() {
		return "Record.Version.Field (" + this.reference() + ")";
	},

	//-------------------------------------------------------------------------

	'recordVersion': function () {
		return this._recordVersion;
	},

	//-------------------------------------------------------------------------

	'reference': function () {
		return this._reference;
	},
	
	//-------------------------------------------------------------------------

	'getItem': function (aKey) {
		return Clipperz.Async.callbacks("Clipperz.PM.DataModel.Record.Version.Field.getItem", [
			MochiKit.Base.method(this, 'recordVersion'),
			MochiKit.Base.methodcaller('getValue', 'fields' + '.' + this.reference() + '.' + aKey)
		], {trace:false});
	},

	'setItem': function (aKey, aValue) {
		return Clipperz.Async.callbacks("Clipperz.PM.DataModel.Record.Version.Field.getItem", [
			MochiKit.Base.method(this, 'recordVersion'),
			MochiKit.Base.methodcaller('setValue', 'fields' + '.' + this.reference() + '.' + aKey, aValue)
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'label': function () {
		return this.getItem('label');
	},

	'setLabel': function (aValue) {
		return this.setItem('label', aValue);
	},
	
	//-------------------------------------------------------------------------

	'value': function () {
		return this.getItem('value');
	},

	'setValue': function (aValue) {
		return this.setItem('value', aValue);
	},

	//-------------------------------------------------------------------------

	'actionType': function () {
		return Clipperz.Async.callbacks("Clipperz.PM.DataModel.Record.Version.Field.actionType", [
			Clipperz.Async.collectResults("Clipperz.PM.DataModel.Record.Version.Field.actionType [collect results]", {
				'isHidden':	MochiKit.Base.method(this, 'isHidden'),
				'value':	MochiKit.Base.method(this, 'value')
			}, {trace:false}),
			function (someValues) {
				var result;		//	'NONE', 'URL', 'EMAIL', 'PASSWORD'

				result = 'NONE';

				if (someValues['isHidden']) {
					result = 'PASSWORD';
				} else if (Clipperz.Base.isUrl(someValues['value'])) {
					result = 'URL';
//				} else if (Clipperz.Base.isBitcoin(someValues['value'])) {
//					result = 'BITCOIN';
				} else if (Clipperz.Base.isEmail(someValues['value'])) {
					result = 'EMAIL';
				};

				return result;
			}
		], {trace:false});
	},

	//-------------------------------------------------------------------------

	'isHidden': function () {
		return this.getItem('hidden');
	},
	
	'setIsHidden': function (aValue) {
		return this.setItem('hidden', aValue);
	},

	//-------------------------------------------------------------------------

	'isEmpty': function () {
		var deferredResult;
		
		deferredResult = new Clipperz.Async.Deferred("Clipperz.PM.DataModel.Record.Version.Field.isEmpty", {trace:false});

		deferredResult.collectResults({
			'label': [
				MochiKit.Base.method(this, 'label'),
				MochiKit.Base.partial(MochiKit.Base.operator.eq, '')
			],
			'value': [
				MochiKit.Base.method(this, 'value'),
				MochiKit.Base.partial(MochiKit.Base.operator.eq, '')
			],
			'isHidden': [
				MochiKit.Base.method(this, 'isHidden'),
				MochiKit.Base.partial(MochiKit.Base.operator.eq, false)
			]
		});
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(function(someValues) {
			return MochiKit.Iter.every(someValues, MochiKit.Base.operator.identity);
		});
		deferredResult.callback();

		return deferredResult;
	},
	
	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});


