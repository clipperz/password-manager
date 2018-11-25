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
Clipperz.Base.module('Clipperz.PM.UI.Components.Cards');

Clipperz.PM.UI.Components.Cards.PasswordGeneratorClass = React.createClass({

	displayName: 'Clipperz.PM.UI.Components.Cards.PasswordGenerator',

	charsetBlocks: {
		'chars_AZ':		'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
		'chars_az':		'abcdefghijklmnopqrstuvwxyz',
		'chars_09':		'0123456789',
		'chars_space':	' ',
		'chars_other':	'~`!@#$%^&*()-_=+,.<>/?[]{}\\|:;\'"'
	},
	
	gradientColors: ['#ff3236', '#e74030', '#cf4e2a', '#b75c24', '#9f6a1e', '#877818', '#6f8612', '#57940c', '#3fa206', '#25ad00', '#25ad00'],

	getInitialState: function () {
		return {
			'length':		this.props['preferences'].getValue('passwordGenerator.length'),
			'options':		'closed',
			'chars_AZ':		this.props['preferences'].getValue('passwordGenerator.characters.A-Z'),
			'chars_az':		this.props['preferences'].getValue('passwordGenerator.characters.a-z'),
			'chars_09':		this.props['preferences'].getValue('passwordGenerator.characters.0-9'),
			'chars_space':	this.props['preferences'].getValue('passwordGenerator.characters.space'),
			'chars_other':	this.props['preferences'].getValue('passwordGenerator.characters.!#?'),

//			'charset':		this.props['preferences'].getValue('passwordGenerator.charset'),
			'charset':	'',
			'password':	'',
			'entropy':	0,
		};
	},

	setPasswordValue: function () {
		this.props['setValueCallback'](this.state['password']);
	},

	toggleOptions: function () {
		var	options;
		
		options = (this.state['options'] == 'closed') ? 'open' : 'closed';
		this.setState({'options':options});
	},
	
	updateCharset: function () {
		var	charsetKeys = ['chars_AZ', 'chars_az', 'chars_09', 'chars_space', 'chars_other'];
		var	self = this;
		var	charset;
		
		charset = MochiKit.Iter.reduce(function (acc, key) { return acc + self.charsetBlocks[key]}, MochiKit.Base.filter(function (key) { return self.state[key] == true;}, charsetKeys), '');

		this.setState({'charset': charset});
		MochiKit.Async.callLater(0.1, this.refreshPasswordValue);
	},

	refreshPasswordValue: function () {
		var	charsetBitSize;
		var	passwordString;
		var	charset;
		var	passwordLength;
		var	randomBytes;
		var	blockIndex;

		charset = this.state['charset'];
		passwordLength = this.state['length'];

		charsetBitSize = 0;
		randomBytes = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(50);
		blockIndex = 0;
		passwordString = "";
		
		while (Math.pow(2, charsetBitSize) < charset.length) {
			charsetBitSize ++;
		}

		if (charsetBitSize > 0) {
//			while (Clipperz.PM.Crypto.passwordEntropy(passwordString) < 128) {
			while (passwordString.length < passwordLength) {
				var	randomValue;

				if (((blockIndex + 1)*charsetBitSize) > (randomBytes.length() * 8)) {
					randomBytes = Clipperz.Crypto.PRNG.defaultRandomGenerator().getRandomBytes(50);
					blockIndex = 0;
				}
				randomValue = randomBytes.bitBlockAtIndexWithSize(blockIndex*charsetBitSize, charsetBitSize);
				if (randomValue < charset.length) {
					passwordString += charset.charAt(randomValue);
				}
	
				blockIndex ++;
			}
		} else {
			passwordString = "";
		}

		this.setState({
			'password': passwordString,
			'entropy': Clipperz.PM.Crypto.passwordEntropy(passwordString)
		});
	},

	refreshEntropyValue: function () {
		this.setState({
			'entropy': Clipperz.PM.Crypto.passwordEntropy(this.state['password'])
		});
	},
	
	handleFormSubmit: function (anEvent) {
		anEvent.preventDefault();
	},

	changeStateWithTargetValue: function (aKey, shouldUpdatePasswordValue) {
		var	self = this;
		return function (anEvent) {
			var	newState = {};

			newState[aKey] = anEvent.target.value;
			self.setState(newState);
			
			if (shouldUpdatePasswordValue) {
				MochiKit.Async.callLater(0.1, self.refreshPasswordValue);
			} else {
				MochiKit.Async.callLater(0.1, self.refreshEntropyValue);
			}
		}
	},

	changeStateWithCheckbox: function (aKey) {
		var	self = this;
		return function (anEvent) {
			var	newState = {};
			
			newState[aKey] = anEvent.target.checked;
			self.setState(newState);

			MochiKit.Async.callLater(0.1, self.updateCharset);
		}
	},

	componentDidMount: function () {
		this.updateCharset();
		this.refreshPasswordValue();
	},

	render: function () {
		var	goodEntropy = 128;
		var	entropyPercentage;
		var	entropyWidth;
		var	entropyColor;
		var	result;

		entropyPercentage = Math.min(this.state['entropy'] / goodEntropy * 100, 100);
		entropyWidth =  (100 - entropyPercentage)+ '%';
		entropyColor = this.gradientColors[Math.floor(entropyPercentage / 10)];

		result = React.DOM.div({'className':'passwordGenerator'}, [
			React.DOM.div({'className':'passwordGeneratorMask', 'onClick':this.props['closeClallback']}),
			React.DOM.div({'className':'passwordGeneratorBaloon'}, [
				React.DOM.form({'onSubmit':this.handleFormSubmit}, [
					React.DOM.div({'className':'optionsWrapper'}, [
						React.DOM.header({}, [
							React.DOM.div({'className':'button', 'onClick':this.toggleOptions}, "options")
						]),
						React.DOM.div({'className':'options ' + this.state['options']}, [
							React.DOM.div({'className':'length'}, [
								React.DOM.span({}, "length"),
								React.DOM.input({'type':'number', 'placehoder':"", 'value':this.state['length'], 'min':"1", 'max':"99", 'onChange':this.changeStateWithTargetValue('length', true), 'ref':'length'}),
							]),
							React.DOM.div({'className':'charList'}, [
								React.DOM.span({}, "characters"),
								React.DOM.div({'className':'charsetSets'}, [
									React.DOM.label({}, [ React.DOM.input({'type':'checkbox', 'checked':this.state['chars_AZ'], 'onChange':this.changeStateWithCheckbox('chars_AZ')}), "A-Z"]),
									React.DOM.label({}, [ React.DOM.input({'type':'checkbox', 'checked':this.state['chars_az'], 'onChange':this.changeStateWithCheckbox('chars_az')}), "a-z"]),
									React.DOM.label({}, [ React.DOM.input({'type':'checkbox', 'checked':this.state['chars_09'], 'onChange':this.changeStateWithCheckbox('chars_09')}), "0-9"]),
									React.DOM.label({}, [ React.DOM.input({'type':'checkbox', 'checked':this.state['chars_space'], 'onChange':this.changeStateWithCheckbox('chars_space')}), "space"]),
									React.DOM.label({}, [ React.DOM.input({'type':'checkbox', 'checked':this.state['chars_other'], 'onChange':this.changeStateWithCheckbox('chars_other')}), "!#?"]),
								]),
								Clipperz.PM.UI.Components.Cards.TextArea({'rows':'1', 'value':this.state['charset'], 'onChange':this.changeStateWithTargetValue('charset', true)})
							]),
						])
					]),
					React.DOM.div({'className':'passwordValue'}, [
						React.DOM.div({'className':'button generatePassword', 'onClick':this.refreshPasswordValue}, "generate password"),
						React.DOM.div({'className':'passwordWrapper'}, [
							Clipperz.PM.UI.Components.Cards.TextArea({'rows':'1', 'value':this.state['password'], 'onChange':this.changeStateWithTargetValue('password', false)}),
							React.DOM.div({'className':'entropyWrapper', 'style':{'backgroundColor':entropyColor}}, [ React.DOM.div({'className':'entropy', 'style':{'width': entropyWidth}})])
						]),
						React.DOM.div({'className':'button setPasswordValue', 'onClick':this.setPasswordValue},     "set password"),
					]),
				])
			])
		]);
		
		return result;
	},

	//=========================================================================
});

Clipperz.PM.UI.Components.Cards.PasswordGenerator = React.createFactory(Clipperz.PM.UI.Components.Cards.PasswordGeneratorClass);