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

if (typeof(Clipperz) == 'undefined') { Clipperz = {}; }
if (typeof(Clipperz.PM) == 'undefined') { Clipperz.PM = {}; }
if (typeof(Clipperz.PM.Components) == 'undefined') { Clipperz.PM.Components = {}; }
if (typeof(Clipperz.PM.Components.Compact) == 'undefined') { Clipperz.PM.Components.Compact = {}; }

Clipperz.PM.Components.Compact.CompactHeader = function(anElement, args) {

    Clipperz.PM.Components.Compact.CompactHeader.superclass.constructor.call(this, anElement, args);

	this.render();
	
	return this;
};

YAHOO.extendX(Clipperz.PM.Components.Compact.CompactHeader, Clipperz.PM.Components.BaseComponent, {
	
	'toString': function() {
		return "Clipperz.PM.Components.Compact.CompactHeader";
	},

	//-----------------------------------------------------
	
	'render': function() {
		this.element().update("");

		Clipperz.YUI.DomHelper.append(this.element().dom, {tag:'div', children:[
			{tag:'img', src:'./images/logo.gif'},
			{tag:'div', id:'lockBlock', children:[
				{tag:'input', type:'checkbox', id:'autolock'},
				{tag:'span', html:'auto'},
				{tag:'a', href:'#', htmlString:Clipperz.PM.Strings['lockMenuLabel'], id:'lock'}
			]}
		]});
		
		Clipperz.YUI.DomHelper.append(this.element().dom, 
			{tag:'div', id:'compactMiscLinks', children:[
				{tag:'a', id:'donateHeaderIconLink', target:'_blank', href:Clipperz.PM.Strings['donateHeaderLinkUrl'], children:[
					{tag:'img', id:'donateHeaderLinkIcon', src:'./images/smiles_small.gif'}
				]},
				{tag:'ul', children:[
					{tag:'li', children:[{tag:'a', id:'donateHeaderLink', html:'donate', target:'_blank'}]},
//					{tag:'li', children:[{tag:'a', id:'creditsHeaderLink', html:'credits', target:'_blank'}]},
					{tag:'li', children:[{tag:'a', id:'feedbackHeaderLink', html:'feedback', target:'_blank'}]},
					{tag:'li', children:[{tag:'a', id:'helpHeaderLink', html:'help', target:'_blank'}]},
					{tag:'li', children:[{tag:'a', id:'forumHeaderLink', html:'forum', target:'_blank'}]}
				]}
			]}
		);

		YAHOO.ext.Element.get('lockBlock').setVisibilityMode(YAHOO.ext.Element.DISPLAY).hide();
		Clipperz.NotificationCenter.notify(this, 'switchLanguage');
	},

	//-----------------------------------------------------
	__syntaxFix__: '__syntaxFix__'
});


