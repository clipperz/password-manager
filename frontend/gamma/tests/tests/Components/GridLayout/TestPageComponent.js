/*

Copyright 2008-2011 Clipperz Srl

This file is part of Clipperz's Javascript Crypto Library.
Javascript Crypto Library provides web developers with an extensive
and efficient set of cryptographic functions. The library aims to
obtain maximum execution speed while preserving modularity and
reusability.
For further information about its features and functionalities please
refer to http://www.clipperz.com

* Javascript Crypto Library is free software: you can redistribute
  it and/or modify it under the terms of the GNU Affero General Public
  License as published by the Free Software Foundation, either version
  3 of the License, or (at your option) any later version.

* Javascript Crypto Library is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Affero General Public License for more details.

* You should have received a copy of the GNU Affero General Public
  License along with Javascript Crypto Library.  If not, see
  <http://www.gnu.org/licenses/>.

*/

Clipperz.Base.module('Clipperz.Tests.GridLayout');

Clipperz.Tests.GridLayout.TestPageComponent = function(args) {
	args = args || {};
	Clipperz.Tests.GridLayout.TestPageComponent.superclass.constructor.call(this, args);

	this._element = args.element || null;

	this._slots = {
		'cardList' : this.getId('cardList'),
		'cardGrid' : this.getId('cardGrid')
	};

	this.render();


	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.Tests.GridLayout.TestPageComponent, Clipperz.PM.UI.Common.Components.BaseComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.Tests.GridLayout.TestPageComponent component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'ul', cls:'testTabs', style:'', children:[
				{tag:'li', id:this.getId('list'), children:[{tag:'span', html:'list'}]},
				{tag:'li', id:this.getId('grid'), children:[{tag:'span', html:'grid'}]}
			]},
			{tag:'ul', cls:'testPanels', children:[
				{tag:'li', id:this.getId('listPanel'), children:[
					{tag:'div', /*id:this.getId('mainPanels'),*/ cls:'mainPanels', children:[
						{tag:'div', id:this.getId('cardList'), cls:'gridComponent cardGrid'}
					]}
				]},
				{tag:'li', id:this.getId('gridPanel'), children:[
					{tag:'div', /*id:this.getId('mainPanels'),*/ cls:'mainPanels', children:[
//						{tag:'div', id:this.getId('cardGrid'), cls:'gridComponent cardGrid'}
						{tag:'h1', html:"Ciao"}
					]}
				]}
			]}
		]);

//		this.append(this.element(), {tag:'div', id:this.getId('mainPanels'), cls:'mainPanels'});
//		this.append(this.getId('mainPanels'), {tag:'div', id:this.getId('cardGrid'), cls:'gridComponent cardGrid'});

		new Clipperz.PM.UI.Common.Controllers.TabPanelController({
			component:this,
			configuration: {
				'LIST': {
					tab:	'list',
					panel:	'listPanel'
				},
				'GRID': {
					tab:	'grid',
					panel:	'gridPanel'
				}
			}
		});
	},

	//-------------------------------------------------------------------------

	__syntaxFix__: "syntax fix"
});
