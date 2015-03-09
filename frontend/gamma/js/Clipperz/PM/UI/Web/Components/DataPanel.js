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

Clipperz.Base.module('Clipperz.PM.UI.Web.Components');

Clipperz.PM.UI.Web.Components.DataPanel = function(args) {
	args = args || {};

	Clipperz.PM.UI.Web.Components.DataPanel.superclass.constructor.apply(this, arguments);

	this._initiallySelectedTab = args.selected || 'OFFLINE_COPY';
	this._tabPanelControllerConfiguration = {
		'OFFLINE_COPY': {
			tab:	'offlineCopyTab',
			panel:	'offlineCopyPanel'
		},
		'SHARING': {
			tab:	'sharingTab',
			panel:	'sharingPanel'
		},
		'IMPORT': {
			tab:	'importTab',
			panel:	'importPanel'
		},
		'EXPORT': {
			tab:	'exportTab',
			panel:	'exportPanel'
		}
	};
	
	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DataPanel, Clipperz.PM.UI.Common.Components.TabPanelComponent, {

	//-------------------------------------------------------------------------

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DataPanel component";
	},

	//-------------------------------------------------------------------------

	'renderSelf': function(/*aContainer, aPosition*/) {
		this.append(this.element(), [
			{tag:'div', cls:'header', children:[
				{tag:'div', cls:'subPanelTabs', children:[
					{tag:'ul', children:[
						{tag:'li', id:this.getId('offlineCopyTab'),	children:[{tag:'a', href:'#', html:'Offline copy'}], cls:'first'},
						{tag:'li', id:this.getId('sharingTab'),		children:[{tag:'a', href:'#', html:'Sharing'}]},
						{tag:'li', id:this.getId('importTab'),		children:[{tag:'a', href:'#', html:'Import'}]},
						{tag:'li', id:this.getId('exportTab'),		children:[{tag:'a', href:'#', html:'Export'}]}
					]}
				]}
			]},
			{tag:'div', cls:'body', children:[
				{tag:'div', cls:'accountPanel', children:[
					{tag:'div', cls:'subPanelContent', children:[
						{tag:'ul', children:[
							{tag:'li', id:this.getId('offlineCopyPanel'),	children:[
//								{tag:'h3', html:"Offline copy"},
								{tag:'p', html:"With just one click you can dump all your encrypted data from Clipperz servers to your hard disk and create a read-only offline version of Clipperz to be used when you are not connected to the Internet."},
								{tag:'a', id:this.getId('offlineCopyDownloadLink'), href:'#', html:"Download", cls:'downloadOfflineCopy'}
							]},
							{tag:'li', id:this.getId('sharingPanel'),	children:[
//								{tag:'h3', html:"Sharing"}
							]},
							{tag:'li', id:this.getId('importPanel'),		children:[
//								{tag:'h3', html:"Import"}
							]},
							{tag:'li', id:this.getId('exportPanel'),		children:[
//								{tag:'h3', html:"Export"}
							]}
						]}
					]}
				]}
			]},
			{tag:'div', cls:'footer'}
		]);

		this.tabPanelController().setup({selected:this.initiallySelectedTab()});
		MochiKit.Signal.connect(this.getId('offlineCopyDownloadLink'), 'onclick', this, 'downloadOfflineCopy');
	},

	'downloadOfflineCopy': function (anEvent) {
		anEvent.preventDefault();
		MochiKit.Signal.signal(Clipperz.Signal.NotificationCenter, 'downloadOfflineCopy', anEvent.src());
	},

	//-------------------------------------------------------------------------
	__syntaxFix__: "syntax fix"
});
