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

//#############################################################################

Clipperz.PM.UI.Web.Components.DirectLoginsColumnManager = function(args) {
	args = args || {};
	Clipperz.PM.UI.Web.Components.DirectLoginsColumnManager.superclass.constructor.call(this, args);
	
	this._enterLeaveCounter = 0;
	this._selectedRowObject = null;

	return this;
}

//=============================================================================

Clipperz.Base.extend(Clipperz.PM.UI.Web.Components.DirectLoginsColumnManager, Clipperz.PM.UI.Web.Components.ColumnManager, {

	'toString': function () {
		return "Clipperz.PM.UI.Web.Components.DirectLoginsColumnManager component";
	},
	
	//-------------------------------------------------------------------------

	'renderHeader': function(aTRElement) {
		Clipperz.PM.UI.Web.Components.DirectLoginsColumnManager.superclass.renderHeader.call(this, aTRElement);

		Clipperz.DOM.Helper.append(MochiKit.DOM.currentDocument().body, {tag:'div', cls:'DirectLoginListPopup', id:this.getId('DirectLoginListPopup'), children:[
			{tag:'div', cls:'DirectLoginListPopup_body', children:[
				{tag:'ul', id:this.getId('DirectLoginListPopup_list'), children:[
//					{tag:'li', children:[
//						{tag:'img', cls:'favicon', src:'http://www.microsoft.com/favicon.ico'},
//						{tag:'a', href:'#', html:"Google Mail"}
//					]},
//					...
				]}
			]},
			{tag:'div', cls:'DirectLoginListPopup_footer'}
		]});

		MochiKit.Style.hideElement(this.getId('DirectLoginListPopup'));

		//	BEWARE: use MochiKit.Signal.connect instead of this.connectEvent, as the HEADER is NOT redrawn after each refresh
		MochiKit.Signal.connect(this.getId('DirectLoginListPopup'), 'onmouseenter', this, 'handleDirectLoginListPopupEnter');
		MochiKit.Signal.connect(this.getId('DirectLoginListPopup'), 'onmouseleave', this, 'handleDirectLoginListPopupLeave');
	},
	
	//-------------------------------------------------------------------------

	'renderCell': function(aRowElement, anObject) {
		var i,c;
		var directLoginsInfo;
		
		directLoginsInfo = anObject[this.name()];
		
		TDElement = Clipperz.DOM.Helper.append(aRowElement, {tag:'td', cls:'card_directLogins'});
		
		c = Math.min(2, directLoginsInfo.length);
		for (i=0; i<c; i++) {
			var elementID;
			
			elementID = 'directLogin_' + directLoginsInfo[i]['_reference'];
	
			Clipperz.DOM.Helper.append(TDElement, {tag:'div',  cls:'card_directLogin', children:[
				{tag:'div', cls:'card_directLogin_body', children:[
					{tag:'a', href:'#', id:elementID, html:directLoginsInfo[i]['label']}
				]}
			]});
//			MochiKit.Signal.connect(elementID, 'onclick', MochiKit.Base.method(this, 'handleDirectLoginClick', directLoginsInfo[i]['_rowObject']));
			this.connectEvent(elementID, 'onclick', MochiKit.Base.method(this, 'handleDirectLoginClick', directLoginsInfo[i]['_rowObject']));
		}
		
		if (directLoginsInfo.length > 2) {
			var ellipsesElement;

			ellipsesElement =  Clipperz.DOM.Helper.append(TDElement, {tag:'div', cls:'card_directLogin_ellipses', children:[
				{tag:'div', cls:'card_directLogin_ellipses_body', children:[
					{tag:'span', html:'&hellip;'}
				]}
			]});

///			MochiKit.Signal.connect(ellipsesElement, 'onmouseenter', MochiKit.Base.method(this, 'handleEllipsesEnter', anObject));
///			MochiKit.Signal.connect(ellipsesElement, 'onmouseleave', MochiKit.Base.method(this, 'handleEllipsesLeave'));
//			MochiKit.Signal.connect(TDElement, 'onmouseleave', MochiKit.Base.method(this, 'handleTDLeave'));
			this.connectEvent(TDElement, 'onmouseleave', MochiKit.Base.method(this, 'handleTDLeave'));
		}
//		MochiKit.Signal.connect(TDElement, 'onmouseenter', MochiKit.Base.method(this, 'handleTDEnter', anObject));
		this.connectEvent(TDElement, 'onmouseenter', MochiKit.Base.method(this, 'handleTDEnter', anObject));

	},

	//=========================================================================
/*
	'handleEllipsesEnter': function (aRecordInfo, anEvent) {
		this._enterLeaveCounter ++;
		if (this._enterLeaveCounter > 2) {
			this._enterLeaveCounter = 2;
		}
		
		if (this._enterLeaveCounter == 1) {
			this.showDirectLoginListPopup(aRecordInfo, anEvent.src());
		}
	},

	'handleEllipsesLeave': function (anEvent) {
		this._enterLeaveCounter --;
		
		MochiKit.Async.callLater(0.3, MochiKit.Base.bind(function () {
			if (this._enterLeaveCounter == 0) {
				this.hideDirectLoginListPopup();
			}
		}, this))
	},
*/
	//-------------------------------------------------------------------------

	'handleTDEnter': function (aRecordInfo, anEvent) {
		if (MochiKit.Selector.findChildElements(anEvent.src(), ['div.card_directLogin_ellipses']).length > 0) {
			this._enterLeaveCounter ++;
			if (this._enterLeaveCounter > 2) {
				this._enterLeaveCounter = 2;
			}
	
			if (this._enterLeaveCounter == 1) {
				this.showDirectLoginListPopup(aRecordInfo, anEvent.src());
			}
		} else {
			if (this._selectedRowObject != null) {
				this.hideDirectLoginListPopup();
			}
		}
	},

	'handleTDLeave': function (anEvent) {
		this._enterLeaveCounter --;
		if (this._enterLeaveCounter < 0) {
			this._enterLeaveCounter = 0;
		}
		
		MochiKit.Async.callLater(0.3, MochiKit.Base.bind(function () {
			if (this._enterLeaveCounter == 0) {
				this.hideDirectLoginListPopup();
			}
		}, this))
	},

	//-------------------------------------------------------------------------

	'handleDirectLoginListPopupEnter': function (anEvent) {
		this._enterLeaveCounter ++;
		if (this._enterLeaveCounter > 2) {
			this._enterLeaveCounter = 2;
		}
	},
	
	'handleDirectLoginListPopupLeave': function (anEvent) {
		this._enterLeaveCounter --;
		if (this._enterLeaveCounter < 0) {
			this._enterLeaveCounter = 0;
		}
		
		MochiKit.Async.callLater(0.3, MochiKit.Base.bind(function () {
			if (this._enterLeaveCounter == 0) {
				this.hideDirectLoginListPopup();
			}
		}, this))
	},

	//=========================================================================

	'showDirectLoginListPopup': function (aRecordInfo, anElement) {
		var position;
		var directLoginsInfo;
		var directLoginsListElement;
		var ellipsesElement;
		
		
		ellipsesElement = MochiKit.Selector.findChildElements(anElement, ['div.card_directLogin_ellipses'])[0];
		position = MochiKit.Style.getElementPosition(ellipsesElement);
//		position = MochiKit.Style.getElementPosition(anElement);
		position.x += 14;
		position.y -= 26;

		MochiKit.Style.setElementPosition(this.getId('DirectLoginListPopup'), position /*[, units='px'] */);

		directLoginsListElement = this.getElement('DirectLoginListPopup_list');
		directLoginsListElement.innerHTML = '';
		
		directLoginsInfo = aRecordInfo[this.name()];
		c = directLoginsInfo.length;
		for (i=0; i<c; i++) {
			var	elementID;
			var	label;
			var trunkedLabel;
			
			label = directLoginsInfo[i]['label'];
			trunkedLabel = (label.length > 20 ? label.slice(0,20) + '&hellip;' : label);
			
			elementID = this.getId('directLoginList_' + directLoginsInfo[i]['_reference']);
			
			Clipperz.DOM.Helper.append(directLoginsListElement, {tag:'li', children:[
				{tag:'div', children:[
					{tag:'img', cls:'favicon', src:directLoginsInfo[i]['favicon']},
					(label == trunkedLabel ? {tag:'a', href:'#', id:elementID, html:trunkedLabel} : {tag:'a', href:'#', id:elementID, title:label, html:trunkedLabel})
				]}
			]});

//			MochiKit.Signal.connect(elementID, 'onclick', MochiKit.Base.method(this, 'handleDirectLoginClick', directLoginsInfo[i]['_rowObject']));
			this.connectEvent(elementID, 'onclick', MochiKit.Base.method(this, 'handleDirectLoginClick', directLoginsInfo[i]['_rowObject']));
		}

//		MochiKit.Style.showElement(this.getId('DirectLoginListPopup'));
		MochiKit.Visual.appear(this.getId('DirectLoginListPopup'), {duration:0.5});
		MochiKit.Signal.signal(this, 'selectRow', aRecordInfo);
		
		this._selectedRowObject = aRecordInfo;
	},

	//-------------------------------------------------------------------------
	
	'hideDirectLoginListPopup': function () {
		if (this._selectedRowObject != null) {
			MochiKit.Signal.signal(this, 'unselectRow', this._selectedRowObject);
			MochiKit.Visual.fade(this.getId('DirectLoginListPopup'), {duration:0.5});
			this._selectedRowObject = null;
			this._enterLeaveCounter = 0;
		}
	},

	//=========================================================================

	'handleDirectLoginClick': function (aDirectLogin, anEvent) {
		anEvent.preventDefault();
//		aDirectLogin.runDirectLogin();
		Clipperz.PM.UI.Common.Controllers.DirectLoginRunner.openDirectLogin(aDirectLogin);
	},

	//-------------------------------------------------------------------------

	'__syntax_fix__' : 'syntax fix'
	
});

