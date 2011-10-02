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

Clipperz.Base.module('Clipperz.PM.UI.iPhone.Controllers');

//	Some parts of this controller have been derived from the iUI library.

Clipperz.PM.UI.iPhone.Controllers.MainController = function() {
	this._loginForm =		null;
	this._cardList =		null;
	this._cachedValues =	null;
	this._user =			null;

	if (typeof window.onorientationchange == 'object') {
		MochiKit.Signal.connect(window, 'onorientationchange', this, 'orientationChangeHandler');
		MochiKit.Async.callLater(0, MochiKit.Base.method(this, 'orientationChangeHandler'));
	} else {
		this.setOrientation('portrait');
//		this.setOrientation('landscape');
	}

	this.addMetaTag('viewport', 'width=devicewidth; initial-scale=1.0; maximum-scale=1.0; user-scalable=0;');
	this.addMetaTag('apple-mobile-web-app-capable', 'yes');
	this.addMetaTag('apple-mobile-web-app-status-bar-style', 'black');

	this.addLinkTag('apple-touch-icon', 'data:image/png;charset=utf-8;base64,iVBORw0KGgoAAAANSUhEUgAAAF8AAABfCAYAAACOTBv1AAAACXBIWXMAAAsTAAALEwEAmpwYAAAQC0lEQVR4nO2ce3wU1b3AvzOzz2TzBpKQhIS3CnLxgYoPqAgiQS8igiAqiFprtT571YpKUa9tvVgrVvhc7IXS4lUUvGgR5CWIoiAIYoQgJGDI+0k2yW42szNz7h8pGBHIPmazqeb7+UD4DHt+vzPfPTnnzJkzIwGCLqKCHO0K/JTpkh9FuuRHkS75UaRLfhTpkh9FuuRHkS75UaRLfhTpkh9FLNGuwKnIzc3l9WXLQAIhQILWvwQnjn3vUNtjkoQQ4kSZr77KY+TIkR1/EgHQKeWnp6eTkJiI0P2gqyeOnxD7T+ttxf/wp4SwOOnTp0/UzqM9OqV8oFXygSVIm3/xvePSKf59up/cWfO9z3c2OnmfH646qVMv2XZa+RKtrT8cWruoztv2O6384/15OLSODZ237Xda+W1nMKEiROe+U9Rp5ZtFJ+51Oq/8E/P7MOjE3oFOLL/1Iik8fUJoCF03pT6RoNPKF4Q/2zF0P0J0yQ8aM/pqoesIXQs/UITotPKFYRDuXMXQ/dDV8oNECIShgRGmOKFjGIY5dYoAnVK+QCB0DSNM+Ybmb/0SOylhy8/NzWXYsGFm1OU7hGhd0RThtdrW2Y758kdcdhGLX30h7Dhhr2ouWrSI9PQ0ykrLeHn+fBYsWIDX6w0zqsAwNOQwW77QtbBnTMdJSkzgkfvuZPqkXOJi7SAMLhl2Htt37gk5ZlgtPycnh7TU7oiGo6TH6zz/3DPUVpXz3qp3+Lch54YeWLR2OyLcbkcPf9wY/bNL2fjuMg7uWM09t11PnE1FaSwADB65746wYofV8ufMeQph6MhfvoT09atIWWORBt7GNaOv5urRWyktLeWll//MXxb/FZ/PF3BcgcDQVeRwux1DQxjBz1m7JSfx2IM/Z/J1Y3A5LUiShFK3G3vpWuwVW9Bjs6i/eCGXnjcgrPqF1fJzrxkLuopUuAJJGMhH12LZMA3LG2ej7JxDZmwT//W7uVSVFPDW/y5l4ID+gQUWAmHo4V8g6VrAA64kSYwfM5It//g7+z9Zye2TxxJPDY7CpSRsnUrCzgdwlH2AZPiwNB5CajyMYqjkjhkRcvVCbvnnnzeUBJcDUbIFyVv+/RNprsKSNx/y5mP0uAS53zSuvWoc48duoqi4lD++vIC/vb4cVVVPHVyA0P1hLw0Yhh/EmU8xtXsyv3noHiaMu5xYmwXJ8GMr34ijfB3Wut2nvR3jKN9EU850fnXHNNZs2BpS/UKW/+QTj4GhIxe+fcbPyVXbsVVtR1ifRMueQE7fKbz0u6f5/dzfsGb9JuY8+wJHiopPKmXObAdDwzhFtyPLMhPGjeKRe2cyIDsVCYHiPoCjcB32qi3Imqfd0PaqrTRmTWFw31QURUbXg69ryPKvuOxihN+LXLQ6oM9L/kasBcuwFixDTzwbS+8buf6q8UwYu4bDR8t4cf5/8+bK99C01hmKOfP87/f5PdO6M/vhX3DdmOHYLTKyWo/92zdxlm/A4j0aVGyLrxzFfQDD1ZdpE8exbMX7QdcvJPmjR/2MWJuCVLIBSXUHXV6pz0fZ8yz2vS+gZYymX/ZEXvnDbOY99yjvrd1CwZFiU65wDd0PhsTUieN44O7p9E5PQhI6trrPcJavx1a7E4nQf7uc1Z/gj8nmjptCk39860tQrFu9ksuGDcX+6f1YioNPeir0mEzUrOtQs65FtyYhSWA7uorYr0O/mKm5/C0MJRYhDBRPEc6KjTirNiP7g28wp6yzNZHywfMQsoUBo+7C1+IPqnxILf+ioYMRaj1K2aZQip8SxVuC85uFOA4uwt/9EloyxkOYSwNSyzEcdRtxVm7C2lRoUk2/Q/HXY2vYj881kLumX88ri888/p1M0PKnTr4BRfKjHN2ApAc+dw8USejYqrZhq9qGkJSwYiXv+iVShFc1nXXbaXb2Ydq1VwQtP+h5/r13zUAYOpbiwAbacAhXXKTFA8S492BoPlLjBPGumKDKBiVfURQG9ctCeKuxVH0WVKIfK4rRjLMhD8lQeWDWpKDKBiX/njtngFCxlK7rkFb1r4KrYTeG5ue6kcGtZwUlf8bUCWBo2ErWBZXkx06MJx+hNZFg8dCzR1LA5QKWH+N0kpOeCJ4yLPVfhVTJHyuy0HA17Qeh8+Cs6wMvF+gHH77vTjA0JG85emx2SJX8saLJsaA3I3Q/V52fE3C5gKeaY6+6AmH4UV39qRu2EMVThL1qK47qj7F4S0Kp8780ftlFo3MATc6z8Fp6Ht/ZS0xiMg6bFZ/a/gVXwFe4TqeTqVMmMevm6zk7pwea9xhCGCAEFk8RjpptOGo/xdJcFuZpdV78sotGxwAanQNptvZEIJAkGUtiDnuLmnht+Xo2bNoS8E37kJYXYmNjmXbTjcy8aTwDMpLRmmtb1+CFQPEcIaZuB87a7VhaKoMN3enwy3E0OgfQYO9PsyWt1ZikYEnKIa+oidfe2sj6jZtD2iURkvy2uFwupk+9kRmTc+mT7kLz1CAMAQgsniPEHNtJzLGdWNTqcNJ0KKoST6O9Pw2O/visaQjDQJJlrEl9+KrYy+IVm/hg/YfoYd5vCFt+W+Lj47ll2mRuvWE02d0crV+EECAMbN4iYty7iKnfjcVfa1ZK01CVBBrs/Wi098dnTUUIA0lubeH7in0sXrmZNes2hi28LabKb0tCQgIzpk/h5glXkpko//OLaB0jbM1FxLh3E9uwF6t2LBLpA0JVEmiw9afB3heftUfrLjlZxprUm/wylSXvbOH9DzaiaZHZ+xMx+W1JSkpi5i1TuGn85fSMF2hNVSe6JntzEbGNe3E15WHRzFnqPRMtSiIN9n402Pris3RrHasAW3IfDlT4WfLOVlav3RAx4W3pEPltSUlJYdYtU5g8bjipMS2oDa2DclzDLlJrVkU0twCK4q7DY81AIGFP6UN+ucZfV33M6rUb8PuDW48Plw6X35aZU3J5YvowbGoVWRWvIYvItzZNclIQNxHVlsK4B/5OWXV9xHOejqjt1UzvkcRj04eD5iWtenmHiAewiGYyPZsQ3jqWzp2CIkdvu2pUMsuyxMo/3Y+keehRtxq7Vteh+V16Janql6RY3Lz48IQOzd2WqMh/dc7PSbJ6iG/cSUJzfjSqQKq2D5dWyuV9YPKYoVGpQ4fLv/Ga4YwY6MTmKyO1YUtHpz+BBORon2PVm3h0Yj/6ZAS+FGwWHSo/My2FObMuR9K8ZLjfRya6N2SsqPQWu8DvZsmjY3DYwrtnHCwdJl+WJd78w+2gNtKzaSN2I/Jz+kCI4xgZ4iBOo4ZFj4/v0NwdJn/h0zOJl9wkN+8lQT0cdjyBRAtOE2oGaVIRcUYF/WJr+NVkkx/0OAMdIn/KuEu4OBuc/nLSmneYErNc6k8hF2CY8KizJEFfy0FseJk2zMqFZ6WaUMP2ibj8XunJzJ42FFn3kOXdjBzG9rzjeIinzMjBI2IpNQLcdt4OVkljgO0QQvXw4syBxDkj3/9HVL4iyyx75iaMlnqyfNuwi/Z3/7aHISQO62cj2eOxxCRTbvTEbSSaUFuIUzxk24qR1RqWPnpFxF8XE1H5C2ZPxWXU0K1lP4lGqSkxS4zeeA0HGwqd/N8+G8IwKFD7429nH36gZDjqSJLrSDaKmDvjfFNino6IyZ86bhgXpHtxalVkGHmmxGw04inX0tESB/HEvGU8+6eluBOGoxoShS05puQAGOiqxI6XKzIqGHdRpmlxTyYi8nN6JvPrCX1QdC+99R2mvGxLFzIFLb2R7XHc+8cNJ54yvPnx17B0P4taNZZyNSXsPAAWWXBOQi3oXv7jaomMFLspcU/GdPkWRWbp7FyE6ibH2IMdczbTFqkZeDWZLSUp7N136MTx2to65r5xGMUWw2FPCh7dZkq+eJtG3zg3oqWEhXenYY3A+Gu6/AWPT8ShVtDDKCBRMue+bb3mosyXgEgewuPz3vjB/7+/8RN2egah6xr57m4YwpyBMivOoLsDnL6jvDjLnFlVW0yVf8v4CxmSXItL1JIlFZgSUxMyhzypWOxxPLjgs9PuEnjouSWQOQqPJlHYGGdKboCzUwQOBAMdBdx2lbn9v2ny+2amcN+YZBTdSz9ln2mv1zrs6YZPk9hWm80XeQdP+zld15n5n+uwJedQ0iRT4zOnn7DIMCQNEIKbB9UwqFesKXHBRPnzf/3vGC1udF3jmBZvSszaFgflXgei+1Aem9f+gwdHior5y45YJFmQX2vQYsK6nRBQ3dT6eJhQVZ672bzuxzT5D/75I6SMK9ENQaEvk2+8vdBE6OH9hsQ37ngUu5NH/+frgLdsLF6+nrKECbSosK86vDcUtmiwq0jiYIWEoUnY++by9HvhXygexzT5hwoOM+L2F9mnjES2u6huiWFPQ28aNEdI8Q7Wx9Ki+dnlGcyOPQeCKnvnb9/Gnn0NtR6ZohBv0dY0wceHJKobQFJiqO11K7lPbSRv/6H2CweIAvzWrGCGYfCPjds5ZhvAqGF98R4roaI5BgmDeKsa8DhQ6VU44jawpF3ILXM3BL0VT9M08sqtjD03luraerq7wB7gBbAQcLAS8opB80s4087hrbLBPPXKO6ZvJzFV/nHyDx7m3c+rmXDDJOT6/dSrFtwtFpLtLSjt/K6pOnxZJZBsCcxeBUUlVSHVoayyhoxBo+glDlHnEWQmgtzOl+9TYddhiZJaCYSM45wJ3Pu3YjZ8/EVIdWiPiC0vVFZWcvVd8/nUPxqrqzt1PoMdlQ5qfWdOub9Gwq9J7NNHsG1XcN3NyTy/8B18fabS2Az729k8XemGj/KhpkFCtrqoyJrO+CfX8E3Bt2HV4Ux0yL6dEZdewO9vTaO5cC1CCHrFCfom/bAlljbA/moJS/pljH1qhymbmJKTk1j50CCaDm/jwt6CtJMWQA0D8kuhsEJCGODKOpcl+d14/d0Pw87dHhHpdk6mqLicFZ9Ucc3E27A35lPv1anxQrKTE5ftPj98WS6BEscz66wcKTZne3lzs48q0rm0ZwsV1V4yUr7L6W2B7QehvFYCAc5zJ3H3kkNs3bHXlNzt0WG3Ed1uNzfcv4j368fh6H4Obi98+i2UN7QOcnnlEqoKB61XsvXz/abmXrt5B/tixtCiSuwuaM1XVgeb86DeI6M4Ezjaazrjn1zFkaKOe8omKtsFzx00kFd/eR6evOUgQYJdUN8sYc+8hHHPfnH69/CEgaIorHl+Es1fvU2yS3DMIyOEIC7nfBbujmXF2tDemRMOUdur6XA4WPDUbWRVr0D11CNbY5i3pzebPjNn7f9U9M7O5LWpyTSV5CFJYB08hbvmf0RpWUXEcp6JDunzT4Wmabz74ReIjBEMH9iNQwzhleWRbX317gbktCGcn6ZzICGX2597i4aGxojmbA8R7T/ZvTKF3W7rsHwDB/SL+jkDIqpbxH/qdMrX+f5U6JIfRbrkR5Eu+VGkS34U6ZIfRbrkR5Eu+VGkS34U6ZIfRbrkR5H/Bx8z6HmTXnicAAAAAElFTkSuQmCCCg==');
//	this.addLinkTag('apple-touch-startup-image', 'default.png');

//	if (!window.navigator.standalone)	// not running as an installed app

	MochiKit.Signal.connect(Clipperz.Signal.NotificationCenter, 'selectedDirectLogin', this, 'selectedDirectLoginHandler');

	MochiKit.DOM.addElementClass(document.body, 'iPhone');
	return this;
}

MochiKit.Base.update(Clipperz.PM.UI.iPhone.Controllers.MainController.prototype, {

	'toString': function () {
		return "Clipperz.PM.UI.iPhone.Controllers.MainController";
	},

	//=========================================================================

	'user': function () {
		return this._user;
	},
	
	'setUser': function (aValue) {
		this._user = aValue;
	},

	//=========================================================================

	'loginForm': function() {
		if (this._loginForm == null) {
			MochiKit.DOM.removeElement('mainDiv');
			this._loginForm = new Clipperz.PM.UI.iPhone.Components.LoginForm({element:MochiKit.DOM.currentDocument().body});
			MochiKit.Signal.connect(this._loginForm, 'doLogin', this, 'doLoginHandler')
		}
		
		return this._loginForm;
	},

	'removeLoginForm': function () {
		if (this._loginForm != null) {
			this._loginForm.remove();
			this._loginForm = null;
		}
	},

	//-----------------------------------------------------------------------------

	'cardList': function () {
		if (this._cardList == null) {
			this._cardList = new Clipperz.PM.UI.iPhone.Components.CardList({element:MochiKit.DOM.currentDocument().body});
			MochiKit.Signal.connect(this._cardList, 'searchEvent',	this, 'searchEventHandler')
			MochiKit.Signal.connect(this._cardList, 'selectedCard',	this, 'selecetedCardHandler')
		}
		
		return this._cardList;
	},

	//=========================================================================

	'currentWidth': function () {
		return this._currentWidth;
	},
	
	'setCurrentWidth': function (aValue) {
		this._currentWidth = aValue;
	},

	//=========================================================================

	'orientationChangeHandler': function () {
		switch(window.orientation) {
			case 0:
				this.setOrientation('portrait');
				break;  
			case 90:
			case -90: 
				this.setOrientation('landscape');
				break;
		}
	},

	//-------------------------------------------------------------------------

	'setOrientation': function (anOrientation) {
		document.body.setAttribute('orientation', anOrientation);
		setTimeout(scrollTo, 100, 0, 1);
	},

	//-------------------------------------------------------------------------

	'slidePages': function (fromPage, toPage, backwards) {        
		var axis;
		var slideDone;
		
		slideDone = function () {
	//		console.log("slideDone");
			if (!hasClass(toPage, "dialog")) {
				fromPage.removeAttribute("selected");
			}
			checkTimer = setInterval(checkOrientAndLocation, 300);
			setTimeout(updatePage, 0, toPage, fromPage);
			fromPage.removeEventListener('webkitTransitionEnd', slideDone, false);
		}

		axis = (backwards ? fromPage : toPage).getAttribute("axis");

		clearInterval(checkTimer);

		if (canDoSlideAnim() && axis != 'y') {
			slide2(fromPage, toPage, backwards, slideDone);
		} else {
			slide1(fromPage, toPage, backwards, axis, slideDone);
		}
	},

	//-------------------------------------------------------------------------

	'getCachedValues': function () {
		var deferredResult;

		if (this._cachedObjects != null) {
			deferredResult = MochiKit.Async.succeed(this._cachedObjects);
		} else {
			deferredResult = new Clipperz.Async.Deferred("MainController.getCachedValues", {trace:false});
			deferredResult.addMethod(this.user(), 'getRecords');
			deferredResult.addCallback(MochiKit.Base.map, Clipperz.Async.collectResults("MainController.getCachedValues - collectResults", {
				'_rowObject':			MochiKit.Async.succeed,
				'_reference':			MochiKit.Base.methodcaller('reference'),
				'label':				MochiKit.Base.methodcaller('label'),
				'favicon':				MochiKit.Base.methodcaller('favicon'),
				'_searchableContent':	MochiKit.Base.methodcaller('searchableContent')
			}, {trace:false}));
			deferredResult.addCallback(Clipperz.Async.collectAll);
			deferredResult.addCallback(MochiKit.Base.bind(function (someRows) {
				this._cachedObjects = someRows;
				return this._cachedObjects;
			}, this));
			deferredResult.callback();
		}
		
		return deferredResult;
	},
	//=========================================================================

	'run': function(shouldShowRegistrationForm) {
		this.loginForm().render();
		MochiKit.Async.callLater(1, MochiKit.Base.method(this.loginForm(), 'focusOnUsername'));
	},
	
	//=========================================================================

	'doLoginHandler': function (someArgs) {
		var deferredResult;
		var	parameters;
		var	shouldUseOTP;
//		var loginProgress;
		var	user;
		var getPassphraseDelegate;

		parameters = someArgs;
		shouldUseOTP = (typeof(parameters.passphrase) == 'undefined');

		getPassphraseDelegate = MochiKit.Base.partial(MochiKit.Async.succeed, parameters.passphrase);
		user = new Clipperz.PM.DataModel.User({'username':parameters.username, 'getPassphraseFunction':getPassphraseDelegate});

		deferredResult = new Clipperz.Async.Deferred("MainController.doLogin", {trace:false});
		deferredResult.addCallbackPass(MochiKit.Signal.signal, Clipperz.Signal.NotificationCenter, 'initProgress', {'steps':4});
		deferredResult.addMethod(this.loginForm(), 'showLoginProgress');
		deferredResult.addCallback(MochiKit.Async.wait, 0.1);
		deferredResult.addMethod(Clipperz.Crypto.PRNG.defaultRandomGenerator(), 'deferredEntropyCollection');
		deferredResult.addMethod(user, 'login');
		deferredResult.addMethod(this, 'setUser', user);
		deferredResult.addMethod(user, 'getRecords');
		deferredResult.addMethod(this, 'removeLoginForm');
		deferredResult.addMethod(this.cardList(), 'render');
		deferredResult.addMethod(this, 'displaySelectedRecords', '');
		deferredResult.addErrback(MochiKit.Base.method(this.loginForm(), 'showLoginError'));
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'searchEventHandler': function (aValue) {
//console.log("searching for ... " + aValue);
		return this.displaySelectedRecords(aValue);
	},

	//=========================================================================

	'_displaySelectedRows': function (aFilter, someRows) {
		var result;
		
		result = someRows;

		if (aFilter != null) {
			var filter;
			var	filterRegExp;
			
			filter = aFilter.replace(/[^A-Za-z0-9]/g, "\\$&");
			filterRegExp = new RegExp(filter, "i");
			result = MochiKit.Base.filter(function (aCachedResult) { return filterRegExp.test(aCachedResult['_searchableContent'])}, result);
		}


		result.sort(MochiKit.Base.partial(function (aKey, aComparator, aObject, bObject) {
			return aComparator(aObject[aKey], bObject[aKey]);
		}, 'label',  Clipperz.Base.caseInsensitiveCompare));

		this.cardList().update(result);
	},

	//-------------------------------------------------------------------------
	
	'displaySelectedRecords': function (aFilter) {
		return Clipperz.Async.callbacks("MainController.displaySelectedrows", [
			MochiKit.Base.method(this, 'getCachedValues'),
			MochiKit.Base.method(this, '_displaySelectedRows', aFilter)
		], {trace:false});
	},

	//=========================================================================

	'selecetedCardHandler': function (aRecordReference) {
		var	deferredResult;
		var	recordData;

		recordData = {};
//console.log("Showing detail for card with reference", aRecordReference);
		deferredResult = new Clipperz.Async.Deferred("MainController.selectedCardHandler", {trace:false});
		deferredResult.addMethod(this.user(), 'getRecord', aRecordReference);
		deferredResult.collectResults({
			'_reference':	MochiKit.Base.methodcaller('reference'),
			'title':		MochiKit.Base.methodcaller('label'),
			'favicon':		MochiKit.Base.methodcaller('favicon')
		});
		deferredResult.addCallback(function (someData) {
			MochiKit.Base.update(recordData, someData);
		})
		deferredResult.addMethod(this.cardList(), 'showCard', recordData);

		deferredResult.addMethod(this.user(), 'getRecord', aRecordReference);
		deferredResult.addMethodcaller('notes');
		deferredResult.addCallback(function (someNotes) {
			recordData['notes'] = someNotes;
		})

		deferredResult.addMethod(this.user(), 'getRecord', aRecordReference);
		deferredResult.addMethodcaller('getCurrentRecordVersion');
		deferredResult.addMethodcaller('fields');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Async.collectResults("MainController.selectedCardHandler - fields", {
			'label':	MochiKit.Base.methodcaller('label'),
			'value':	MochiKit.Base.methodcaller('value'),
			'isHidden':	MochiKit.Base.methodcaller('isHidden')
		}, {trace:false}));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(function (someData) {
			recordData['fields'] = someData;
		});

		deferredResult.addMethod(this.user(), 'getRecord', aRecordReference);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.values);
		deferredResult.addCallback(MochiKit.Base.map, Clipperz.Async.collectResults("MainController.selectedCardHandler - directLogins", {
			'label':		MochiKit.Base.methodcaller('label'),
			'favicon':		MochiKit.Base.methodcaller('favicon'),
			'_reference':	MochiKit.Base.methodcaller('reference')
		}, {trace:false}));
		deferredResult.addCallback(Clipperz.Async.collectAll);
		deferredResult.addCallback(function (someData) {
			recordData['directLogins'] = someData;
		});

		deferredResult.addMethod(this.cardList(), 'showCardDetails', recordData);
		deferredResult.callback();
		
		return deferredResult;
	},

	//=========================================================================

	'selectedDirectLoginHandler': function (someData) {
		var	deferredResult;

//console.log("<<< signal - directLogin");
		deferredResult = new Clipperz.Async.Deferred("MainController.selectedDirectLoginHandler", {trace:false});
		deferredResult.addMethod(this.user(), 'getRecord', someData['cardReference']);
		deferredResult.addMethodcaller('directLogins');
		deferredResult.addCallback(MochiKit.Base.itemgetter(someData['directLoginReference']));
//		deferredResult.addMethodcaller('runDirectLogin');
		deferredResult.addCallback(Clipperz.PM.UI.Common.Controllers.DirectLoginRunner.openDirectLogin);
		deferredResult.callback();

		return deferredResult;
	},

	//=========================================================================

	'addMetaTag': function (aName, aContent) {
		var	metaTag;

		metaTag = document.createElement('meta');
		metaTag.name = aName;
		metaTag.content = aContent;
		document.getElementsByTagName('head')[0].appendChild(metaTag);
	},

	'addLinkTag': function (aRel, anHref) {
		var linkTag;

		linkTag = document.createElement('link');
		linkTag.rel = aRel;
		linkTag.href = anHref;
		document.getElementsByTagName('head')[0].appendChild(linkTag);
	},

	//=========================================================================
	__syntaxFix__: "syntax fix"
});