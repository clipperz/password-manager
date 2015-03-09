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

Clipperz.YUI.MessageBox = function(){
    var dlg, opt, mask;
    var bodyEl, msgEl, textboxEl, textareaEl, progressEl, pp;
    var buttons, activeTextEl, bwidth;
    
    var handleButton = function(button){
        if(typeof opt.fn == 'function'){
            if(opt.fn.call(opt.scope||window, button, activeTextEl.dom.value) !== false){
                dlg.hide();
            }
        }else{
            dlg.hide();
        }
    };
    
    return {
	    updateButtons: function(b){
	        var width = 0;
	        if(!b){
	            buttons['ok'].hide();
	            buttons['cancel'].hide();
	            buttons['yes'].hide();
	            buttons['no'].hide();
	            return width;
	        }
	        for(var k in buttons){
	            if(typeof buttons[k] != 'function'){
	                if(b[k]){
	                    buttons[k].show();
	                    buttons[k].setText(typeof b[k] == 'string' ? b[k] : YAHOO.ext.MessageBox.buttonText[k]);
	                    width += buttons[k].el.getWidth()+15;
	                }else{
	                    buttons[k].hide();
	                }
	            }
	        }
	        return width;
	    },

        getDialog : function(){
           if(!dlg){
                dlg = new YAHOO.ext.BasicDialog('mb-dlg', {
                    autoCreate:true,
                    shadow:true,
                    draggable:true,
                    resizable:false,
                    constraintoviewport:true,
                    fixedcenter:true,
                    shim:true,
                    modal:true,
                    width:400, height:100,
                    buttonAlign:'center',
                    closeClick : function(){
                        if(opt && opt.buttons && opt.buttons.no && !opt.buttons.cancel){
                            handleButton('no');
                        }else{
                            handleButton('cancel');
                        }
                    }
                });
                dlg.closeClick = function(){
                    alert('wtf');
                };
                mask = dlg.mask;
                dlg.addKeyListener(27, dlg.hide, dlg);
                buttons = {};
                buttons['ok'] = dlg.addButton(this.buttonText['ok'], handleButton.createCallback('ok'));
                buttons['yes'] = dlg.addButton(this.buttonText['yes'], handleButton.createCallback('yes'));
                buttons['no'] = dlg.addButton(this.buttonText['no'], handleButton.createCallback('no'));
                buttons['cancel'] = dlg.addButton(this.buttonText['cancel'], handleButton.createCallback('cancel'));
                bodyEl = dlg.body.createChild({
                    tag:'div', 
                    html:'<span class="ext-mb-text"></span><br /><input type="text" class="ext-mb-input"><textarea class="ext-mb-textarea"></textarea><div class="ext-mb-progress-wrap"><div class="ext-mb-progress"><div class="ext-mb-progress-bar">&#160;</div></div></div>'
                });
                msgEl = bodyEl.dom.firstChild;
                textboxEl = getEl(bodyEl.dom.childNodes[2]);
                textboxEl.enableDisplayMode();
                textboxEl.addKeyListener([10,13], function(){
                    if(dlg.isVisible() && opt && opt.buttons){
                        if(opt.buttons.ok){
                            handleButton('ok');
                        }else if(opt.buttons.yes){
                            handleButton('yes');
                        }
                    }
                });
                textareaEl = getEl(bodyEl.dom.childNodes[3]);
                textareaEl.enableDisplayMode();
                progressEl = getEl(bodyEl.dom.childNodes[4]);
                progressEl.enableDisplayMode();
                pp = getEl(progressEl.dom.firstChild.firstChild);
            }
            return dlg;
        },
        
        updateText : function(text){
            if(!dlg.isVisible() && !opt.width){
                dlg.resizeTo(this.maxWidth, 100); // resize first so content is never clipped from previous shows
            }
            msgEl.innerHTML = text;
            var w = Math.max(Math.min(opt.width || msgEl.offsetWidth, this.maxWidth), 
                        Math.max(opt.minWidth || this.minWidth, bwidth));
            if(opt.prompt){
                activeTextEl.setWidth(w);
            }
            dlg.setContentSize(w, bodyEl.getHeight());
        },        
        
        updateProgress : function(value, text){
            if(text){
                this.updateText(text);
            }
            pp.setWidth(value*progressEl.dom.firstChild.offsetWidth);
        },        
        
        isVisible : function(){
            return dlg && dlg.isVisible();  
        },
        
        hide : function(){
            if(this.isVisible()){
                dlg.hide();
            }  
        },
        
        show : function(options){
            var d = this.getDialog();
            opt = options;
            d.setTitle(opt.title || '&#160;');
            d.close.setDisplayed(opt.closable !== false);
            activeTextEl = textboxEl;
            opt.prompt = opt.prompt || (opt.multiline ? true : false)
            if(opt.prompt){
                if(opt.multiline){
                    textboxEl.hide();
                    textareaEl.show();
                    textareaEl.setHeight(typeof opt.multiline == 'number' ? 
                        opt.multiline : this.defaultTextHeight);
                    activeTextEl = textareaEl;
                }else{
                    textboxEl.show();
                    textareaEl.hide();
                }
            }else{
                textboxEl.hide();
                textareaEl.hide();
            }
            progressEl.setDisplayed(opt.progress === true);
            this.updateProgress(0);
            activeTextEl.dom.value = opt.value || '';
            if(opt.prompt){
                dlg.setDefaultButton(activeTextEl);
            }else{
                var bs = opt.buttons;
                var db = null;
                if(bs && bs.ok){
                    db = buttons['ok'];
                }else if(bs && bs.yes){
                    db = buttons['yes'];
                }
                dlg.setDefaultButton(db);
            }
            bwidth = this.updateButtons(opt.buttons);
            this.updateText(opt.msg);
            d.modal = opt.modal !== false;
            d.mask = opt.modal !== false ? mask : false;
            d.animateTarget = null;
            d.show(options.animEl);
        },
        
        progress : function(title, msg){
            this.show({
                title : title,
                msg : msg,
                buttons: false,
                progress:true,
                closable:false
            });
        },

		progressElement : function() {
			return progressEl;
		},
        
		opt: function() {
			return opt;
		},
		
        alert : function(title, msg, fn, scope){
            this.show({
                title : title,
                msg : msg,
                buttons: this.OK,
                fn: fn,
                scope : scope
            });
        },
        
        confirm : function(title, msg, fn, scope){
            this.show({
                title : title,
                msg : msg,
                buttons: this.YESNO,
                fn: fn,
                scope : scope
            });
        },
        
        prompt : function(title, msg, fn, scope, multiline){
            this.show({
                title : title,
                msg : msg,
                buttons: this.OKCANCEL,
                fn: fn,
                minWidth:250,
                scope : scope,
                prompt:true,
                multiline: multiline
            });
        },
        
        OK : {ok:true},
        YESNO : {yes:true, no:true},
        OKCANCEL : {ok:true, cancel:true},
        YESNOCANCEL : {yes:true, no:true, cancel:true},
        
        defaultTextHeight:75,
        maxWidth : 500,
        minWidth : 100,
        buttonText : {
            ok : 'OK',
            cancel : 'Cancel',
            yes : 'Yes',
            no : 'No'
        }
    };
}();
