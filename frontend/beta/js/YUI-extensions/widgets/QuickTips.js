/**
 * @class YAHOO.ext.QuickTips
 * @singleton
 */
YAHOO.ext.QuickTips = function(){
    var el, tipBody, tipTitle, tm, cfg, close, tagEls = {}, reader, esc, anim, removeCls = null;
    var ce, bd, xy;
    var visible = false, disabled = true, inited = false;
    var showProc = hideProc = dismissProc = 1, locks = [];
    var E = YAHOO.util.Event, dd;
    
    var onOver = function(e){
        if(disabled){
            return;
        }
        var t = E.getTarget(e);
        if(!t){
            return;
        }
        if(ce && t == ce.el){
            clearTimeout(hideProc);
            return;
        }
        if(t && tagEls[t.id]){
            tagEls[t.id].el = t;
            showProc = show.defer(tm.showDelay, tm, [tagEls[t.id]]);
            return;
        }
        var ttp = reader.getAttribute(t, cfg.attribute);
        if(!ttp && tm.interceptTitles && t.title){
            ttp = t.title;
            t.title = '';
            if(reader.useNS){
                t.setAttributeNS('y', 'qtip', ttp);
            }else{
                t.setAttribute('qtip', ttp);
            }
        }
        if(ttp){
            xy = E.getXY(e);
            xy[0] += 12; xy[1] += 20;
            showProc = show.defer(tm.showDelay, tm, [{
                el: t, 
                text: ttp, 
                width: reader.getAttribute(t, cfg.width),
                autoHide: reader.getAttribute(t, cfg.hide) != 'user',
                title: reader.getAttribute(t, cfg.title),
           	    cls: reader.getAttribute(t, cfg.cls)
            }]);
        }
    };
    
    var onOut = function(e){
        clearTimeout(showProc);
        var t = E.getTarget(e);
        if(t && ce && ce.el == t && (tm.autoHide && ce.autoHide !== false)){
            hideProc = setTimeout(hide, tm.hideDelay);
        }
    };
    
    var onMove = function(e){
        if(disabled){
            return;
        }
        xy = E.getXY(e);
        xy[0] += 12; xy[1] += 20;
        if(tm.trackMouse && ce){
            el.setXY(xy);
        }
    };
    
    var onDown = function(e){
        clearTimeout(showProc);
        clearTimeout(hideProc);
        if(!e.within(el)){
            if(tm.hideOnClick && ce && ce.autoHide !== false){
                hide();
                tm.disable();
            }
        }
    };
    
    var onUp = function(e){
        tm.enable();
    }
    
    var show = function(o){
        if(disabled){
            return;
        }
        clearTimeout(dismissProc);
        stopAnim();
        ce = o;
        if(removeCls){ // in case manually hidden
            el.removeClass(removeCls);
            removeCls = null;
        }
        if(ce.cls){
            el.addClass(ce.cls);
            removeCls = ce.cls;
        }
        if(ce.title){
            tipTitleText.update(ce.title);
            tipTitle.show();
        }else{
            tipTitle.hide();
        }
        tipBody.update(o.text);
        if(!ce.width){
            if(tipBody.dom.style.width){
               tipBody.dom.style.width  = '';
            }
            if(tipBody.dom.offsetWidth > tm.maxWidth){
                tipBody.setWidth(tm.maxWidth);
            }
        }else{
            tipBody.setWidth(ce.width);
        }
        if(!ce.autoHide){
            close.setDisplayed(true);
            if(dd){
                dd.unlock();
            }
        }else{
            close.setDisplayed(false);
            if(dd){
                dd.lock();
            }
        }
        if(xy){
            el.setXY(xy);
        }
        if(tm.animate){
            anim.attributes = {opacity:{to:1}};
            el.setOpacity(.1);
            el.setStyle('visibility', 'visible');
            anim.animateX(afterShow);
        }else{
            afterShow();
        }
    };
    
    var afterShow = function(){
        if(ce){
            el.show();
            esc.enable();
            if(tm.autoDismiss && ce.autoHide !== false){
                dismissProc = setTimeout(hide, tm.autoDismissDelay);
            }
        }
    }
    
    var hide = function(noanim){
        clearTimeout(dismissProc);
        clearTimeout(hideProc);
        ce = null;
        if(el.isVisible()){
            esc.disable();
            stopAnim();
            if(noanim !== true && tm.animate){
                anim.attributes = {opacity:{to:.1}};
                el.beforeAction();
                anim.animateX(afterHide);
            }else{
                afterHide();
            } 
        }
    };
    
    var afterHide = function(){
        el.hide();
        if(removeCls){
            el.removeClass(removeCls);
            removeCls = null;
        }
    }
    
    var stopAnim = function(){
        if(anim && anim.isAnimated()){
            anim.stop();
        }
    }
    
    return {
       init : function(){
          if(YAHOO.ext.util.Browser.isIE && !YAHOO.ext.util.Browser.isIE7){
              return;
          }
          tm = YAHOO.ext.QuickTips;
          cfg = tm.tagConfig;
          reader = new YAHOO.ext.CustomTagReader(cfg.namespace);
          if(!inited){
              el = new YAHOO.ext.Layer({cls:'ytip', shadow:true, useDisplay: false});
              el.update('<div class="ytip-hd-left"><div class="ytip-hd-right"><div class="ytip-hd"></div></div></div>');
              tipTitle = getEl(el.dom.firstChild);
              tipTitleText = getEl(el.dom.firstChild.firstChild.firstChild);
              tipTitle.enableDisplayMode('block');
              tipBody = el.createChild({tag:'div', cls:'ytip-bd'});
              close = el.createChild({tag:'div', cls:'ytip-close'});
              close.on('click', hide);
              d = getEl(document);
              d.mon('mousedown', onDown);
              d.on('mouseup', onUp);
              d.on('mouseover', onOver);
              d.on('mouseout', onOut);
              d.on('mousemove', onMove);
              esc = d.addKeyListener(27, hide);
              esc.disable();
              if(tm.animate){
                  anim = new YAHOO.util.Anim(el.dom, {}, .1);
              }
              if(YAHOO.util.DD){
                  dd = el.initDD('default', null, {
                      onDrag : function(){
                          el.sync();  
                      }
                  });
                  dd.setHandleElId(tipTitleText.id);
                  dd.lock();
              }
              inited = true;
          }
          this.scan(document.body);
          this.enable(); 
       },
       
       tips : function(config){
           var cs = config instanceof Array ? config : arguments;
           for(var i = 0, len = cs.length; i < len; i++) {
               var c = cs[i];
               var target = c.target;
               if(target){
                   if(target instanceof Array){
                       for(var j = 0, jlen = target.length; j < jlen; j++){
                           tagEls[target[j]] = c;
                       }
                   }else{
                       tagEls[target] = c;
                   }               	   
               }
           }
       },
       
       enable : function(){
           if(inited){
               locks.pop();
               if(locks.length < 1){
                   disabled = false;
               }
           }
       },
       
       disable : function(){
          disabled = true;
          clearTimeout(showProc);
          clearTimeout(hideProc);
          clearTimeout(dismissProc);
          if(ce){
              hide(true);
          }
          locks.push(1);
       },
       
       scan : function(toScan){
           toScan = toScan.dom ? toScan.dom : YAHOO.util.Dom.get(toScan);
           var found = [];
           reader.eachElement(cfg.tag, toScan, function(el){
               var t = reader.getAttribute(el, cfg.target);
           	   if(t){
           	       found.push({
           	          target: t.indexOf(',') != -1 ? t.split(',') : t, 
                      text: el.innerHTML,
                      autoHide: reader.getAttribute(el, cfg.hide) != 'user',
                      width: reader.getAttribute(el, cfg.width),
           	          title: reader.getAttribute(el, cfg.title),
           	          cls: reader.getAttribute(el, cfg.cls)
           	       });
           	   }
           	   el.parentNode.removeChild(el);
           });
           this.tips(found);
       },
       
       tagConfig : { 
           namespace : 'y',
           tag : 'qtip',
           attribute : 'qtip',
           width : 'width',
           target : 'target',
           title : 'qtitle',
           hide : 'hide',
           cls : 'qclass'
       },
       
       maxWidth : 300,
       interceptTitles : true,
       trackMouse : false,
       hideOnClick : true,
       showDelay : 500,
       hideDelay : 200,
       autoHide : true,
       autoDismiss : true,
       autoDismissDelay : 5000,
       /**
        * True to turn on fade animation. Defaults to true 
        * except in IE7 (ClearType/scrollbar flicker issues in IE7 with filters).
        * @type Boolean
        */
       animate : YAHOO.util.Anim && !YAHOO.ext.util.Browser.isIE7 
   } 
}();
