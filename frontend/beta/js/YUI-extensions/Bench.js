// @deprecated
// Use YAHOO.timer() instead
YAHOO.ext.util.Bench = function(){
   this.timers = {};
   this.lastKey = null;
};
YAHOO.ext.util.Bench.prototype = {
   start : function(key){
       this.lastKey = key;
       this.timers[key] = {};
       this.timers[key].startTime = new Date().getTime(); 
   },
   
   stop : function(key){
       key = key || this.lastKey;
       this.timers[key].endTime = new Date().getTime(); 
   },
   
   getElapsed : function(key){
       key = key || this.lastKey;
       return this.timers[key].endTime - this.timers[key].startTime;
   },
   
   toString : function(html){
       var results = "";
       for(var key in this.timers){
           if(typeof this.timers[key] != 'function'){
               results += key + ":\t" + (this.getElapsed(key) / 1000) + " seconds\n";
           }
       }
       if(html){
           results = results.replace("\n", '<br>');
       }
       return results;
   },
   
   show : function(){
       alert(this.toString());
   }
};
