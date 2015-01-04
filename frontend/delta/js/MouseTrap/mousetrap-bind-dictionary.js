/*

Copyright 2008-2013 Clipperz Srl

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

/**
 * Overwrites default Mousetrap.bind method to optionally accept
 * an object to bind multiple key events in a single call
 *
 * You can pass it in like:
 *
 * Mousetrap.bind({
 *     'a': function() { console.log('a'); },
 *     'b': function() { console.log('b'); }
 * });
 *
 * And can optionally pass in 'keypress', 'keydown', or 'keyup'
 * as a second argument
 *
 */
/* global Mousetrap:true */
Mousetrap = (function(Mousetrap) {
    var self = Mousetrap,
        _oldBind = self.bind,
        args;

    self.bind = function() {
        args = arguments;

        // normal call
        if (typeof args[0] == 'string' || args[0] instanceof Array) {
            return _oldBind(args[0], args[1], args[2]);
        }

        // object passed in
        for (var key in args[0]) {
            if (args[0].hasOwnProperty(key)) {
                _oldBind(key, args[0][key], args[1]);
            }
        }
    };

    return self;
}) (Mousetrap);
