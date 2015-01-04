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
 * adds a pause and unpause method to Mousetrap
 * this allows you to enable or disable keyboard shortcuts
 * without having to reset Mousetrap and rebind everything
 */
/* global Mousetrap:true */
Mousetrap = (function(Mousetrap) {
    var self = Mousetrap,
        _originalStopCallback = self.stopCallback,
        enabled = true;

    self.stopCallback = function(e, element, combo) {
        if (!enabled) {
            return true;
        }

        return _originalStopCallback(e, element, combo);
    };

    self.pause = function() {
        enabled = false;
    };

    self.unpause = function() {
        enabled = true;
    };

    return self;
}) (Mousetrap);
