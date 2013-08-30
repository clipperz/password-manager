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

/***

MochiKit.Test 1.5

See <http://mochikit.com/> for documentation, downloads, license, etc.

(c) 2005 Bob Ippolito.  All rights Reserved.

***/

MochiKit.Base.module(MochiKit, 'Test', '1.5', ['Base']);

MochiKit.Test.runTests = function (obj) {
    if (typeof(obj) == "string") {
        // TODO: Remove this temporary API change advertisement
        throw new TypeError("Automatic module import not supported, call runTests() with proper object: " + obj);
    }
    var suite = new MochiKit.Test.Suite();
    suite.run(obj);
};

MochiKit.Test.Suite = function () {
    this.testIndex = 0;
    MochiKit.Base.bindMethods(this);
};

MochiKit.Test.Suite.prototype = {
    run: function (obj) {
        try {
            obj(this);
        } catch (e) {
            this.traceback(e);
        }
    },
    traceback: function (e) {
        var items = MochiKit.Iter.sorted(MochiKit.Base.items(e));
        print("not ok " + this.testIndex + " - Error thrown");
        for (var i = 0; i < items.length; i++) {
            var kv = items[i];
            if (kv[0] == "stack") {
                kv[1] = kv[1].split(/\n/)[0];
            }
            this.print("# " + kv.join(": "));
        }
    },
    print: function (s) {
        print(s);
    },
    is: function (got, expected, /* optional */message) {
        var res = 1;
        var msg = null;
        try {
            res = MochiKit.Base.compare(got, expected);
        } catch (e) {
            msg = "Can not compare " + typeof(got) + ":" + typeof(expected);
        }
        if (res) {
            msg = "Expected value did not compare equal";
        }
        if (!res) {
            return this.testResult(true, message);
        }
        return this.testResult(false, message,
            [[msg], ["got:", got], ["expected:", expected]]);
    },

    testResult: function (pass, msg, failures) {
        this.testIndex += 1;
        if (pass) {
            this.print("ok " + this.testIndex + " - " + msg);
            return;
        }
        this.print("not ok " + this.testIndex + " - " + msg);
        if (failures) {
            for (var i = 0; i < failures.length; i++) {
                this.print("# " + failures[i].join(" "));
            }
        }
    },

    isDeeply: function (got, expected, /* optional */message) {
        var m = MochiKit.Base;
        var res = 1;
        try {
            res = m.compare(got, expected);
        } catch (e) {
            // pass
        }
        if (res === 0) {
            return this.ok(true, message);
        }
        var gk = m.keys(got);
        var ek = m.keys(expected);
        gk.sort();
        ek.sort();
        if (m.compare(gk, ek)) {
            // differing keys
            var cmp = {};
            var i;
            for (i = 0; i < gk.length; i++) {
                cmp[gk[i]] = "got";
            }
            for (i = 0; i < ek.length; i++) {
                if (ek[i] in cmp) {
                    delete cmp[ek[i]];
                } else {
                    cmp[ek[i]] = "expected";
                }
            }
            var diffkeys = m.keys(cmp);
            diffkeys.sort();
            var gotkeys = [];
            var expkeys = [];
            while (diffkeys.length) {
                var k = diffkeys.shift();
                if (k in Object.prototype) {
                    continue;
                }
                (cmp[k] == "got" ? gotkeys : expkeys).push(k);
            }


        }

        return this.testResult((!res), msg,
            (msg ? [["got:", got], ["expected:", expected]] : undefined)
        );
    },

    ok: function (res, message) {
        return this.testResult(res, message);
    }
};

MochiKit.Test.__new__ = function () {
    var m = MochiKit.Base;
    this.Suite.__export__ = false;
    m.nameFunctions(this);

};

MochiKit.Test.__new__();

MochiKit.Base._exportSymbols(this, MochiKit.Test);
