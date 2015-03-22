/***

MochiKit.Base 1.5

See <http://mochikit.com/> for documentation, downloads, license, etc.

(c) 2005 Bob Ippolito.  All rights Reserved.

***/


// MochiKit module (namespace)
var MochiKit = MochiKit || {};
if (typeof(MochiKit.__export__) == "undefined") {
    MochiKit.__export__ = true;
}
MochiKit.NAME = "MochiKit";
MochiKit.VERSION = "1.5";
MochiKit.__repr__ = function () {
    return "[" + this.NAME + " " + this.VERSION + "]";
};
MochiKit.toString = function () {
    return this.__repr__();
};


// MochiKit.Base module
MochiKit.Base = MochiKit.Base || {};

/**
 * Creates a new module in a parent namespace. This function will
 * create a new empty module object with "NAME", "VERSION",
 * "toString" and "__repr__" properties. This object will be inserted into the parent object
 * using the specified name (i.e. parent[name] = module). It will
 * also verify that all the dependency modules are defined in the
 * parent, or an error will be thrown.
 *
 * @param {Object} parent the parent module (use "this" or "window" for
 *            a global module)
 * @param {String} name the module name, e.g. "Base"
 * @param {String} version the module version, e.g. "1.5"
 * @param {Array} [deps] the array of module dependencies (as strings)
 */
MochiKit.Base.module = function (parent, name, version, deps) {
    var module = parent[name] = parent[name] || {};
    var prefix = (parent.NAME ? parent.NAME + "." : "");
    module.NAME = prefix + name;
    module.VERSION = version;
    module.__repr__ = function () {
        return "[" + this.NAME + " " + this.VERSION + "]";
    };
    module.toString = function () {
        return this.__repr__();
    };
    for (var i = 0; deps != null && i < deps.length; i++) {
        if (!(deps[i] in parent)) {
            throw module.NAME + ' depends on ' + prefix + deps[i] + '!';
        }
    }
    return module;
};

MochiKit.Base.module(MochiKit, "Base", "1.5", []);

/** @id MochiKit.Base.update */
MochiKit.Base.update = function (self, obj/*, ... */) {
    if (self === null || self === undefined) {
        self = {};
    }
    for (var i = 1; i < arguments.length; i++) {
        var o = arguments[i];
        if (typeof(o) != 'undefined' && o !== null) {
            for (var k in o) {
                self[k] = o[k];
            }
        }
    }
    return self;
};

MochiKit.Base.update(MochiKit.Base, {
    /** @id MochiKit.Base.camelize */
    camelize: function (selector) {
        /* from dojo.style.toCamelCase */
        var arr = selector.split('-');
        var cc = arr[0];
        for (var i = 1; i < arr.length; i++) {
            cc += arr[i].charAt(0).toUpperCase() + arr[i].substring(1);
        }
        return cc;
    },

    /** @id MochiKit.Base.counter */
    counter: function (n/* = 1 */) {
        if (arguments.length === 0) {
            n = 1;
        }
        return function () {
            return n++;
        };
    },

    /** @id MochiKit.Base.clone */
    clone: function (obj) {
        var me = arguments.callee;
        if (arguments.length == 1) {
            me.prototype = obj;
            return new me();
        }
    },
    
    _flattenArray: function (res, lst) {
        for (var i = 0; i < lst.length; i++) {
            var o = lst[i];
            if (o instanceof Array) {
                arguments.callee(res, o);
            } else {
                res.push(o);
            }
        }
        return res;
    },

    /** @id MochiKit.Base.flattenArray */
    flattenArray: function (lst) {
        return MochiKit.Base._flattenArray([], lst);
    },

    /** @id MochiKit.Base.flattenArguments */
    flattenArguments: function (lst/* ...*/) {
        var res = [];
        var m = MochiKit.Base;
        var args = m.extend(null, arguments);
        while (args.length) {
            var o = args.shift();
            if (o && typeof(o) == "object" && typeof(o.length) == "number") {
                for (var i = o.length - 1; i >= 0; i--) {
                    args.unshift(o[i]);
                }
            } else {
                res.push(o);
            }
        }
        return res;
    },

    /** @id MochiKit.Base.extend */
    extend: function (self, obj, /* optional */skip) {
        // Extend an array with an array-like object starting
        // from the skip index
        if (!skip) {
            skip = 0;
        }
        if (obj) {
            // allow iterable fall-through, but skip the full isArrayLike
            // check for speed, this is called often.
            var l = obj.length;
            if (typeof(l) != 'number' /* !isArrayLike(obj) */) {
                if (typeof(MochiKit.Iter) != "undefined") {
                    obj = MochiKit.Iter.list(obj);
                    l = obj.length;
                } else {
                    throw new TypeError("Argument not an array-like and MochiKit.Iter not present");
                }
            }
            if (!self) {
                self = [];
            }
            for (var i = skip; i < l; i++) {
                self.push(obj[i]);
            }
        }
        // This mutates, but it's convenient to return because
        // it's often used like a constructor when turning some
        // ghetto array-like to a real array
        return self;
    },


    /** @id MochiKit.Base.updatetree */
    updatetree: function (self, obj/*, ...*/) {
        if (self === null || self === undefined) {
            self = {};
        }
        for (var i = 1; i < arguments.length; i++) {
            var o = arguments[i];
            if (typeof(o) != 'undefined' && o !== null) {
                for (var k in o) {
                    var v = o[k];
                    if (typeof(self[k]) == 'object' && typeof(v) == 'object') {
                        arguments.callee(self[k], v);
                    } else {
                        self[k] = v;
                    }
                }
            }
        }
        return self;
    },

    /** @id MochiKit.Base.setdefault */
    setdefault: function (self, obj/*, ...*/) {
        if (self === null || self === undefined) {
            self = {};
        }
        for (var i = 1; i < arguments.length; i++) {
            var o = arguments[i];
            for (var k in o) {
                if (!(k in self)) {
                    self[k] = o[k];
                }
            }
        }
        return self;
    },

    /** @id MochiKit.Base.keys */
    keys: function (obj) {
        var rval = [];
        for (var prop in obj) {
            rval.push(prop);
        }
        return rval;
    },

    /** @id MochiKit.Base.values */
    values: function (obj) {
        var rval = [];
        for (var prop in obj) {
            rval.push(obj[prop]);
        }
        return rval;
    },

     /** @id MochiKit.Base.items */
    items: function (obj) {
        var rval = [];
        var e;
        for (var prop in obj) {
            var v;
            try {
                v = obj[prop];
            } catch (e) {
                continue;
            }
            rval.push([prop, v]);
        }
        return rval;
    },


    _newNamedError: function (module, name, func) {
        func.prototype = new MochiKit.Base.NamedError(module.NAME + "." + name);
        func.prototype.constructor = func;
        module[name] = func;
    },


    /** @id MochiKit.Base.operator */
    operator: {
        // unary logic operators
        /** @id MochiKit.Base.truth */
        truth: function (a) { return !!a; },
        /** @id MochiKit.Base.lognot */
        lognot: function (a) { return !a; },
        /** @id MochiKit.Base.identity */
        identity: function (a) { return a; },

        // bitwise unary operators
        /** @id MochiKit.Base.not */
        not: function (a) { return ~a; },
        /** @id MochiKit.Base.neg */
        neg: function (a) { return -a; },

        // binary operators
        /** @id MochiKit.Base.add */
        add: function (a, b) { return a + b; },
        /** @id MochiKit.Base.sub */
        sub: function (a, b) { return a - b; },
        /** @id MochiKit.Base.div */
        div: function (a, b) { return a / b; },
        /** @id MochiKit.Base.mod */
        mod: function (a, b) { return a % b; },
        /** @id MochiKit.Base.mul */
        mul: function (a, b) { return a * b; },

        // bitwise binary operators
        /** @id MochiKit.Base.and */
        and: function (a, b) { return a & b; },
        /** @id MochiKit.Base.or */
        or: function (a, b) { return a | b; },
        /** @id MochiKit.Base.xor */
        xor: function (a, b) { return a ^ b; },
        /** @id MochiKit.Base.lshift */
        lshift: function (a, b) { return a << b; },
        /** @id MochiKit.Base.rshift */
        rshift: function (a, b) { return a >> b; },
        /** @id MochiKit.Base.zrshift */
        zrshift: function (a, b) { return a >>> b; },

        // near-worthless built-in comparators
        /** @id MochiKit.Base.eq */
        eq: function (a, b) { return a == b; },
        /** @id MochiKit.Base.ne */
        ne: function (a, b) { return a != b; },
        /** @id MochiKit.Base.gt */
        gt: function (a, b) { return a > b; },
        /** @id MochiKit.Base.ge */
        ge: function (a, b) { return a >= b; },
        /** @id MochiKit.Base.lt */
        lt: function (a, b) { return a < b; },
        /** @id MochiKit.Base.le */
        le: function (a, b) { return a <= b; },

        // strict built-in comparators
        seq: function (a, b) { return a === b; },
        sne: function (a, b) { return a !== b; },

        // compare comparators
        /** @id MochiKit.Base.ceq */
        ceq: function (a, b) { return MochiKit.Base.compare(a, b) === 0; },
        /** @id MochiKit.Base.cne */
        cne: function (a, b) { return MochiKit.Base.compare(a, b) !== 0; },
        /** @id MochiKit.Base.cgt */
        cgt: function (a, b) { return MochiKit.Base.compare(a, b) == 1; },
        /** @id MochiKit.Base.cge */
        cge: function (a, b) { return MochiKit.Base.compare(a, b) != -1; },
        /** @id MochiKit.Base.clt */
        clt: function (a, b) { return MochiKit.Base.compare(a, b) == -1; },
        /** @id MochiKit.Base.cle */
        cle: function (a, b) { return MochiKit.Base.compare(a, b) != 1; },

        // binary logical operators
        /** @id MochiKit.Base.logand */
        logand: function (a, b) { return a && b; },
        /** @id MochiKit.Base.logor */
        logor: function (a, b) { return a || b; },
        /** @id MochiKit.Base.contains */
        contains: function (a, b) { return b in a; }
    },

    /** @id MochiKit.Base.forwardCall */
    forwardCall: function (func) {
        return function () {
            return this[func].apply(this, arguments);
        };
    },

    /** @id MochiKit.Base.itemgetter */
    itemgetter: function (func) {
        return function (arg) {
            return arg[func];
        };
    },

    /** @id MochiKit.Base.bool */
    bool: function (value) {
        if (typeof(value) === "boolean" || value instanceof Boolean) {
            return value.valueOf();
        } else if (typeof(value) === "string" || value instanceof String) {
            return value.length > 0 && value != "false" && value != "null" &&
                   value != "undefined" && value != "0";
        } else if (typeof(value) === "number" || value instanceof Number) {
            return !isNaN(value) && value != 0;
        } else if (value != null && typeof(value.length) === "number") {
            return value.length !== 0;
        } else {
            return value != null;
        }
    },

    /** @id MochiKit.Base.typeMatcher */
    typeMatcher: function (/* typ */) {
        var types = {};
        for (var i = 0; i < arguments.length; i++) {
            var typ = arguments[i];
            types[typ] = typ;
        }
        return function () {
            for (var i = 0; i < arguments.length; i++) {
                if (!(typeof(arguments[i]) in types)) {
                    return false;
                }
            }
            return true;
        };
    },

    /** @id MochiKit.Base.isNull */
    isNull: function (/* ... */) {
        for (var i = 0; i < arguments.length; i++) {
            if (arguments[i] !== null) {
                return false;
            }
        }
        return true;
    },

    /** @id MochiKit.Base.isUndefinedOrNull */
    isUndefinedOrNull: function (/* ... */) {
        for (var i = 0; i < arguments.length; i++) {
            var o = arguments[i];
            if (!(typeof(o) == 'undefined' || o === null)) {
                return false;
            }
        }
        return true;
    },

    /** @id MochiKit.Base.isEmpty */
    isEmpty: function (obj) {
        return !MochiKit.Base.isNotEmpty.apply(this, arguments);
    },

    /** @id MochiKit.Base.isNotEmpty */
    isNotEmpty: function (obj) {
        for (var i = 0; i < arguments.length; i++) {
            var o = arguments[i];
            if (!(o && o.length)) {
                return false;
            }
        }
        return true;
    },

    /** @id MochiKit.Base.isArrayLike */
    isArrayLike: function () {
        for (var i = 0; i < arguments.length; i++) {
            var o = arguments[i];
            var typ = typeof(o);
            if (
                (typ != 'object' && !(typ == 'function' && typeof(o.item) == 'function')) ||
                o === null ||
                typeof(o.length) != 'number' ||
                o.nodeType === 3 ||
                o.nodeType === 4
            ) {
                return false;
            }
        }
        return true;
    },

    /** @id MochiKit.Base.isDateLike */
    isDateLike: function () {
        for (var i = 0; i < arguments.length; i++) {
            var o = arguments[i];
            if (typeof(o) != "object" || o === null
                    || typeof(o.getTime) != 'function') {
                return false;
            }
        }
        return true;
    },


    /** @id MochiKit.Base.xmap */
    xmap: function (fn/*, obj... */) {
        if (fn === null) {
            return MochiKit.Base.extend(null, arguments, 1);
        }
        var rval = [];
        for (var i = 1; i < arguments.length; i++) {
            rval.push(fn(arguments[i]));
        }
        return rval;
    },

    /** @id MochiKit.Base.map */
    map: function (fn, lst/*, lst... */) {
        var m = MochiKit.Base;
        var itr = MochiKit.Iter;
        var isArrayLike = m.isArrayLike;
        if (arguments.length <= 2) {
            // allow an iterable to be passed
            if (!isArrayLike(lst)) {
                if (itr) {
                    // fast path for map(null, iterable)
                    lst = itr.list(lst);
                    if (fn === null) {
                        return lst;
                    }
                } else {
                    throw new TypeError("Argument not an array-like and MochiKit.Iter not present");
                }
            }
            // fast path for map(null, lst)
            if (fn === null) {
                return m.extend(null, lst);
            }
            // disabled fast path for map(fn, lst)
            /*
            if (false && typeof(Array.prototype.map) == 'function') {
                // Mozilla fast-path
                return Array.prototype.map.call(lst, fn);
            }
            */
            var rval = [];
            for (var i = 0; i < lst.length; i++) {
                rval.push(fn(lst[i]));
            }
            return rval;
        } else {
            // default for map(null, ...) is zip(...)
            if (fn === null) {
                fn = Array;
            }
            var length = null;
            for (var i = 1; i < arguments.length; i++) {
                // allow iterables to be passed
                if (!isArrayLike(arguments[i])) {
                    if (itr) {
                        return itr.list(itr.imap.apply(null, arguments));
                    } else {
                        throw new TypeError("Argument not an array-like and MochiKit.Iter not present");
                    }
                }
                // find the minimum length
                var l = arguments[i].length;
                if (length === null || length > l) {
                    length = l;
                }
            }
            rval = [];
            for (var i = 0; i < length; i++) {
                var args = [];
                for (var j = 1; j < arguments.length; j++) {
                    args.push(arguments[j][i]);
                }
                rval.push(fn.apply(this, args));
            }
            return rval;
        }
    },

    /** @id MochiKit.Base.xfilter */
    xfilter: function (fn/*, obj... */) {
        var rval = [];
        if (fn === null) {
            fn = MochiKit.Base.operator.truth;
        }
        for (var i = 1; i < arguments.length; i++) {
            var o = arguments[i];
            if (fn(o)) {
                rval.push(o);
            }
        }
        return rval;
    },

    /** @id MochiKit.Base.filter */
    filter: function (fn, lst, self) {
        var rval = [];
        // allow an iterable to be passed
        var m = MochiKit.Base;
        if (!m.isArrayLike(lst)) {
            if (MochiKit.Iter) {
                lst = MochiKit.Iter.list(lst);
            } else {
                throw new TypeError("Argument not an array-like and MochiKit.Iter not present");
            }
        }
        if (fn === null) {
            fn = m.operator.truth;
        }
        if (typeof(Array.prototype.filter) == 'function') {
            // Mozilla fast-path
            return Array.prototype.filter.call(lst, fn, self);
        } else if (typeof(self) == 'undefined' || self === null) {
            for (var i = 0; i < lst.length; i++) {
                var o = lst[i];
                if (fn(o)) {
                    rval.push(o);
                }
            }
        } else {
            for (var i = 0; i < lst.length; i++) {
                o = lst[i];
                if (fn.call(self, o)) {
                    rval.push(o);
                }
            }
        }
        return rval;
    },


    _wrapDumbFunction: function (func) {
        return function () {
            // fast path!
            switch (arguments.length) {
                case 0: return func();
                case 1: return func(arguments[0]);
                case 2: return func(arguments[0], arguments[1]);
                case 3: return func(arguments[0], arguments[1], arguments[2]);
            }
            var args = [];
            for (var i = 0; i < arguments.length; i++) {
                args.push("arguments[" + i + "]");
            }
            return eval("(func(" + args.join(",") + "))");
        };
    },

    /** @id MochiKit.Base.methodcaller */
    methodcaller: function (func/*, args... */) {
        var args = MochiKit.Base.extend(null, arguments, 1);
        if (typeof(func) == "function") {
            return function (obj) {
                return func.apply(obj, args);
            };
        } else {
            return function (obj) {
                return obj[func].apply(obj, args);
            };
        }
    },

    /** @id MochiKit.Base.method */
    method: function (self, func) {
        var m = MochiKit.Base;
        return m.bind.apply(this, m.extend([func, self], arguments, 2));
    },

    /** @id MochiKit.Base.compose */
    compose: function (f1, f2/*, f3, ... fN */) {
        var fnlist = [];
        var m = MochiKit.Base;
        if (arguments.length === 0) {
            throw new TypeError("compose() requires at least one argument");
        }
        for (var i = 0; i < arguments.length; i++) {
            var fn = arguments[i];
            if (typeof(fn) != "function") {
                throw new TypeError(m.repr(fn) + " is not a function");
            }
            fnlist.push(fn);
        }
        return function () {
            var args = arguments;
            for (var i = fnlist.length - 1; i >= 0; i--) {
                args = [fnlist[i].apply(this, args)];
            }
            return args[0];
        };
    },

    /** @id MochiKit.Base.bind */
    bind: function (func, self/* args... */) {
        if (typeof(func) == "string") {
            func = self[func];
        }
        var im_func = func.im_func;
        var im_preargs = func.im_preargs;
        var im_self = func.im_self;
        var m = MochiKit.Base;
        if (typeof(func) == "function" && typeof(func.apply) == "undefined") {
            // this is for cases where JavaScript sucks ass and gives you a
            // really dumb built-in function like alert() that doesn't have
            // an apply
            func = m._wrapDumbFunction(func);
        }
        if (typeof(im_func) != 'function') {
            im_func = func;
        }
        if (typeof(self) != 'undefined') {
            im_self = self;
        }
        if (typeof(im_preargs) == 'undefined') {
            im_preargs = [];
        } else  {
            im_preargs = im_preargs.slice();
        }
        m.extend(im_preargs, arguments, 2);
        var newfunc = function () {
            var args = arguments;
            var me = arguments.callee;
            if (me.im_preargs.length > 0) {
                args = m.concat(me.im_preargs, args);
            }
            var self = me.im_self;
            if (!self) {
                self = this;
            }
            return me.im_func.apply(self, args);
        };
        newfunc.im_self = im_self;
        newfunc.im_func = im_func;
        newfunc.im_preargs = im_preargs;
        if (typeof(im_func.NAME) == 'string') {
            newfunc.NAME = "bind(" + im_func.NAME + ",...)";
        }
        return newfunc;
    },

    /** @id MochiKit.Base.bindLate */
    bindLate: function (func, self/* args... */) {
        var m = MochiKit.Base;
        var args = arguments;
        if (typeof(func) === "string") {
            args = m.extend([m.forwardCall(func)], arguments, 1);
            return m.bind.apply(this, args);
        }
        return m.bind.apply(this, args);
    },

    /** @id MochiKit.Base.bindMethods */
    bindMethods: function (self) {
        var bind = MochiKit.Base.bind;
        for (var k in self) {
            var func = self[k];
            if (typeof(func) == 'function') {
                self[k] = bind(func, self);
            }
        }
    },

    /** @id MochiKit.Base.registerComparator */
    registerComparator: function (name, check, comparator, /* optional */ override) {
        MochiKit.Base.comparatorRegistry.register(name, check, comparator, override);
    },

    _primitives: {'boolean': true, 'string': true, 'number': true},

    /** @id MochiKit.Base.compare */
    compare: function (a, b) {
        if (a == b) {
            return 0;
        }
        var aIsNull = (typeof(a) == 'undefined' || a === null);
        var bIsNull = (typeof(b) == 'undefined' || b === null);
        if (aIsNull && bIsNull) {
            return 0;
        } else if (aIsNull) {
            return -1;
        } else if (bIsNull) {
            return 1;
        }
        var m = MochiKit.Base;
        // bool, number, string have meaningful comparisons
        var prim = m._primitives;
        if (!(typeof(a) in prim && typeof(b) in prim)) {
            try {
                return m.comparatorRegistry.match(a, b);
            } catch (e) {
                if (e != m.NotFound) {
                    throw e;
                }
            }
        }
        if (a < b) {
            return -1;
        } else if (a > b) {
            return 1;
        }
        // These types can't be compared
        var repr = m.repr;
        throw new TypeError(repr(a) + " and " + repr(b) + " can not be compared");
    },

    /** @id MochiKit.Base.compareDateLike */
    compareDateLike: function (a, b) {
        return MochiKit.Base.compare(a.getTime(), b.getTime());
    },

    /** @id MochiKit.Base.compareArrayLike */
    compareArrayLike: function (a, b) {
        var compare = MochiKit.Base.compare;
        var count = a.length;
        var rval = 0;
        if (count > b.length) {
            rval = 1;
            count = b.length;
        } else if (count < b.length) {
            rval = -1;
        }
        for (var i = 0; i < count; i++) {
            var cmp = compare(a[i], b[i]);
            if (cmp) {
                return cmp;
            }
        }
        return rval;
    },

    /** @id MochiKit.Base.registerRepr */
    registerRepr: function (name, check, wrap, /* optional */override) {
        MochiKit.Base.reprRegistry.register(name, check, wrap, override);
    },

    /** @id MochiKit.Base.repr */
    repr: function (o) {
        if (typeof(o) == "undefined") {
            return "undefined";
        } else if (o === null) {
            return "null";
        }
        try {
            if (typeof(o.__repr__) == 'function') {
                return o.__repr__();
            } else if (typeof(o.repr) == 'function' && o.repr != arguments.callee) {
                return o.repr();
            }
            return MochiKit.Base.reprRegistry.match(o);
        } catch (e) {
            try {
                if (typeof(o.NAME) == 'string' && (
                        o.toString == Function.prototype.toString ||
                        o.toString == Object.prototype.toString
                    )) {
                    return o.NAME;
                }
            } catch (ignore) {
            }
        }
        try {
            var ostring = (o + "");
        } catch (e) {
            return "[" + typeof(o) + "]";
        }
        if (typeof(o) == "function") {
            ostring = ostring.replace(/^\s+/, "").replace(/\s+/g, " ");
            ostring = ostring.replace(/,(\S)/, ", $1");
            var idx = ostring.indexOf("{");
            if (idx != -1) {
                ostring = ostring.substr(0, idx) + "{...}";
            }
        }
        return ostring;
    },

    /** @id MochiKit.Base.reprArrayLike */
    reprArrayLike: function (o) {
        var m = MochiKit.Base;
        return "[" + m.map(m.repr, o).join(", ") + "]";
    },

    /** @id MochiKit.Base.reprString */
    reprString: function (o) {
        return ('"' + o.replace(/(["\\])/g, '\\$1') + '"'
            ).replace(/[\f]/g, "\\f"
            ).replace(/[\b]/g, "\\b"
            ).replace(/[\n]/g, "\\n"
            ).replace(/[\t]/g, "\\t"
            ).replace(/[\v]/g, "\\v"
            ).replace(/[\r]/g, "\\r");
    },

    /** @id MochiKit.Base.reprNumber */
    reprNumber: function (o) {
        return o + "";
    },

    /** @id MochiKit.Base.registerJSON */
    registerJSON: function (name, check, wrap, /* optional */override) {
        MochiKit.Base.jsonRegistry.register(name, check, wrap, override);
    },


    /** @id MochiKit.Base.evalJSON */
    evalJSON: function (jsonText) {
        return eval("(" + MochiKit.Base._filterJSON(jsonText) + ")");
    },

    _filterJSON: function (s) {
        var m = s.match(/^\s*\/\*(.*)\*\/\s*$/);
        return (m) ? m[1] : s;
    },

    /** @id MochiKit.Base.serializeJSON */
    serializeJSON: function (o) {
        var objtype = typeof(o);
        if (objtype == "number" || objtype == "boolean") {
            return o + "";
        } else if (o === null) {
            return "null";
        } else if (objtype == "string") {
            var res = "";
            for (var i = 0; i < o.length; i++) {
                var c = o.charAt(i);
                if (c == '\"') {
                    res += '\\"';
                } else if (c == '\\') {
                    res += '\\\\';
                } else if (c == '\b') {
                    res += '\\b';
                } else if (c == '\f') {
                    res += '\\f';
                } else if (c == '\n') {
                    res += '\\n';
                } else if (c == '\r') {
                    res += '\\r';
                } else if (c == '\t') {
                    res += '\\t';
                } else if (o.charCodeAt(i) <= 0x1F) {
                    var hex = o.charCodeAt(i).toString(16);
                    if (hex.length < 2) {
                        hex = '0' + hex;
                    }
                    res += '\\u00' + hex.toUpperCase();
                } else {
                    res += c;
                }
            }
            return '"' + res + '"';
        }
        // recurse
        var me = arguments.callee;
        // short-circuit for objects that support "json" serialization
        // if they return "self" then just pass-through...
        var newObj;
        if (typeof(o.toJSON) == "function") {
            newObj = o.toJSON();
            if (o !== newObj) {
                return me(newObj);
            }
        }
        if (typeof(o.__json__) == "function") {
            newObj = o.__json__();
            if (o !== newObj) {
                return me(newObj);
            }
        }
        if (typeof(o.json) == "function") {
            newObj = o.json();
            if (o !== newObj) {
                return me(newObj);
            }
        }
        // array
        if (objtype != "function" && typeof(o.length) == "number") {
            var res = [];
            for (var i = 0; i < o.length; i++) {
                var val = me(o[i]);
                if (typeof(val) != "string") {
                    // skip non-serializable values
                    continue;
                }
                res.push(val);
            }
            return "[" + res.join(", ") + "]";
        }
        // look in the registry
        var m = MochiKit.Base;
        try {
            newObj = m.jsonRegistry.match(o);
            if (o !== newObj) {
                return me(newObj);
            }
        } catch (e) {
            if (e != m.NotFound) {
                // something really bad happened
                throw e;
            }
        }
        // undefined is outside of the spec
        if (objtype == "undefined") {
            throw new TypeError("undefined can not be serialized as JSON");
        }
        // it's a function with no adapter, bad
        if (objtype == "function") {
            return null;
        }
        // generic object code path
        res = [];
        for (var k in o) {
            var useKey;
            if (typeof(k) == "number") {
                useKey = '"' + k + '"';
            } else if (typeof(k) == "string") {
                useKey = me(k);
            } else {
                // skip non-string or number keys
                continue;
            }
            val = me(o[k]);
            if (typeof(val) != "string") {
                // skip non-serializable values
                continue;
            }
            res.push(useKey + ":" + val);
        }
        return "{" + res.join(", ") + "}";
    },


    /** @id MochiKit.Base.objEqual */
    objEqual: function (a, b) {
        return (MochiKit.Base.compare(a, b) === 0);
    },

    /** @id MochiKit.Base.arrayEqual */
    arrayEqual: function (self, arr) {
        if (self.length != arr.length) {
            return false;
        }
        return (MochiKit.Base.compare(self, arr) === 0);
    },

    /** @id MochiKit.Base.concat */
    concat: function (/* lst... */) {
        var rval = [];
        var extend = MochiKit.Base.extend;
        for (var i = 0; i < arguments.length; i++) {
            extend(rval, arguments[i]);
        }
        return rval;
    },

    /** @id MochiKit.Base.keyComparator */
    keyComparator: function (key/* ... */) {
        // fast-path for single key comparisons
        var m = MochiKit.Base;
        var compare = m.compare;
        if (arguments.length == 1) {
            return function (a, b) {
                return compare(a[key], b[key]);
            };
        }
        var compareKeys = m.extend(null, arguments);
        return function (a, b) {
            var rval = 0;
            // keep comparing until something is inequal or we run out of
            // keys to compare
            for (var i = 0; (rval === 0) && (i < compareKeys.length); i++) {
                var key = compareKeys[i];
                rval = compare(a[key], b[key]);
            }
            return rval;
        };
    },

    /** @id MochiKit.Base.reverseKeyComparator */
    reverseKeyComparator: function (key) {
        var comparator = MochiKit.Base.keyComparator.apply(this, arguments);
        return function (a, b) {
            return comparator(b, a);
        };
    },

    /** @id MochiKit.Base.partial */
    partial: function (func) {
        var m = MochiKit.Base;
        return m.bind.apply(this, m.extend([func, undefined], arguments, 1));
    },

    /** @id MochiKit.Base.listMinMax */
    listMinMax: function (which, lst) {
        if (lst.length === 0) {
            return null;
        }
        var cur = lst[0];
        var compare = MochiKit.Base.compare;
        for (var i = 1; i < lst.length; i++) {
            var o = lst[i];
            if (compare(o, cur) == which) {
                cur = o;
            }
        }
        return cur;
    },

    /** @id MochiKit.Base.objMax */
    objMax: function (/* obj... */) {
        return MochiKit.Base.listMinMax(1, arguments);
    },

    /** @id MochiKit.Base.objMin */
    objMin: function (/* obj... */) {
        return MochiKit.Base.listMinMax(-1, arguments);
    },

    /** @id MochiKit.Base.findIdentical */
    findIdentical: function (lst, value, start/* = 0 */, /* optional */end) {
        if (typeof(end) == "undefined" || end === null) {
            end = lst.length;
        }
        if (typeof(start) == "undefined" || start === null) {
            start = 0;
        }
        for (var i = start; i < end; i++) {
            if (lst[i] === value) {
                return i;
            }
        }
        return -1;
    },

    /** @id MochiKit.Base.mean */
    mean: function(/* lst... */) {
        /* http://www.nist.gov/dads/HTML/mean.html */
        var sum = 0;

        var m = MochiKit.Base;
        var args = m.extend(null, arguments);
        var count = args.length;

        while (args.length) {
            var o = args.shift();
            if (o && typeof(o) == "object" && typeof(o.length) == "number") {
                count += o.length - 1;
                for (var i = o.length - 1; i >= 0; i--) {
                    sum += o[i];
                }
            } else {
                sum += o;
            }
        }

        if (count <= 0) {
            throw new TypeError('mean() requires at least one argument');
        }

        return sum/count;
    },

    /** @id MochiKit.Base.median */
    median: function(/* lst... */) {
        /* http://www.nist.gov/dads/HTML/median.html */
        var data = MochiKit.Base.flattenArguments(arguments);
        if (data.length === 0) {
            throw new TypeError('median() requires at least one argument');
        }
        data.sort(MochiKit.Base.compare);
        if (data.length % 2 == 0) {
            var upper = data.length / 2;
            return (data[upper] + data[upper - 1]) / 2;
        } else {
            return data[(data.length - 1) / 2];
        }
    },

    /** @id MochiKit.Base.findValue */
    findValue: function (lst, value, start/* = 0 */, /* optional */end) {
        if (typeof(end) == "undefined" || end === null) {
            end = lst.length;
        }
        if (typeof(start) == "undefined" || start === null) {
            start = 0;
        }
        var cmp = MochiKit.Base.compare;
        for (var i = start; i < end; i++) {
            if (cmp(lst[i], value) === 0) {
                return i;
            }
        }
        return -1;
    },

    /** @id MochiKit.Base.nodeWalk */
    nodeWalk: function (node, visitor) {
        var nodes = [node];
        var extend = MochiKit.Base.extend;
        while (nodes.length) {
            var res = visitor(nodes.shift());
            if (res) {
                extend(nodes, res);
            }
        }
    },


    /** @id MochiKit.Base.nameFunctions */
    nameFunctions: function (namespace) {
        var base = namespace.NAME;
        if (typeof(base) == 'undefined') {
            base = '';
        } else {
            base = base + '.';
        }
        for (var name in namespace) {
            var o = namespace[name];
            if (typeof(o) == 'function' && typeof(o.NAME) == 'undefined') {
                try {
                    o.NAME = base + name;
                } catch (e) {
                    // pass
                }
            }
        }
    },


    /** @id MochiKit.Base.queryString */
    queryString: function (names, values) {
        // check to see if names is a string or a DOM element, and if
        // MochiKit.DOM is available.  If so, drop it like it's a form
        // Ugliest conditional in MochiKit?  Probably!
        if (typeof(MochiKit.DOM) != "undefined" && arguments.length == 1
            && (typeof(names) == "string" || (
                typeof(names.nodeType) != "undefined" && names.nodeType > 0
            ))
        ) {
            var kv = MochiKit.DOM.formContents(names);
            names = kv[0];
            values = kv[1];
        } else if (arguments.length == 1) {
            // Allow the return value of formContents to be passed directly
            if (typeof(names.length) == "number" && names.length == 2) {
                return arguments.callee(names[0], names[1]);
            }
            var o = names;
            names = [];
            values = [];
            for (var k in o) {
                var v = o[k];
                if (typeof(v) == "function") {
                    continue;
                } else if (MochiKit.Base.isArrayLike(v)){
                    for (var i = 0; i < v.length; i++) {
                        names.push(k);
                        values.push(v[i]);
                    }
                } else {
                    names.push(k);
                    values.push(v);
                }
            }
        }
        var rval = [];
        var len = Math.min(names.length, values.length);
        var urlEncode = MochiKit.Base.urlEncode;
        for (var i = 0; i < len; i++) {
            v = values[i];
            if (typeof(v) != 'undefined' && v !== null) {
                rval.push(urlEncode(names[i]) + "=" + urlEncode(v));
            }
        }
        return rval.join("&");
    },


    /** @id MochiKit.Base.parseQueryString */
    parseQueryString: function (encodedString, useArrays) {
        // strip a leading '?' from the encoded string
        var qstr = (encodedString.charAt(0) == "?")
            ? encodedString.substring(1)
            : encodedString;
        var pairs = qstr.replace(/\+/g, "%20").split(/\&amp\;|\&\#38\;|\&#x26;|\&/);
        var o = {};
        var decode;
        if (typeof(decodeURIComponent) != "undefined") {
            decode = decodeURIComponent;
        } else {
            decode = unescape;
        }
        if (useArrays) {
            for (var i = 0; i < pairs.length; i++) {
                var pair = pairs[i].split("=");
                var name = decode(pair.shift());
                if (!name) {
                    continue;
                }
                var arr = o[name];
                if (!(arr instanceof Array)) {
                    arr = [];
                    o[name] = arr;
                }
                arr.push(decode(pair.join("=")));
            }
        } else {
            for (var i = 0; i < pairs.length; i++) {
                pair = pairs[i].split("=");
                var name = pair.shift();
                if (!name) {
                    continue;
                }
                o[decode(name)] = decode(pair.join("="));
            }
        }
        return o;
    }
});

/** @id MochiKit.Base.AdapterRegistry */
MochiKit.Base.AdapterRegistry = function () {
    this.pairs = [];
};

MochiKit.Base.AdapterRegistry.prototype = {
    /** @id MochiKit.Base.AdapterRegistry.prototype.register */
    register: function (name, check, wrap, /* optional */ override) {
        if (override) {
            this.pairs.unshift([name, check, wrap]);
        } else {
            this.pairs.push([name, check, wrap]);
        }
    },

    /** @id MochiKit.Base.AdapterRegistry.prototype.match */
    match: function (/* ... */) {
        for (var i = 0; i < this.pairs.length; i++) {
            var pair = this.pairs[i];
            if (pair[1].apply(this, arguments)) {
                return pair[2].apply(this, arguments);
            }
        }
        throw MochiKit.Base.NotFound;
    },

    /** @id MochiKit.Base.AdapterRegistry.prototype.unregister */
    unregister: function (name) {
        for (var i = 0; i < this.pairs.length; i++) {
            var pair = this.pairs[i];
            if (pair[0] == name) {
                this.pairs.splice(i, 1);
                return true;
            }
        }
        return false;
    }
};

/**
 * Exports all symbols from one or more modules into the specified
 * namespace (or scope). This is similar to MochiKit.Base.update(),
 * except for special handling of the "__export__" flag, contained
 * sub-modules (exported recursively), and names starting with "_".
 *
 * @param {Object} namespace the object or scope to modify
 * @param {Object} module the module to export
 */
MochiKit.Base.moduleExport = function (namespace, module/*, ...*/) {
    var SKIP = { toString: true, NAME: true, VERSION: true };
    var mods = MochiKit.Base.extend([], arguments, 1);
    while ((module = mods.shift()) != null) {
        for (var k in module) {
            var v = module[k];
            if (v != null) {
                var flagSet = (typeof(v.__export__) == 'boolean');
                var nameValid = (k[0] !== "_" && !SKIP[k]);
                if (flagSet ? v.__export__ : nameValid) {
                    if (typeof(v) == 'object' && v.NAME && v.VERSION) {
                        mods.push(v);
                    } else {
                        namespace[k] = module[k];
                    }
                }
            }
        }
    }
    return namespace;
};

/**
 * Identical to moduleExport, but also considers the global and
 * module-specific "__export__" flag.
 */
MochiKit.Base._exportSymbols = function (namespace, module) {
    if (MochiKit.__export__ !== false && module.__export__ !== false) {
        MochiKit.Base.moduleExport(namespace, module);
    }
};

/**
 * Creates a deprecated function alias in the specified module. The
 * deprecated function will forward all calls and arguments to a
 * target function, while also logging a debug message on the first
 * call (if MochiKit.Logging is loaded). The destination function may
 * be located in another module, which must be loaded, or an
 * exception will be thrown.
 *
 * @param {Object/String} module the source module or module name
 *            (e.g. 'DOM' or 'MochiKit.DOM')
 * @param {String} name the deprecated function name (e.g. 'getStyle')
 * @param {String} target the fully qualified name of the target
 *            function (e.g. 'MochiKit.Style.getStyle')
 * @param {String} version the first version when the source function
 *            was deprecated (e.g. '1.4')
 * @param {Boolean} [exportable] the exportable function flag,
 *            defaults to false
 */
MochiKit.Base._deprecated = function (module, name, target, version, exportable) {
    if (typeof(module) === 'string') {
        if (module.indexOf('MochiKit.') === 0) {
            module = module.substring(9);
        }
        module = MochiKit[module];
    }
    var targetModule = target.split('.')[1];
    var targetName = target.split('.')[2];
    var func = function () {
        var self = arguments.callee;
        var msg = module.NAME + '.' + name + ' is deprecated since version ' +
                  version + '. Use ' + target + ' instead.';
        if (self.logged !== true) {
            self.logged = true;
            if (MochiKit.Logging) {
                MochiKit.Logging.logDebug(msg);
            } else if (console && console.log) {
                console.log(msg);
            }
        }
        if (!MochiKit[targetModule]) {
            throw new Error(msg);
        }
        return MochiKit[targetModule][targetName].apply(this, arguments);
    };
    func.__export__ = (exportable === true);
    module[name] = func;
};

MochiKit.Base.__new__ = function () {
    var m = this;

    /** @id MochiKit.Base.noop */
    m.noop = m.operator.identity;

    // Backwards compat
    m._deprecated(m, 'forward', 'MochiKit.Base.forwardCall', '1.3');
    m._deprecated(m, 'find', 'MochiKit.Base.findValue', '1.3');

    if (typeof(encodeURIComponent) != "undefined") {
        /** @id MochiKit.Base.urlEncode */
        m.urlEncode = function (unencoded) {
            return encodeURIComponent(unencoded).replace(/\'/g, '%27');
        };
    } else {
        m.urlEncode = function (unencoded) {
            return escape(unencoded
                ).replace(/\+/g, '%2B'
                ).replace(/\"/g,'%22'
                ).replace(/\'/g, '%27');
        };
    }

    /** @id MochiKit.Base.NamedError */
    m.NamedError = function (name) {
        this.message = name;
        this.name = name;
    };
    m.NamedError.prototype = new Error();
    m.NamedError.prototype.constructor = m.NamedError;
    m.update(m.NamedError.prototype, {
        repr: function () {
            if (this.message && this.message != this.name) {
                return this.name + "(" + m.repr(this.message) + ")";
            } else {
                return this.name + "()";
            }
        },
        toString: m.forwardCall("repr")
    });

    /** @id MochiKit.Base.NotFound */
    m.NotFound = new m.NamedError("MochiKit.Base.NotFound");


    /** @id MochiKit.Base.listMax */
    m.listMax = m.partial(m.listMinMax, 1);
    /** @id MochiKit.Base.listMin */
    m.listMin = m.partial(m.listMinMax, -1);

    /** @id MochiKit.Base.isCallable */
    m.isCallable = m.typeMatcher('function');
    /** @id MochiKit.Base.isUndefined */
    m.isUndefined = m.typeMatcher('undefined');
    /** @id MochiKit.Base.isValue */
    m.isValue = m.typeMatcher('boolean', 'number', 'string');

    /** @id MochiKit.Base.merge */
    m.merge = m.partial(m.update, null);
    /** @id MochiKit.Base.zip */
    m.zip = m.partial(m.map, null);

    /** @id MochiKit.Base.average */
    m.average = m.mean;

    /** @id MochiKit.Base.comparatorRegistry */
    m.comparatorRegistry = new m.AdapterRegistry();
    m.registerComparator("dateLike", m.isDateLike, m.compareDateLike);
    m.registerComparator("arrayLike", m.isArrayLike, m.compareArrayLike);

    /** @id MochiKit.Base.reprRegistry */
    m.reprRegistry = new m.AdapterRegistry();
    m.registerRepr("arrayLike", m.isArrayLike, m.reprArrayLike);
    m.registerRepr("string", m.typeMatcher("string"), m.reprString);
    m.registerRepr("numbers", m.typeMatcher("number", "boolean"), m.reprNumber);

    /** @id MochiKit.Base.jsonRegistry */
    m.jsonRegistry = new m.AdapterRegistry();

    m.nameFunctions(this);

};

MochiKit.Base.__new__();

//
// XXX: Internet Explorer blows
//
if (MochiKit.__export__) {
    compare = MochiKit.Base.compare;
    compose = MochiKit.Base.compose;
    serializeJSON = MochiKit.Base.serializeJSON;
    mean = MochiKit.Base.mean;
    median = MochiKit.Base.median;
}

MochiKit.Base._exportSymbols(this, MochiKit.Base);
