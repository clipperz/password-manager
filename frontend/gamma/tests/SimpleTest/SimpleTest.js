/**
 * SimpleTest, a partial Test.Simple/Test.More API compatible test library.
 *
 * Why?
 *
 * Test.Simple doesn't work on IE < 6.
 * TODO:
 *  * Support the Test.Simple API used by MochiKit, to be able to test MochiKit 
 * itself against IE 5.5
 *
**/

if (typeof(SimpleTest) == "undefined") {
    var SimpleTest = {};
}

// Check to see if the TestRunner is present and has logging
if (typeof(parent) != "undefined" && parent.TestRunner) {
    SimpleTest._logEnabled = parent.TestRunner.logEnabled;
}

SimpleTest._tests = [];
SimpleTest._stopOnLoad = true;
SimpleTest._scopeCopy = {};

/**
 * Saves a copy of the specified scope variables.
 */
SimpleTest.saveScope = function (scope) {
    SimpleTest._scopeCopy = {};
    for (var k in scope) {
        SimpleTest._scopeCopy[k] = scope[k];
    }
}

/**
 * Verifies the specified scope against the stored copy and reports
 * any differences as test failures.
 */
SimpleTest.verifyScope = function (scope) {
    var filter = ['test', '_firebug','_FirebugConsole','XMLHttpRequest','Audio',
                  'XSLTProcessor','Option','Image','scrollMaxX','scrollMaxY',
                  'clipboardData'];
    for (var k in scope) {
        if (MochiKit.Base.findValue(filter, k) < 0) {
            var v = scope[k];
            var old = SimpleTest._scopeCopy[k];
            if (v !== old && typeof(old) === "undefined") {
                SimpleTest.ok(false, "scope modified, variable " + k + " was added");
            } else if (v !== old) {
                SimpleTest.ok(false, "scope modified, variable " + k + " changed from: " + old + ", to: " + v);
            }
        }
    }
    for (var k in SimpleTest._scopeCopy) {
        if (!(k in scope)) {
            SimpleTest.ok(false, "scope modified, variable " + k + " has been removed");
        }
    }
}

/**
 * Something like assert.
 */
SimpleTest.ok = function (condition, name, diag) {
    var test = {'result': !!condition, 'name': name, 'diag': diag || ""};
    if (SimpleTest._logEnabled) {
        var msg = test.result ? "PASS" : "FAIL";
        msg += " | " + test.name;
        if (test.result) {
            parent.TestRunner.logger.log(msg);
        } else {
            msg += " | " + test.diag;
            parent.TestRunner.logger.error(msg);
        }
    }
    SimpleTest._tests.push(test);
};

/**
 * Roughly equivalent to ok(a==b, name)
 */
SimpleTest.is = function (a, b, name) {
    var repr = MochiKit.Base.repr;
    SimpleTest.ok(a == b, name, "got " + repr(a) + ", expected " + repr(b));
};

/**
 * Roughly equivalent to ok(compare(a,b)==0, name)
 */
SimpleTest.eq = function (a, b, name) {
    var base = MochiKit.Base;
    var repr = base.repr;
    try {
        SimpleTest.ok(base.compare(a, b) == 0, name, "got " + repr(a) + ", expected " + repr(b));
    } catch (e) {
        SimpleTest.ok(false, name, "exception in compare: " + repr(e));
    }
};


/**
 * Makes a test report, returns it as a DIV element.
**/
SimpleTest.report = function () {
    var DIV = MochiKit.DOM.DIV;
    var passed = 0;
    var failed = 0;
    var results = MochiKit.Base.map(
        function (test) {
            var cls, msg;
            if (test.result) {
                passed++;
                cls = "test_ok";
                msg = "ok - " + test.name;
            } else {
                failed++;
                cls = "test_not_ok";
                msg = "not ok - " + test.name;
                if (test.diag != null && test.diag != "") {
                    msg += ": " + test.diag;
                }
            }
            return DIV({"class": cls}, msg);
        },
        SimpleTest._tests
    );
    var summary_class = ((failed == 0) ? 'all_pass' : 'some_fail');
    return DIV({'class': 'tests_report'},
        DIV({'class': 'tests_summary ' + summary_class},
            DIV({'class': 'tests_passed'}, "Passed: " + passed),
            DIV({'class': 'tests_failed'}, "Failed: " + failed)),
        results
    );
};

/**
 * Toggle element visibility
**/
SimpleTest.toggle = function(el) {
    if (MochiKit.Style.getStyle(el, 'display') == 'block') {
        el.style.display = 'none';
    } else {
        el.style.display = 'block';
    }
};

/**
 * Toggle visibility for divs with a specific class.
**/
SimpleTest.toggleByClass = function (cls) {
    var elems = MochiKit.DOM.getElementsByTagAndClassName('div', cls);
    MochiKit.Base.map(SimpleTest.toggle, elems);
	return false;
};

/**
 * Shows the report in the browser
**/

SimpleTest.showReport = function() {
    var base = MochiKit.Base;
    var dom = MochiKit.DOM;
    var togglePassed = dom.A({'href': '#'}, "Toggle passed tests");
    var toggleFailed = dom.A({'href': '#'}, "Toggle failed tests");
    togglePassed.onclick = base.partial(SimpleTest.toggleByClass, 'test_ok');
    toggleFailed.onclick = base.partial(SimpleTest.toggleByClass, 'test_not_ok');
    var body = document.getElementsByTagName("body")[0];
    var firstChild = body.childNodes[0];
    var addNode;
    if (firstChild) {
        addNode = function (el) {
            body.insertBefore(el, firstChild);
        };
    } else {
        addNode = function (el) {
            body.appendChild(el)
        };
    }
    addNode(togglePassed);
    addNode(dom.SPAN(null, " "));
    addNode(toggleFailed);
    addNode(SimpleTest.report());
};

/**
 * Tells SimpleTest to don't finish the test when the document is loaded,
 * useful for asynchronous tests.
 *
 * When SimpleTest.waitForExplicitFinish is called,
 * explicit SimpleTest.finish() is required.
**/
SimpleTest.waitForExplicitFinish = function () {
    SimpleTest._stopOnLoad = false;
};

/**
 * Talks to the TestRunner if being ran on a iframe and the parent has a 
 * TestRunner object.
**/
SimpleTest.talkToRunner = function () {
    if (typeof(parent) != "undefined" && parent.TestRunner) {
        parent.TestRunner.testFinished(document);
    }
};

/**
 * Finishes the tests. This is automatically called, except when 
 * SimpleTest.waitForExplicitFinish() has been invoked.
**/
SimpleTest.finish = function () {
    SimpleTest.showReport();
    SimpleTest.talkToRunner();
};


MochiKit.DOM.addLoadEvent(function() {
    if (SimpleTest._stopOnLoad) {
        SimpleTest.finish();
    }
});

//  --------------- Test.Builder/Test.More isDeeply() -----------------


SimpleTest.DNE = {dne: 'Does not exist'};
SimpleTest.LF = "\r\n";
SimpleTest._isRef = function (object) {
    var type = typeof(object);
    return type == 'object' || type == 'function';
};


SimpleTest._deepCheck = function (e1, e2, stack, seen) {
    var ok = false;
    // Either they're both references or both not.
    var sameRef = !(!SimpleTest._isRef(e1) ^ !SimpleTest._isRef(e2));
    if (e1 == null && e2 == null) {
        ok = true;
    } else if (e1 != null ^ e2 != null) {
        ok = false;
    } else if (e1 == SimpleTest.DNE ^ e2 == SimpleTest.DNE) {
        ok = false;
    } else if (sameRef && e1 == e2) {
        // Handles primitives and any variables that reference the same
        // object, including functions.
        ok = true;
    } else if (SimpleTest.isa(e1, 'Array') && SimpleTest.isa(e2, 'Array')) {
        ok = SimpleTest._eqArray(e1, e2, stack, seen);
    } else if (typeof e1 == "object" && typeof e2 == "object") {
        ok = SimpleTest._eqAssoc(e1, e2, stack, seen);
    } else {
        // If we get here, they're not the same (function references must
        // always simply rererence the same function).
        stack.push({ vals: [e1, e2] });
        ok = false;
    }
    return ok;
};

SimpleTest._eqArray = function (a1, a2, stack, seen) {
    // Return if they're the same object.
    if (a1 == a2) return true;

    // JavaScript objects have no unique identifiers, so we have to store
    // references to them all in an array, and then compare the references
    // directly. It's slow, but probably won't be much of an issue in
    // practice. Start by making a local copy of the array to as to avoid
    // confusing a reference seen more than once (such as [a, a]) for a
    // circular reference.
    for (var j = 0; j < seen.length; j++) {
        if (seen[j][0] == a1) {
            return seen[j][1] == a2;
        }
    }

    // If we get here, we haven't seen a1 before, so store it with reference
    // to a2.
    seen.push([ a1, a2 ]);

    var ok = true;
    // Only examines enumerable attributes. Only works for numeric arrays!
    // Associative arrays return 0. So call _eqAssoc() for them, instead.
    var max = a1.length > a2.length ? a1.length : a2.length;
    if (max == 0) return SimpleTest._eqAssoc(a1, a2, stack, seen);
    for (var i = 0; i < max; i++) {
        var e1 = i > a1.length - 1 ? SimpleTest.DNE : a1[i];
        var e2 = i > a2.length - 1 ? SimpleTest.DNE : a2[i];
        stack.push({ type: 'Array', idx: i, vals: [e1, e2] });
        if (ok = SimpleTest._deepCheck(e1, e2, stack, seen)) {
            stack.pop();
        } else {
            break;
        }
    }
    return ok;
};

SimpleTest._eqAssoc = function (o1, o2, stack, seen) {
    // Return if they're the same object.
    if (o1 == o2) return true;

    // JavaScript objects have no unique identifiers, so we have to store
    // references to them all in an array, and then compare the references
    // directly. It's slow, but probably won't be much of an issue in
    // practice. Start by making a local copy of the array to as to avoid
    // confusing a reference seen more than once (such as [a, a]) for a
    // circular reference.
    seen = seen.slice(0);
    for (var j = 0; j < seen.length; j++) {
        if (seen[j][0] == o1) {
            return seen[j][1] == o2;
        }
    }

    // If we get here, we haven't seen o1 before, so store it with reference
    // to o2.
    seen.push([ o1, o2 ]);

    // They should be of the same class.

    var ok = true;
    // Only examines enumerable attributes.
    var o1Size = 0; for (var i in o1) o1Size++;
    var o2Size = 0; for (var i in o2) o2Size++;
    var bigger = o1Size > o2Size ? o1 : o2;
    for (var i in bigger) {
        var e1 = o1[i] == undefined ? SimpleTest.DNE : o1[i];
        var e2 = o2[i] == undefined ? SimpleTest.DNE : o2[i];
        stack.push({ type: 'Object', idx: i, vals: [e1, e2] });
        if (ok = SimpleTest._deepCheck(e1, e2, stack, seen)) {
            stack.pop();
        } else {
            break;
        }
    }
    return ok;
};

SimpleTest._formatStack = function (stack) {
    var variable = '$Foo';
    for (var i = 0; i < stack.length; i++) {
        var entry = stack[i];
        var type = entry['type'];
        var idx = entry['idx'];
        if (idx != null) {
            if (/^\d+$/.test(idx)) {
                // Numeric array index.
                variable += '[' + idx + ']';
            } else {
                // Associative array index.
                idx = idx.replace("'", "\\'");
                variable += "['" + idx + "']";
            }
        }
    }

    var vals = stack[stack.length-1]['vals'].slice(0, 2);
    var vars = [
        variable.replace('$Foo',     'got'),
        variable.replace('$Foo',     'expected')
    ];

    var out = "Structures begin differing at:" + SimpleTest.LF;
    for (var i = 0; i < vals.length; i++) {
        var val = vals[i];
        if (val == null) {
            val = 'undefined';
        } else {
             val == SimpleTest.DNE ? "Does not exist" : "'" + val + "'";
        }
    }

    out += vars[0] + ' = ' + vals[0] + SimpleTest.LF;
    out += vars[1] + ' = ' + vals[1] + SimpleTest.LF;
    
    return '    ' + out;
};


SimpleTest.isDeeply = function (it, as, name) {
    var ok;
    // ^ is the XOR operator.
    if (SimpleTest._isRef(it) ^ SimpleTest._isRef(as)) {
        // One's a reference, one isn't.
        ok = false;
    } else if (!SimpleTest._isRef(it) && !SimpleTest._isRef(as)) {
        // Neither is an object.
        ok = SimpleTest.is(it, as, name);
    } else {
        // We have two objects. Do a deep comparison.
        var stack = [], seen = [];
        if ( SimpleTest._deepCheck(it, as, stack, seen)) {
            ok = SimpleTest.ok(true, name);
        } else {
            ok = SimpleTest.ok(false, name, SimpleTest._formatStack(stack));
        }
    }
    return ok;
};

SimpleTest.typeOf = function (object) {
    var c = Object.prototype.toString.apply(object);
    var name = c.substring(8, c.length - 1);
    if (name != 'Object') return name;
    // It may be a non-core class. Try to extract the class name from
    // the constructor function. This may not work in all implementations.
    if (/function ([^(\s]+)/.test(Function.toString.call(object.constructor))) {
        return RegExp.$1;
    }
    // No idea. :-(
    return name;
};

SimpleTest.isa = function (object, clas) {
    return SimpleTest.typeOf(object) == clas;
};



// Global symbols:
var ok = SimpleTest.ok;
var is = SimpleTest.is;
var isDeeply = SimpleTest.isDeeply;
