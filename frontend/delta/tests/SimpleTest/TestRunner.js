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

/**
 * TestRunner: A test runner for SimpleTest
 * TODO:
 * 
 *  * Avoid moving iframes: That causes reloads on mozilla and opera.
 *
 *
**/
var TestRunner = {};
TestRunner.logEnabled = false;
TestRunner._iframes = {};
TestRunner._iframeDocuments = {};
TestRunner._iframeRows = {};
TestRunner._currentTest = 0;
TestRunner._urls = [];
TestRunner._testsDiv = DIV();
TestRunner._progressDiv = DIV();
TestRunner._summaryDiv = DIV(null, 
    H1(null, "Tests Summary"),
    TABLE(null, 
        THEAD(null, 
            TR(null,
                TH(null, "Test"), 
                TH(null, "Passed"), 
                TH(null, "Failed")
            )
        ),
		TFOOT(/*null, TR(null, TH(null, "-"), TH(null, "--"), TH(null, "---"))*/),
        TBODY()
    )
);

/**
 * This function is called after generating the summary.
**/
TestRunner.onComplete = null;

/**
 * If logEnabled is true, this is the logger that will be used.
**/
TestRunner.logger = MochiKit.Logging.logger;

/**
 * Toggle element visibility
**/
TestRunner._toggle = function(el) {
    if (el.className == "noshow") {
        el.className = "";
        el.style.cssText = "";
    } else {
        el.className = "noshow";
        el.style.cssText = "width:0px; height:0px; border:0px;";
    }
};


/**
 * Creates the iframe that contains a test
**/
TestRunner._makeIframe = function (url) {
    var iframe = document.createElement('iframe');
    iframe.src = url;
    iframe.name = url;
    iframe.width = "500";
    var tbody = TestRunner._summaryDiv.getElementsByTagName("tbody")[0];
    var tr = TR(null, TD({'colspan': '3'}, iframe));
    iframe._row = tr;
    tbody.appendChild(tr);
    return iframe;
};

/**
 * TestRunner entry point.
 *
 * The arguments are the URLs of the test to be ran.
 *
**/
TestRunner.runTests = function (/*url...*/) {
    if (TestRunner.logEnabled)
        TestRunner.logger.log("SimpleTest START");
  
    var body = document.getElementsByTagName("body")[0];
    appendChildNodes(body,
        TestRunner._testsDiv,
        TestRunner._progressDiv,
        TestRunner._summaryDiv
    );
    for (var i = 0; i < arguments.length; i++) {
        TestRunner._urls.push(arguments[i]); 
    }

    TestRunner.runNextTest();
};

/**
 * Run the next test. If no test remains, calls makeSummary
**/
TestRunner.runNextTest = function() {
    if (TestRunner._currentTest < TestRunner._urls.length) {
        var url = TestRunner._urls[TestRunner._currentTest];
        var progress = SPAN(null,
            "Running ", A({href:url}, url), "..."
        );
        
        if (TestRunner.logEnabled)
            TestRunner.logger.log(scrapeText(progress));
        
        TestRunner._progressDiv.appendChild(progress);
        TestRunner._iframes[url] = TestRunner._makeIframe(url);
    }  else {
        TestRunner.makeSummary();
        if (TestRunner.onComplete) {
            TestRunner.onComplete();
		}
    }
};

/**
 * This stub is called by SimpleTest when a test is finished.
**/
TestRunner.testFinished = function (doc) {
    appendChildNodes(TestRunner._progressDiv, SPAN(null, "Done"), BR());
    var finishedURL = TestRunner._urls[TestRunner._currentTest];
    
    if (TestRunner.logEnabled)
        TestRunner.logger.debug("SimpleTest finished " + finishedURL);
    
    TestRunner._iframeDocuments[finishedURL] = doc;
    // TestRunner._iframes[finishedURL].style.display = "none";
	if (finishedURL != null) {
    	TestRunner._toggle(TestRunner._iframes[finishedURL]);
    	TestRunner._currentTest++;
    	TestRunner.runNextTest();
	}
};

/**
 * Display the summary in the browser
**/
/*
TestRunner.makeSummary = function() {
    if (TestRunner.logEnabled)
        TestRunner.logger.log("SimpleTest FINISHED");
    var rows = [];
    for (var url in TestRunner._iframeDocuments) {
        var doc = TestRunner._iframeDocuments[url];
        var nOK = withDocument(doc,
            partial(getElementsByTagAndClassName, 'div', 'test_ok')
        ).length;
        var nNotOK = withDocument(doc,
            partial(getElementsByTagAndClassName, 'div', 'test_not_ok')
        ).length;
        var toggle = partial(TestRunner._toggle, TestRunner._iframes[url]);
        var jsurl = "TestRunner._toggle(TestRunner._iframes['" + url + "'])";
        var row = TR(
            {'style': {'backgroundColor': nNotOK > 0 ? "#f00":"#0f0"}}, 
            TD(null, url),
            TD(null, nOK),
            TD(null, nNotOK)
        );
        row.onclick = toggle;
        var tbody = TestRunner._summaryDiv.getElementsByTagName("tbody")[0];
        tbody.insertBefore(row, TestRunner._iframes[url]._row)
    }
};
*/

TestRunner.makeSummary = function() {
    var base = MochiKit.Base;
    var dom = MochiKit.DOM;
    var iter = MochiKit.Iter;
	var	total_Ok, total_not_Ok;
	
	total_Ok = 0;
	total_not_Ok = 0;
	
    if (TestRunner.logEnabled)
        TestRunner.logger.log("SimpleTest FINISHED");
    var rows = [];
    for (var url in TestRunner._iframeDocuments) {
        var doc = TestRunner._iframeDocuments[url];
        var nOK = withDocument(doc,
            partial(getElementsByTagAndClassName, 'div', 'test_ok')
        ).length;
        var nNotOK = withDocument(doc,
            partial(getElementsByTagAndClassName, 'div', 'test_not_ok')
        ).length;
        var toggle = partial(TestRunner._toggle, TestRunner._iframes[url]);
        var jsurl = "TestRunner._toggle(TestRunner._iframes['" + url + "'])";
        var row = TR(
            {'style': {'backgroundColor': nNotOK > 0 ? "#f00":"#0f0"}}, 
            TD(null, url),
            TD(null, nOK),
            TD(null, nNotOK)
        );
        row.onclick = toggle;
        var tbody = TestRunner._summaryDiv.getElementsByTagName("tbody")[0];
        tbody.insertBefore(row, TestRunner._iframes[url]._row);

		total_Ok += nOK;
		total_not_Ok += nNotOK;
    }

	{
        var tfoot = TestRunner._summaryDiv.getElementsByTagName("tfoot")[0];
        tfoot.appendChild(TR(null,
                TH(null, ""), 
                TH({'style':"color:green;"}, total_Ok), 
                TH({'style':"color:" + ((total_not_Ok == 0) ? 'black' : 'red') + ";"}, total_not_Ok)
		));
	}

	var testRunnerResults;
	var i, c;
	
	testRunnerResults = dom.DIV({'style': 'display:none; visibility:hidden;'}, null);

	c = total_Ok;
	for (i=0; i<c; i++) {
		dom.appendChildNodes(testRunnerResults, dom.DIV({'class': 'test_ok'}, "ok"));
	}
	
	c = total_not_Ok;
	for (i=0; i<c; i++) {
		dom.appendChildNodes(testRunnerResults, dom.DIV({'class': 'test_not_ok'}, "fail"));
	}

	document.getElementsByTagName("body")[0].appendChild(testRunnerResults);

    if (typeof(parent) != "undefined" && parent.TestRunner) {
        parent.TestRunner.testFinished(document);
    }
};
