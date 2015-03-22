/***

MochiKit.MochiKit 1.5

See <http://mochikit.com/> for documentation, downloads, license, etc.

(c) 2005 Bob Ippolito.  All rights Reserved.

***/

var MochiKit = MochiKit || {};

/** @id MochiKit.MochiKit */
MochiKit.MochiKit = MochiKit.MochiKit || {};

MochiKit.MochiKit.NAME = "MochiKit.MochiKit";
MochiKit.MochiKit.VERSION = "1.5";
MochiKit.MochiKit.__export__ = false;
MochiKit.MochiKit.__repr__ = function () {
    return "[" + this.NAME + " " + this.VERSION + "]";
};

/** @id MochiKit.MochiKit.toString */
MochiKit.MochiKit.toString = function () {
    return this.__repr__();
};

/** @id MochiKit.MochiKit.SUBMODULES */
MochiKit.MochiKit.SUBMODULES = [
    "Base",
    "Iter",
    "Logging",
    "DateTime",
    "Format",
    "Text",
    "Async",
    "DOM",
    "Selector",
    "Style",
    "LoggingPane",
    "Color",
    "Signal",
    "Position",
    "Visual",
    "DragAndDrop",
    "Sortable"
];

(function () {
    if (typeof(document) == "undefined") {
        return;
    }
    var scripts = document.getElementsByTagName("script");
    var kXHTMLNSURI = "http://www.w3.org/1999/xhtml";
    var kSVGNSURI = "http://www.w3.org/2000/svg";
    var kXLINKNSURI = "http://www.w3.org/1999/xlink";
    var kXULNSURI = "http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul";
    var base = null;
    var baseElem = null;
    var allScripts = {};
    var i;
    var src;
    for (i = 0; i < scripts.length; i++) {
        src = null;
        switch (scripts[i].namespaceURI) {
            case kSVGNSURI:
                src = scripts[i].getAttributeNS(kXLINKNSURI, "href");
                break;
            /*
            case null: // HTML
            case '': // HTML
            case kXHTMLNSURI:
            case kXULNSURI:
            */
            default:
                src = scripts[i].getAttribute("src");
                break;
        }
        if (!src) {
            continue;
        }
        allScripts[src] = true;
        if (src.match(/MochiKit.js(\?.*)?$/)) {
            base = src.substring(0, src.lastIndexOf('MochiKit.js'));
            baseElem = scripts[i];
        }
    }
    if (base === null) {
        return;
    }
    var modules = MochiKit.MochiKit.SUBMODULES;
    for (var i = 0; i < modules.length; i++) {
        if (MochiKit[modules[i]]) {
            continue;
        }
        var uri = base + modules[i] + '.js';
        if (uri in allScripts) {
            continue;
        }
        if (baseElem.namespaceURI == kSVGNSURI ||
            baseElem.namespaceURI == kXULNSURI) {
            // SVG, XUL
            /*
                SVG does not support document.write, so if Safari wants to
                support SVG tests it should fix its deferred loading bug
                (see following below).
            */
            var s = document.createElementNS(baseElem.namespaceURI, 'script');
            s.setAttribute("id", "MochiKit_" + base + modules[i]);
            if (baseElem.namespaceURI == kSVGNSURI) {
                s.setAttributeNS(kXLINKNSURI, 'href', uri);
            } else {
                s.setAttribute('src', uri);
            }
            s.setAttribute("type", "application/x-javascript");
            baseElem.parentNode.appendChild(s);
        } else {
            // HTML, XHTML
            /*
                DOM can not be used here because Safari does
                deferred loading of scripts unless they are
                in the document or inserted with document.write

                This is not XHTML compliant.  If you want XHTML
                compliance then you must use the packed version of MochiKit
                or include each script individually (basically unroll
                these document.write calls into your XHTML source)
            */
            document.write('<' + baseElem.nodeName + ' src="' + uri +
                '" type="text/javascript"></script>');
        }
    };
})();
