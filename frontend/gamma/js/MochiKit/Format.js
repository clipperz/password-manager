/***

MochiKit.Format 1.5

See <http://mochikit.com/> for documentation, downloads, license, etc.

(c) 2005 Bob Ippolito.  All rights Reserved.

***/

MochiKit.Base.module(MochiKit, 'Format', '1.5', ['Base']);

MochiKit.Format._numberFormatter = function (placeholder, header, footer, locale, isPercent, precision, leadingZeros, separatorAt, trailingZeros) {
    return function (num) {
        num = parseFloat(num);
        if (typeof(num) == "undefined" || num === null || isNaN(num)) {
            return placeholder;
        }
        var curheader = header;
        var curfooter = footer;
        if (num < 0) {
            num = -num;
        } else {
            curheader = curheader.replace(/-/, "");
        }
        var me = arguments.callee;
        var fmt = MochiKit.Format.formatLocale(locale);
        if (isPercent) {
            num = num * 100.0;
            curfooter = fmt.percent + curfooter;
        }
        num = MochiKit.Format.roundToFixed(num, precision);
        var parts = num.split(/\./);
        var whole = parts[0];
        var frac = (parts.length == 1) ? "" : parts[1];
        var res = "";
        while (whole.length < leadingZeros) {
            whole = "0" + whole;
        }
        if (separatorAt) {
            while (whole.length > separatorAt) {
                var i = whole.length - separatorAt;
                //res = res + fmt.separator + whole.substring(i, whole.length);
                res = fmt.separator + whole.substring(i, whole.length) + res;
                whole = whole.substring(0, i);
            }
        }
        res = whole + res;
        if (precision > 0) {
            while (frac.length < trailingZeros) {
                frac = frac + "0";
            }
            res = res + fmt.decimal + frac;
        }
        return curheader + res + curfooter;
    };
};

/** @id MochiKit.Format.numberFormatter */
MochiKit.Format.numberFormatter = function (pattern, placeholder/* = "" */, locale/* = "default" */) {
    // http://java.sun.com/docs/books/tutorial/i18n/format/numberpattern.html
    // | 0 | leading or trailing zeros
    // | # | just the number
    // | , | separator
    // | . | decimal separator
    // | % | Multiply by 100 and format as percent
    if (typeof(placeholder) == "undefined") {
        placeholder = "";
    }
    var match = pattern.match(/((?:[0#]+,)?[0#]+)(?:\.([0#]+))?(%)?/);
    if (!match) {
        throw TypeError("Invalid pattern");
    }
    var header = pattern.substr(0, match.index);
    var footer = pattern.substr(match.index + match[0].length);
    if (header.search(/-/) == -1) {
        header = header + "-";
    }
    var whole = match[1];
    var frac = (typeof(match[2]) == "string" && match[2] != "") ? match[2] : "";
    var isPercent = (typeof(match[3]) == "string" && match[3] != "");
    var tmp = whole.split(/,/);
    var separatorAt;
    if (typeof(locale) == "undefined") {
        locale = "default";
    }
    if (tmp.length == 1) {
        separatorAt = null;
    } else {
        separatorAt = tmp[1].length;
    }
    var leadingZeros = whole.length - whole.replace(/0/g, "").length;
    var trailingZeros = frac.length - frac.replace(/0/g, "").length;
    var precision = frac.length;
    var rval = MochiKit.Format._numberFormatter(
        placeholder, header, footer, locale, isPercent, precision,
        leadingZeros, separatorAt, trailingZeros
    );
    var m = MochiKit.Base;
    if (m) {
        var fn = arguments.callee;
        var args = m.concat(arguments);
        rval.repr = function () {
            return [
                self.NAME,
                "(",
                m.map(m.repr, args).join(", "),
                ")"
            ].join("");
        };
    }
    return rval;
};

/** @id MochiKit.Format.formatLocale */
MochiKit.Format.formatLocale = function (locale) {
    if (typeof(locale) == "undefined" || locale === null) {
        locale = "default";
    }
    if (typeof(locale) == "string") {
        var rval = MochiKit.Format.LOCALE[locale];
        if (typeof(rval) == "string") {
            rval = arguments.callee(rval);
            MochiKit.Format.LOCALE[locale] = rval;
        }
        return rval;
    } else {
        return locale;
    }
};

/** @id MochiKit.Format.twoDigitAverage */
MochiKit.Format.twoDigitAverage = function (numerator, denominator) {
    if (denominator) {
        var res = numerator / denominator;
        if (!isNaN(res)) {
            return MochiKit.Format.twoDigitFloat(res);
        }
    }
    return "0";
};

/** @id MochiKit.Format.twoDigitFloat */
MochiKit.Format.twoDigitFloat = function (aNumber) {
    var res = MochiKit.Format.roundToFixed(aNumber, 2);
    if (res.indexOf(".00") > 0) {
        return res.substring(0, res.length - 3);
    } else if (res.charAt(res.length - 1) == "0") {
        return res.substring(0, res.length - 1);
    } else {
        return res;
    }
};

/** @id MochiKit.Format.lstrip */
MochiKit.Format.lstrip = function (str, /* optional */chars) {
    str = str + "";
    if (typeof(str) != "string") {
        return null;
    }
    if (!chars) {
        return str.replace(/^\s+/, "");
    } else {
        return str.replace(new RegExp("^[" + chars + "]+"), "");
    }
};

/** @id MochiKit.Format.rstrip */
MochiKit.Format.rstrip = function (str, /* optional */chars) {
    str = str + "";
    if (typeof(str) != "string") {
        return null;
    }
    if (!chars) {
        return str.replace(/\s+$/, "");
    } else {
        return str.replace(new RegExp("[" + chars + "]+$"), "");
    }
};

/** @id MochiKit.Format.strip */
MochiKit.Format.strip = function (str, /* optional */chars) {
    var self = MochiKit.Format;
    return self.rstrip(self.lstrip(str, chars), chars);
};

/** @id MochiKit.Format.truncToFixed */
MochiKit.Format.truncToFixed = function (aNumber, precision) {
    var fixed = MochiKit.Format._numberToFixed(aNumber, precision);
    var fracPos = fixed.indexOf(".");
    if (fracPos > 0 && fracPos + precision + 1 < fixed.length) {
        fixed = fixed.substring(0, fracPos + precision + 1);
        fixed = MochiKit.Format._shiftNumber(fixed, 0);
    }
    return fixed;
};

/** @id MochiKit.Format.roundToFixed */
MochiKit.Format.roundToFixed = function (aNumber, precision) {
    var fixed = MochiKit.Format._numberToFixed(aNumber, precision);
    var fracPos = fixed.indexOf(".");
    if (fracPos > 0 && fracPos + precision + 1 < fixed.length) {
        var str = MochiKit.Format._shiftNumber(fixed, precision);
        str = MochiKit.Format._numberToFixed(Math.round(parseFloat(str)), 0);
        fixed = MochiKit.Format._shiftNumber(str, -precision);
    }
    return fixed;
};

/**
 * Converts a number to a fixed format string. This function handles
 * conversion of exponents by shifting the decimal point to the left
 * or the right. It also guarantees a specified minimum number of
 * fractional digits (but no maximum).
 *
 * @param {Number} aNumber the number to convert
 * @param {Number} precision the minimum number of decimal digits
 *
 * @return {String} the fixed format number string
 */
MochiKit.Format._numberToFixed = function (aNumber, precision) {
    var str = aNumber.toString();
    var parts = str.split(/[eE]/);
    var exp = (parts.length === 1) ? 0 : parseInt(parts[1], 10) || 0;
    var fixed = MochiKit.Format._shiftNumber(parts[0], exp);
    parts = fixed.split(/\./);
    var whole = parts[0];
    var frac = (parts.length === 1) ? "" : parts[1];
    while (frac.length < precision) {
        frac += "0";
    }
    if (frac.length > 0) {
        return whole + "." + frac;
    } else {
        return whole;
    }
};

/**
 * Shifts the decimal dot location in a fixed format number string.
 * This function handles negative values and will add and remove
 * leading and trailing zeros as needed.
 *
 * @param {String} num the fixed format number string
 * @param {Number} exp the base-10 exponent to apply
 *
 * @return {String} the new fixed format number string
 */
MochiKit.Format._shiftNumber = function (num, exp) {
    var pos = num.indexOf(".");
    if (pos < 0) {
        pos = num.length;
    } else {
        num = num.substring(0, pos) + num.substring(pos + 1);
    }
    pos += exp;
    while (pos <= 0 || (pos <= 1 && num.charAt(0) === "-")) {
        if (num.charAt(0) === "-") {
            num = "-0" + num.substring(1);
        } else {
            num = "0" + num;
        }
        pos++;
    }
    while (pos > num.length) {
        num += "0";
    }
    if (pos < num.length) {
        num = num.substring(0, pos) + "." + num.substring(pos);
    }
    while (/^0[^.]/.test(num)) {
        num = num.substring(1);
    }
    while (/^-0[^.]/.test(num)) {
        num = "-" + num.substring(2);
    }
    return num;
};

/** @id MochiKit.Format.percentFormat */
MochiKit.Format.percentFormat = function (aNumber) {
    return MochiKit.Format.twoDigitFloat(100 * aNumber) + '%';
};

MochiKit.Format.LOCALE = {
    en_US: {separator: ",", decimal: ".", percent: "%"},
    de_DE: {separator: ".", decimal: ",", percent: "%"},
    pt_BR: {separator: ".", decimal: ",", percent: "%"},
    fr_FR: {separator: " ", decimal: ",", percent: "%"},
    "default": "en_US",
    __export__: false
};

MochiKit.Format.__new__ = function () {
    MochiKit.Base.nameFunctions(this);
    var base = this.NAME + ".";
    var k, v, o;
    for (k in this.LOCALE) {
        o = this.LOCALE[k];
        if (typeof(o) == "object") {
            o.repr = function () { return this.NAME; };
            o.NAME = base + "LOCALE." + k;
        }
    }
};

MochiKit.Format.__new__();

MochiKit.Base._exportSymbols(this, MochiKit.Format);
