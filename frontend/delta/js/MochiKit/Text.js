/***

MochiKit.Text 1.5

See <http://mochikit.com/> for documentation, downloads, license, etc.

(c) 2008 Per Cederberg.  All rights Reserved.

***/

MochiKit.Base.module(MochiKit, 'Text', '1.5', ['Base', 'Format']);

/**
 * Checks if a text string starts with the specified substring. If
 * either of the two strings is null, false will be returned.
 *
 * @param {String} substr the substring to search for
 * @param {String} str the string to search in
 *
 * @return {Boolean} true if the string starts with the substring, or
 *         false otherwise
 */
MochiKit.Text.startsWith = function (substr, str) {
    return str != null && substr != null && str.indexOf(substr) == 0;
};

/**
 * Checks if a text string ends with the specified substring. If
 * either of the two strings is null, false will be returned.
 *
 * @param {String} substr the substring to search for
 * @param {String} str the string to search in
 *
 * @return {Boolean} true if the string ends with the substring, or
 *         false otherwise
 */
MochiKit.Text.endsWith = function (substr, str) {
    return str != null && substr != null &&
           str.lastIndexOf(substr) == Math.max(str.length - substr.length, 0);
};

/**
 * Checks if a text string contains the specified substring. If
 * either of the two strings is null, false will be returned.
 *
 * @param {String} substr the substring to search for
 * @param {String} str the string to search in
 *
 * @return {Boolean} true if the string contains the substring, or
 *         false otherwise
 */
MochiKit.Text.contains = function (substr, str) {
    return str != null && substr != null && str.indexOf(substr) >= 0;
};

/**
 * Adds a character to the left-hand side of a string until it
 * reaches the specified minimum length.
 *
 * @param {String} str the string to process
 * @param {Number} minLength the requested minimum length
 * @param {String} fillChar the padding character to add, defaults
 *            to a space
 *
 * @return {String} the padded string
 */
MochiKit.Text.padLeft = function (str, minLength, fillChar) {
    str = str || "";
    fillChar = fillChar || " ";
    while (str.length < minLength) {
        str = fillChar + str;
    }
    return str;
};

/**
 * Adds a character to the right-hand side of a string until it
 * reaches the specified minimum length.
 *
 * @param {String} str the string to process
 * @param {Number} minLength the requested minimum length
 * @param {String} fillChar the padding character to add, defaults
 *            to a space
 *
 * @return {String} the padded string
 */
MochiKit.Text.padRight = function (str, minLength, fillChar) {
    str = str || "";
    fillChar = fillChar || " ";
    while (str.length < minLength) {
        str += fillChar;
    }
    return str;
};

/**
 * Returns a truncated copy of a string. If the string is shorter
 * than the specified maximum length, the object will be returned
 * unmodified. If an optional tail string is specified, additional
 * elements will be removed in order to accomodate the tail (that
 * will be appended). This function also works on arrays.
 *
 * @param {String} str the string to truncate
 * @param {Number} maxLength the maximum length
 * @param {String} [tail] the tail to append on truncation
 *
 * @return {String} the truncated string
 */
MochiKit.Text.truncate = function (str, maxLength, tail) {
    if (str == null || str.length <= maxLength || maxLength < 0) {
        return str;
    } else if (tail != null) {
        str = str.slice(0, Math.max(0, maxLength - tail.length));
        if (typeof(str) == "string") {
            return str + tail;
        } else {
            return MochiKit.Base.extend(str, tail);
        }
    } else {
        return str.slice(0, maxLength);
    }
};

/**
 * Splits a text string using separator as the split point
 * If max is given, at most max splits are done, giving at most
 * max + 1 elements in the returned list.
 *
 * @param {String} str the string to split
 * @param {String/RegExp} [separator] the separator char or regexp to use,
 *            defaults to newline
 * @param {Number} [max] the maximum number of parts to return
 * @return {Array} an array of parts of the string
 */
MochiKit.Text.split = function (str, separator, max) {
    if (str == null) {
        return str;
    }
    separator = separator || '\n';
    var bits = str.split(separator);
    if ((typeof(max) == "undefined") || max >= bits.length - 1) {
        return bits;
    }
    bits.splice(max, bits.length, bits.slice(max, bits.length).join(separator));
    return bits;
};

/**
 * Splits a text string using separator as the split point
 * If max is given, at most max splits are done,
 * using splits from the right
 *
 * @param {String} str the string to split
 * @param {String/RegExp} [separator] the separator char or regexp to use,
 *            defaults to newline
 * @param {Number} [max] the maximum number of parts to return
 * @return {Array} an array of parts of the string
 */
MochiKit.Text.rsplit = function (str, separator, max) {
    if (str == null) {
        return str;
    }
    separator = separator || '\n';
    var bits = str.split(separator);
    if ((typeof(max) == "undefined") || max >= bits.length - 1){
        return bits;
    }
    bits.splice(0, bits.length-max, bits.slice(0, bits.length-max).join(separator));
    return bits;
};

/**
 * Creates a formatter function for the specified formatter pattern
 * and locale. The returned function takes as many arguments as the
 * formatter pattern requires. See separate documentation for
 * information about the formatter pattern syntax.
 *
 * @param {String} pattern the formatter pattern string
 * @param {Object} [locale] the locale to use, defaults to
 *            LOCALE.en_US
 *
 * @return {Function} the formatter function created
 *
 * @throws FormatPatternError if the format pattern was invalid
 */
MochiKit.Text.formatter = function (pattern, locale) {
    if (locale == null) {
        locale = MochiKit.Format.formatLocale();
    } else if (typeof(locale) == "string") {
        locale = MochiKit.Format.formatLocale(locale);
    }
    var parts = MochiKit.Text._parsePattern(pattern);
    return function() {
        var values = MochiKit.Base.extend([], arguments);
        var res = [];
        for (var i = 0; i < parts.length; i++) {
            if (typeof(parts[i]) == "string") {
                res.push(parts[i]);
            } else {
                res.push(MochiKit.Text.formatValue(parts[i], values, locale));
            }
        }
        return res.join("");
    };
};

/**
 * Formats the specified arguments according to a formatter pattern.
 * See separate documentation for information about the formatter
 * pattern syntax.
 *
 * @param {String} pattern the formatter pattern string
 * @param {Object} [...] the optional values to format
 *
 * @return {String} the formatted output string
 *
 * @throws FormatPatternError if the format pattern was invalid
 */
MochiKit.Text.format = function (pattern/*, ...*/) {
    var func = MochiKit.Text.formatter(pattern);
    return func.apply(this, MochiKit.Base.extend([], arguments, 1));
};

/**
 * Format a value with the specified format specifier.
 *
 * @param {String/Object} spec the format specifier string or parsed
 *            format specifier object
 * @param {Object} value the value to format
 * @param {Object} [locale] the locale to use, defaults to
 *            LOCALE.en_US
 *
 * @return {String} the formatted output string
 *
 * @throws FormatPatternError if the format specifier was invalid
 */
MochiKit.Text.formatValue = function (spec, value, locale) {
    var self = MochiKit.Text;
    if (typeof(spec) === "string") {
        spec = self._parseFormatFlags(spec, 0, spec.length);
    }
    for (var i = 0; spec.path != null && i < spec.path.length; i++) {
        if (value != null) {
            value = value[spec.path[i]];
        }
    }
    if (locale == null) {
        locale = MochiKit.Format.formatLocale();
    } else if (typeof(locale) == "string") {
        locale = MochiKit.Format.formatLocale(locale);
    }
    var str = "";
    if (spec.type == "number") {
        if (value instanceof Number) {
            value = value.valueOf();
        }
        if (typeof(value) != "number" || isNaN(value)) {
            str = "";
        } else if (value === Number.POSITIVE_INFINITY) {
            str = "\u221e";
        } else if (value === Number.NEGATIVE_INFINITY) {
            str = "-\u221e";
        } else {
            var sign = (value < 0) ? "-" : spec.sign;
            value = Math.abs(value);
            if (spec.format === "%") {
                str = self._truncToPercent(value, spec.precision);
            } else if (spec.format === "d") {
                str = MochiKit.Format.roundToFixed(value, 0);
            } else if (spec.radix != 10) {
                str = Math.floor(value).toString(spec.radix);
                if (spec.format === "x") {
                    str = str.toLowerCase();
                } else if (spec.format === "X") {
                    str = str.toUpperCase();
                }
            } else if (spec.precision >= 0) {
                str = MochiKit.Format.roundToFixed(value, spec.precision);
            } else {
                str = value.toString();
            }
            if (spec.padding === "0" && spec.format === "%") {
                str = self.padLeft(str, spec.width - sign.length - 1, "0");
            } else if (spec.padding == "0") {
                str = self.padLeft(str, spec.width - sign.length, "0");
            }
            str = self._localizeNumber(str, locale, spec.group);
            str = sign + str;
        }
        if (str !== "" && spec.format === "%") {
            str = str + locale.percent;
        }
    } else {
        if (spec.format == "r") {
            str = MochiKit.Base.repr(value);
        } else {
            str = (value == null) ? "" : value.toString();
        }
        str = self.truncate(str, spec.precision);
    }
    if (spec.align == "<") {
        str = self.padRight(str, spec.width);
    } else {
        str = self.padLeft(str, spec.width);
    }
    return str;
};

/**
 * Adjust an already formatted numeric string for locale-specific
 * grouping and decimal separators. The grouping is optional and
 * will attempt to keep the number string length intact by removing
 * padded zeros (if possible).
 *
 * @param {String} num the formatted number string
 * @param {Object} locale the formatting locale to use
 * @param {Boolean} group the grouping flag
 *
 * @return {String} the localized number string
 */
MochiKit.Text._localizeNumber = function (num, locale, group) {
    var parts = num.split(/\./);
    var whole = parts[0];
    var frac = (parts.length == 1) ? "" : parts[1];
    var res = (frac.length > 0) ? locale.decimal : "";
    while (group && frac.length > 3) {
        res = res + frac.substring(0, 3) + locale.separator;
        frac = frac.substring(3);
        if (whole.charAt(0) == "0") {
            whole = whole.substring(1);
        }
    }
    if (frac.length > 0) {
        res = res + frac;
    }
    while (group && whole.length > 3) {
        var pos = whole.length - 3;
        res = locale.separator + whole.substring(pos) + res;
        whole = whole.substring((whole.charAt(0) == "0") ? 1 : 0, pos);
    }
    return whole + res;
};

/**
 * Parses a format pattern and returns an array of constant strings
 * and format info objects.
 *
 * @param {String} pattern the format pattern to analyze
 *
 * @return {Array} an array of strings and format info objects
 *
 * @throws FormatPatternError if the format pattern was invalid
 */
MochiKit.Text._parsePattern = function (pattern) {
    var self = MochiKit.Text;
    var parts = [];
    var re = /{[^{}]*}|{{?|}}?/g;
    var lastPos = re.lastIndex = 0;
    var m;
    while ((m = re.exec(pattern)) != null) {
        if (lastPos < m.index) {
            parts.push(pattern.substring(lastPos, m.index))
        }
        var str = m[0];
        lastPos = m.index + str.length;
        if (self.startsWith("{", str) && self.endsWith("}", str)) {
            parts.push(self._parseFormat(pattern, m.index + 1, lastPos - 1));
        } else if (self.startsWith("{{", str) || self.startsWith("}}", str)) {
            parts.push(str.substring(1));
        } else if (self.startsWith("{", str)) {
            var msg = "unescaped { char, should be escaped as {{";
            throw new self.FormatPatternError(pattern, m.index, msg);
        } else if (self.startsWith("}", str)) {
            var msg = "unescaped } char, should be escaped as }}";
            throw new self.FormatPatternError(pattern, m.index, msg);
        }
    }
    if (lastPos < pattern.length) {
        parts.push(pattern.substring(lastPos));
    }
    return parts;
};

/**
 * Parses a format instruction and returns a format info object.
 *
 * @param {String} pattern the format pattern string
 * @param {Number} startPos the first index of the format instruction
 * @param {Number} endPos the last index of the format instruction
 *
 * @return {Object} the format info object
 *
 * @throws FormatPatternError if the format pattern was invalid
 */
MochiKit.Text._parseFormat = function (pattern, startPos, endPos) {
    var self = MochiKit.Text;
    var text = pattern.substring(startPos, endPos);
    var parts = self.split(text, ":", 1);
    var path = parts[0];
    var flagsPos = startPos + path.length + ((parts.length == 1) ? 0 : 1);
    var info = self._parseFormatFlags(pattern, flagsPos, endPos);
    info.path = (path == "") ? [] : path.split(".");
    for (var i = 0; i < info.path.length; i++) {
        var v = info.path[i];
        // TODO: replace with MochiKit.Format.strip?
        v = v.replace(/^\s+/, "").replace(/\s+$/, "");
        if (v == "" && info.path.length == 1) {
            v = 0;
        } else if (v == "") {
            var msg = "format value path contains blanks";
            throw new self.FormatPatternError(pattern, startPos, msg);
        } else if (/^\d+$/.test(v)) {
            v = parseInt(v, 10);
        }
        info.path[i] = v;
    }
    if (info.path.length <= 0 || typeof(info.path[0]) != "number") {
        info.path.unshift(0);
    }
    return info;
};

/**
 * Parses a string with format flags and returns a format info object.
 *
 * @param {String} pattern the format pattern string
 * @param {Number} startPos the first index of the format instruction
 * @param {Number} endPos the last index of the format instruction
 *
 * @return {Object} the format info object
 *
 * @throws FormatPatternError if the format pattern was invalid
 */
MochiKit.Text._parseFormatFlags = function (pattern, startPos, endPos) {
    var update = MochiKit.Base.update;
    var info = { type: "string", format: "s", width: 0, precision: -1,
                 align: ">", sign: "", padding: " ", group: false };
    // TODO: replace with MochiKit.Format.rstrip?
    var text = pattern.substring(startPos, endPos).replace(/\s+$/, "");
    var m = /^([<>+ 0,-]+)?(\d+)?(\.\d*)?([srbdoxXf%])?(.*)$/.exec(text);
    var flags = m[1];
    var width = m[2];
    var precision = m[3];
    var type = m[4];
    var unmatched = m[5];
    for (var i = 0; flags && i < flags.length; i++) {
        var chr = flags.charAt(i);
        if (chr == "<" || chr == ">") {
            info.align = chr;
        } else if (chr == "+" || chr == "-" || chr == " ") {
            info.sign = (chr == "-") ? "" : chr;
        } else if (chr == "0") {
            info.padding = chr;
        } else if (chr == ",") {
            info.group = true;
        }
    }
    if (width) {
        info.width = parseInt(width, 10);
    }
    if (precision && precision.length > 1) {
        info.precision = parseInt(precision.substring(1), 10);
    }
    if (type == "s" || type == "r") {
        info.format = type;
    } else if (type == "b") {
        update(info, { type: "number", format: type, radix: 2 });
    } else if (type == "o") {
        update(info, { type: "number", format: type, radix: 8 });
    } else if (type == "x" || type == "X") {
        update(info, { type: "number", format: type, radix: 16 });
    } else if (type == "d" || type == "f" || type == "%") {
        update(info, { type: "number", format: type, radix: 10 });
    }
    if (unmatched) {
        var msg = "unsupported format flag: " + unmatched.charAt(0);
        throw new MochiKit.Text.FormatPatternError(pattern, startPos, msg);
    }
    return info;
};

/**
 * Formats a value as a percentage. This method avoids multiplication
 * by 100 since it leads to weird numeric rounding errors. Instead it
 * just move the decimal separator in the text string. It is ugly,
 * but works...
 *
 * @param {Number} value the value to format
 * @param {Number} precision the number of precision digits
 */
MochiKit.Text._truncToPercent = function (value, precision) {
    // TODO: This can be simplified by using MochiKit.Format._shiftNumber
    //       as roundToFixed does.
    var str;
    if (precision >= 0) {
        str = MochiKit.Format.roundToFixed(value, precision + 2);
    } else {
        str = (value == null) ? "0" : value.toString();
    }
    var arr = MochiKit.Text.split(str, ".", 2);
    var frac = MochiKit.Text.padRight(arr[1], 2, "0");
    var whole = arr[0] + frac.substring(0, 2);
    frac = frac.substring(2);
    while (/^0[0-9]/.test(whole)) {
        whole = whole.substring(1);
    }
    return (frac.length <= 0) ? whole : whole + "." + frac;
};

/**
 * Creates a new format pattern error.
 *
 * @param {String} pattern the format pattern string
 * @param {Number} pos the position of the error
 * @param {String} message the error message text
 *
 * @return {Error} the format pattern error
 *
 * @class The format pattern error class. This error is thrown when
 *     a syntax error is encountered inside a format string.
 * @property {String} pattern The format pattern string.
 * @property {Number} pos The position of the error.
 * @property {String} message The error message text.
 * @extends MochiKit.Base.NamedError
 */
MochiKit.Text.FormatPatternError = function (pattern, pos, message) {
    this.pattern = pattern;
    this.pos = pos;
    this.message = message;
};

MochiKit.Text.FormatPatternError.prototype = new MochiKit.Base.NamedError("MochiKit.Text.FormatPatternError");
MochiKit.Text.FormatPatternError.constructor = MochiKit.Text.FormatPatternError;

//
//XXX: Internet Explorer export fix
//
if (MochiKit.__export__) {
    formatter = MochiKit.Text.formatter;
    format = MochiKit.Text.format;
    formatValue = MochiKit.Text.formatValue;
}


MochiKit.Base.nameFunctions(MochiKit.Text);
MochiKit.Base._exportSymbols(this, MochiKit.Text);
