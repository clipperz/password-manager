/***

MochiKit.Text 1.5

See <http://mochikit.com/> for documentation, downloads, license, etc.

(c) 2008 Per Cederberg.  All rights Reserved.

***/

MochiKit.Base._module('Text', '1.5', ['Base', 'Format']);

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
}

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
}

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
}

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
}

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
}

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
}

/**
 * Splits a text string, applies a function and joins the results
 * back together again. This is a convenience function for calling
 * split(), map() and join() separately. It can be used to easily
 * trim each line in a text string (using the strip function), or to
 * translate a text word-by-word.
 *
 * @param {Function} func the function to apply
 * @param {String} str the string to split
 * @param {String} [separator] the separator character to use,
 *            defaults to newline
 *
 * @return {String} a string with the joined up results
 */
MochiKit.Text.splitJoin = function (func, str, separator) {
    if (str == null || str.length == 0) {
        return str;
    }
    separator = separator || '\n'
    return MochiKit.Base.map(func, str.split(separator)).join(separator);
}

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
    if (typeof(locale) == "undefined") {
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
    }
}

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
}

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
 */
MochiKit.Text.formatValue = function (spec, value, locale) {
    var self = MochiKit.Text;
    if (typeof(spec) === "string") {
        spec = self._parseFormatFlags(spec, 0, spec.length - 1);
    }
    for (var i = 0; spec.path != null && i < spec.path.length; i++) {
        if (value != null) {
            value = value[spec.path[i]];
        }
    }
    if (typeof(locale) == "undefined") {
        locale = MochiKit.Format.formatLocale();
    } else if (typeof(locale) == "string") {
        locale = MochiKit.Format.formatLocale(locale);
    }
    var str = "";
    if (spec.numeric) {
        if (typeof(value) != "number" || isNaN(value)) {
            str = "";
        } else if (value === Number.POSITIVE_INFINITY) {
            str = "\u221e";
        } else if (value === Number.NEGATIVE_INFINITY) {
            str = "-\u221e";
        } else {
            var sign = (spec.sign === "-") ? "" : spec.sign;
            sign = (value < 0) ? "-" : sign;
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
            str = self._localizeNumber(str, locale, spec.grouping);
            str = sign + str;
        }
        if (str !== "" && spec.format === "%") {
            str = str + locale.percent;
        }
    } else {
        if (spec.format == "r") {
            str = MochiKit.Base.repr(value);
        } else {
            str = (value == null) ? "null" : value.toString();
        }
        str = self.truncate(str, spec.precision);
    }
    if (spec.align == "<") {
        str = self.padRight(str, spec.width);
    } else {
        str = self.padLeft(str, spec.width);
    }
    return str;
}

/**
 * Adjust an already formatted numeric string for locale-specific
 * grouping and decimal separators. The grouping is optional and
 * will attempt to keep the number string length intact by removing
 * padded zeros (if possible).
 *
 * @param {String} num the formatted number string
 * @param {Object} locale the formatting locale to use
 * @param {Boolean} grouping the grouping flag
 *
 * @return {String} the localized number string
 */
MochiKit.Text._localizeNumber = function (num, locale, grouping) {
    var parts = num.split(/\./);
    var whole = parts[0];
    var frac = (parts.length == 1) ? "" : parts[1];
    var res = (frac.length > 0) ? locale.decimal : "";
    while (grouping && frac.length > 3) {
        res = res + frac.substring(0, 3) + locale.separator;
        frac = frac.substring(3);
        if (whole.charAt(0) == "0") {
            whole = whole.substring(1);
        }
    }
    if (frac.length > 0) {
        res += frac;
    }
    while (grouping && whole.length > 3) {
        var pos = whole.length - 3;
        res = locale.separator + whole.substring(pos) + res;
        whole = whole.substring((whole.charAt(0) == "0") ? 1 : 0, pos);
    }
    return whole + res;
}

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
    var start = 0;
    var pos = 0;
    for (pos = 0; pos < pattern.length; pos++) {
        if (pattern.charAt(pos) == "{") {
            if (pos + 1 >= pattern.length) {
                var msg = "unescaped { char, should be escaped as {{";
                throw new self.FormatPatternError(pattern, pos, msg);
            } else if (pattern.charAt(pos + 1) == "{") {
                parts.push(pattern.substring(start, pos + 1));
                start = pos + 2;
                pos++;
            } else {
                if (start < pos) {
                    parts.push(pattern.substring(start, pos));
                }
                start = pattern.indexOf("}", pos) + 1;
                if (start <= 0) {
                    var msg = "unmatched { char, not followed by a } char";
                    throw new self.FormatPatternError(pattern, pos, msg);
                }
                parts.push(self._parseFormat(pattern, pos + 1, start - 1));
                pos = start - 1;
            }
        } else if (pattern.charAt(pos) == "}") {
            if (pos + 1 >= pattern.length || pattern.charAt(pos + 1) != "}") {
                var msg = "unescaped } char, should be escaped as }}";
                throw new self.FormatPatternError(pattern, pos, msg);
            }
            parts.push(pattern.substring(start, pos + 1));
            start = pos + 2;
            pos++;
        }
    }
    if (start < pos) {
        parts.push(pattern.substring(start, pos));
    }
    return parts;
}

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
    var info;
    var pos = text.indexOf(":");
    if (pos == 0) {
        info = self._parseFormatFlags(pattern, startPos + 1, endPos);
        info.path = [0];
    } else if (pos > 0) {
        info = self._parseFormatFlags(pattern, startPos + pos + 1, endPos);
        info.path = text.substring(0, pos).split(".");
    } else {
        info = self._parseFormatFlags(pattern, endPos, endPos);
        info.path = text.split(".");
    }
    var DIGITS = /^\d+$/;
    for (var i = 0; i < info.path.length; i++) {
        var e = info.path[i];
        if (typeof(e) == "string") {
            // TODO: replace with MochiKit.Format.strip?
            e = e.replace(/^\s+/, "").replace(/\s+$/, "");
            if (e == "" && info.path.length == 1) {
                e = 0;
            } else if (e == "") {
                var msg = "format value path contains blanks";
                throw new self.FormatPatternError(pattern, startPos, msg);
            } else if (DIGITS.test(e)) {
                e = parseInt(e);
            }
        }
        info.path[i] = e;
    }
    if (info.path.length < 0 || typeof(info.path[0]) != "number") {
        info.path.unshift(0);
    }
    return info;
}

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
    var self = MochiKit.Text;
    var info = { numeric: false, format: "s", width: 0, precision: -1,
                 align: ">", sign: "-", padding: " ", grouping: false };
    // TODO: replace with MochiKit.Format.rstrip?
    var flags = pattern.substring(startPos, endPos).replace(/\s+$/, "");
    while (flags.length > 0) {
        switch (flags.charAt(0)) {
        case ">":
        case "<":
            info.align = flags.charAt(0);
            flags = flags.substring(1);
            break;
        case "+":
        case "-":
        case " ":
            info.sign = flags.charAt(0);
            flags = flags.substring(1);
            break;
        case ",":
            info.grouping = true;
            flags = flags.substring(1);
            break;
        case ".":
            var chars = /^\d*/.exec(flags.substring(1))[0];
            info.precision = parseInt(chars);
            flags = flags.substring(1 + chars.length);
            break;
        case "0":
            info.padding = flags.charAt(0);
            flags = flags.substring(1);
            break;
        case "1":
        case "2":
        case "3":
        case "4":
        case "5":
        case "6":
        case "7":
        case "8":
        case "9":
            var chars = /^\d*/.exec(flags)[0];
            info.width = parseInt(chars);
            flags = flags.substring(chars.length);
            break;
        case "s":
        case "r":
            info.format = flags.charAt(0);
            flags = flags.substring(1);
            break;
        case "b":
        case "d":
        case "o":
        case "x":
        case "X":
        case "f":
        case "%":
            info.numeric = true;
            info.format = flags.charAt(0);
            info.radix = 10;
            if (info.format === "b") {
                info.radix = 2;
            } else if (info.format === "o") {
                info.radix = 8;
            } else if (info.format === "x" || info.format === "X") {
                info.radix = 16;
            }
            flags = flags.substring(1);
            break;
        default:
            var msg = "unsupported format flag: " + flags.charAt(0);
            throw new self.FormatPatternError(pattern, startPos, msg);
        }
    }
    return info;
}

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
    // TODO: This can be simplified by using the same helper function
    //       as roundToFixed now does.
    var str;
    if (precision >= 0) {
        str = MochiKit.Format.roundToFixed(value, precision + 2);
    } else {
        str = (value == null) ? "0" : value.toString();
    }
    var fracPos = str.indexOf(".");
    if (fracPos < 0) {
        str = str + "00";
    } else if (fracPos + 3 >= str.length) {
        var fraction = str.substring(fracPos + 1);
        while (fraction.length < 2) {
            fraction = fraction + "0";
        }
        str = str.substring(0, fracPos) + fraction;
    } else {
        var fraction = str.substring(fracPos + 1);
        str = str.substring(0, fracPos) + fraction.substring(0, 2) +
              "." + fraction.substring(2);
    }
    while (str.length > 1 && str.charAt(0) == "0" && str.charAt(1) != ".") {
        str = str.substring(1);
    }
    return str;
}

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
}
MochiKit.Text.FormatPatternError.prototype =
    new MochiKit.Base.NamedError("MochiKit.Text.FormatPatternError");


//
//XXX: Internet Explorer exception handling blows
//
if (MochiKit.__export__) {
    formatter = MochiKit.Text.formatter;
    format = MochiKit.Text.format;
    formatValue = MochiKit.Text.formatValue;
}


MochiKit.Base.nameFunctions(MochiKit.Text);
MochiKit.Base._exportSymbols(this, MochiKit.Text);
