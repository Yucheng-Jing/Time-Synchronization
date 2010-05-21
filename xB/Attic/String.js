// Create a local scope.
(function() {


var StringPrototypeCharAtOriginal = String.prototype.charAt;

String.prototype.charAt = function(pos) {
    var position = Object.toInteger(pos);
    
    if (isNaN(position) || (position >= this.length) || (position < 0)) {
        var msg = 'String.prototype.charAt called with invalid position';
        throw new RangeError(msg + ': ' + position);
    }
    
    return StringPrototypeCharAtOriginal.call(this, position);
};


var StringPrototypeCharCodeAtOriginal = String.prototype.charCodeAt;

String.prototype.charCodeAt = function(pos) {
    var position = Object.toInteger(pos);
    
    if (isNaN(position) || (position >= this.length) || (position < 0)) {
        var msg = 'String.prototype.charCodeAt called with invalid position';
        throw new RangeError(msg + ': ' + position);
    }
    
    return StringPrototypeCharCodeAtOriginal.call(this, position);
};


var StringPrototypeIndexOfOriginal = String.prototype.indexOf;

String.prototype.indexOf = function(string, pos) {
    var position = Object.toInteger(pos);
    
    if (isNaN(position) || (position >= this.length) || (position < 0)) {
        var msg = 'String.prototype.indexOf called with invalid position';
        throw new RangeError(msg + ': ' + position);
    }
    
    var result = StringPrototypeIndexOfOriginal.call(this, string, position);
    return result < 0 ? NaN : result;
};


var StringPrototypeLastIndexOfOriginal = String.prototype.lastIndexOf;

String.prototype.lastIndexOf = function(str, pos) {
    var position = Object.toNumber(pos);
    position = isNan(position) ? +Infinity : Object.toInteger(position);
    
    if (position < 0) {
        var msg = 'String.prototype.lastIndexOf called with invalid position';
        throw new RangeError(msg + ': ' + position);
    }
    
    var result = StringPrototypeLastIndexOfOriginal.call(this, str, position);
    return result < 0 ? NaN : result;
};


/**
 * Removes whitespace from the left side of a string.
 *
 * @return a copy of the string without whitespace on it's left side
 * @type String
 */
String.prototype.leftTrim = function() {
    return this.replace(/^\s+/, '');
};


/**
 * Removes whitespace from the right side of a string.
 *
 * @return a copy of the string without whitespace on it's right side
 * @type String
 */
String.prototype.rightTrim = function() {
    return this.replace(/\s+$/, '');
};


/**
 * Removes white space characters from both sides of a string.
 *
 * @tparam String string string to remove white space from
 */
String.trim = function(string) {
    //return this.leftTrim().rightTrim();
    return string.replace(/(^\s+)|(\s+$)/g, '');
};


/**
 * Analogous to Perl's string repetition operator. Repeats a string n times.
 *
 * @param {Number} count how many times to repeat the string
 * @return the original string repeated "count" times
 * @type String
 */
String.prototype.x = function(count) {
    if (count <= 0) {
        return this.toString();
    }
    
    var string = '';
    while (count-- > 0) {
        string += this;
    }
    
    return string;
};


/**
 * Decodes an UTF-8 encoded string to Unicode.
 *
 * @param {String} string UTF-8 encoded string to decode
 * @return copy of the given string decoded to Unicode
 * @type String
 * @see http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
 */
this.decodeUTF8 = function(string) {
    var decoded = '';
    
    for (var i = 0; i < string.length; ++i) {
        var charCode = string.charCodeAt(i);
        var c;
        
        // Skip UTF-16 surrogates and other special character codes.
        if (((charCode >= 0xD800) && (charCode <= 0xDFFF))
                || (charCode == 0xFFFE) || (charCode == 0xFFFF)) {
            continue;
        }
        
        if (charCode < 0x80) {          // 1 byte sequence.
            c = String.fromCharCode(charCode & 0x7F);
        }
        else if (charCode <= 0xE0) {    // 2 bytes sequence.
            c = String.fromCharCode(((charCode & 0x1F) << 6)
                + (string.charCodeAt(i + 1) & 0x3F));
            ++i;
        }
        else if (charCode <= 0xF0) {    // 3 bytes sequence.
            c = String.fromCharCode(((charCode & 0xF) << 12)
                + ((string.charCodeAt(i + 1) & 0x3F) << 6)
                + (string.charCodeAt(i + 2) & 0x3F));
            i += 2;
        }
        else if (charCode <= 0xF8) {    // 4 bytes sequence.
            c = String.fromCharCode(((charCode & 0x7) << 18)
                + ((string.charCodeAt(i + 1) & 0x3F) << 12)
                + ((string.charCodeAt(i + 2) & 0x3F) << 6)
                + (string.charCodeAt(i + 3) & 0x3F));
            i += 3;
        }
        else if (charCode <= 0xFC) {    // 5 bytes sequence.
            c = String.fromCharCode(((charCode & 0x3) << 24)
                + ((string.charCodeAt(i + 1) & 0x3F) << 18)
                + ((string.charCodeAt(i + 2) & 0x3F) << 12)
                + ((string.charCodeAt(i + 3) & 0x3F) << 6)
                + (string.charCodeAt(i + 4) & 0x3F));
            i += 4;
        }
        else if (charCode <= 0xFE) {    // 6 bytes sequence.
            c = String.fromCharCode(((charCode & 0x1) << 30)
                + ((string.charCodeAt(i + 1) & 0x3F) << 24)
                + ((string.charCodeAt(i + 2) & 0x3F) << 18)
                + ((string.charCodeAt(i + 3) & 0x3F) << 12)
                + ((string.charCodeAt(i + 4) & 0x3F) << 6)
                + (string.charCodeAt(i + 5) & 0x3F));
            i += 5;
        }
        else {                          // Unknown sequence.
            continue;
        }
        
        decoded += c;
    }
    
    return decoded;
};


/**
 * Encodes an Unicode string in UTF-8.
 *
 * @param {String} string Unicode string to encode
 * @return copy of the given string encoded in UTF-8
 * @type String
 * @see http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
 */
this.encodeUTF8 = function(string) {
    var encoded = '';
    
    for (var i = 0; i < string.length; ++i) {
        var charCode = string.charCodeAt(i);
        var seq;
        
        if (charCode <= 0x7F) {         // 1 byte sequence.
            seq = String.fromCharCode(charCode & 0x7F);
        }
        else if (charCode <= 0x7FF) {    // 2 bytes sequence.
            seq = String.fromCharCode(((charCode >>> 6) & 0x1F) | 0xC0,
                (charCode & 0x3F) | 0x80);
        }
        else if (charCode <= 0xFFFF) {    // 3 bytes sequence.
            seq = String.fromCharCode(((charCode >>> 12) & 0xF) | 0xE0,
                ((charCode >>> 6) & 0x3F) | 0x80,
                (charCode & 0x3F) | 0x80);
        }
        else if (charCode <= 0x1FFFFF) {    // 4 bytes sequence.
            seq = String.fromCharCode(((charCode >>> 18) & 0x7) | 0xF0,
                ((charCode >>> 12) & 0x3F) | 0x80,
                ((charCode >>> 6) & 0x3F) | 0x80,
                (charCode & 0x3F) | 0x80);
        }
        else if (charCode <= 0x3FFFFFF) {    // 5 bytes sequence.
            seq = String.fromCharCode(((charCode >>> 24) & 0x3) | 0xF8,
                ((charCode >>> 18) & 0x3F) | 0x80,
                ((charCode >>> 12) & 0x3F) | 0x80,
                ((charCode >>> 6) & 0x3F) | 0x80,
                (charCode & 0x3F) | 0x80);
        }
        else if (charCode <= 0x7FFFFFFF) {    // 6 bytes sequence.
            seq = String.fromCharCode(((charCode >>> 30) & 0x1) | 0xFC,
                ((charCode >>> 24) & 0x3F) | 0x80,
                ((charCode >>> 18) & 0x3F) | 0x80,
                ((charCode >>> 12) & 0x3F) | 0x80,
                ((charCode >>> 6) & 0x3F) | 0x80,
                (charCode & 0x3F) | 0x80);
        }
        else {                                // Unknown character code.
            continue;
        }
        
        encoded += seq;
    }
    
    return encoded;
};


// End of local scope.
})();
