// Begin a local scope.
(function() {


// TODO: Validate argument "name" to check if it's well formed.
module = function(name, properties) {
    var names = name.split(/[.]/);
    var module = Object.global[names.shift()] = {};
    
    for (var i = 0; i < names.length; ++i) {
        module[names[i]] = {};
        module = module[names[i]];
    }
    for (var name in properties) {
        module[name] = properties[name];
    }
};


/**
 * Checks if all values are defined.
 *
 * @return false if any argument is undefined or null
 * @type Boolean
 */
function defined(/* ... */) {
    for (var i = 0; i < arguments.length; ++i) {
        if ((typeof arguments[i] == 'undefined') || (arguments[i] === null)) {
            return false;
        }
    }
    
    return true;
}


/**
 * Sets a timer.
 *
 * @param f function to be called or string to be eval'ed when the timer expires
 * @param {Number} timeout timeout in milliseconds
 * @return the timer ID
 * @type Number
 */
window._setTimeout = window.setTimeout;
window.setTimeout = function(f, timeout) {
    if (typeof(f) != 'function') {
        return window._setTimeout(f, timeout);
    }
    
    window.setTimeout._timers.add({run: f, tm: timeout}).sort(function(t1, t2) {
        return t2.tm - t1.tm;    // Sort by descending timeouts.
    });
    
    return window._setTimeout('window.setTimeout._timers.pop().run()', timeout);
};
window.setTimeout._timers = [];


decodeURI = function(encodedURI) {
    var reservedSet = [';', '/', '?', ':', '@', '&', '=', '+', '$' ,',', '#'];
    return decode(String(encodedURI), reservedSet);
};


decodeURIComponent = function(encodedURIComponent) {
    return decode(String(encodedURIComponent), []);
};


var isFiniteOriginal = isFinite;

isFinite = function(number) {
    return isFiniteOriginal(Object.toNumber(number));
};


var isNaNOriginal = isNaN;

isNaN = function(number) {
    // NaN is the only value that doesn't compare equal to itself.
    return isNaNOriginal(Object.toNumber(number));
};


/**
 * Gets all property names from an object.
 *
 * @param {Object} object object from which to get all property names
 * @return all property names from the given object
 * @type Array
 */
Object.keys = function(object) {
    var i = 0, keys = [];
    for (keys[i++] in object);
    return keys;
};


/**
 * Gets all property values from an object.
 *
 * @param {Object} object object from which to get all property values
 * @return all property values from the given object
 * @type Array
 */
Object.values = function(object) {
    var values = [];
    
    for (var key in object) {
        values.push(object[key]);
    }
    
    return values;
};


/**
 * Deserializes a string representing a serialized object.
 *
 * @param {String} string string representing the serialized object
 * @return the deserialized object
 * @type Object
 */
Object.deserialize = function(string) {
    var object = {}, pairs = string.split(';');
    
    for (var i = 0; i < pairs.length; ++i) {
        var fields = pairs[i].split(':');
        var type = fields[0];
        var property = decodeURIComponent(fields[1]);
        var value = decodeURIComponent(fields[2]);
        
        // Type conversion:
        switch (type) {
        case 'number':
            value = Number(value);
            break;
        case 'boolean':
            value = Boolean(value);
            break;
        case 'object':
            if (value == 'this') {
                value = object;
            }
            else if (value == 'null') {
                value = null;
            }
            else {
                value = Object.deserialize(value);
            }
            break;
        case 'array':
            value = Array.from(Object.deserialize(value));
            break;
        default:
            break;
        }
        
        object[property] = value;
    }
    
    return object;
};


/**
 * Serializes an object's attributes (excluding its methods) into a string.
 *
 * @param {Object} object object to be serialized
 * @return a string representing the serialized object
 * @type String
 */
Object.serialize = function(object) {
    var string = '';
    
    for (var property in object) {
        var value = object[property];
        var type = typeof(value);
        
        switch (type) {
        case 'function':
            continue;
            break;
        case 'object':
            if (value == object) {
                value = 'this';    // Skip self-referencing properties.
            }
            else if (value === null) {
                value = 'null';
            }
            else {
                if (value instanceof Array) {
                    type = 'array';
                }
                value = Object.serialize(value);
            }
            break;
        default:
            break;
        }
        
        string += type + ':' + encodeURIComponent(property) + ':'
                  + encodeURIComponent(value) + ';';
    }
    
    if (object instanceof Array) {
        return string + 'number:length:' + object.length + ';';
    }
    else {
        return string;
    }
};


/**
 * Decodes the given string.
 *
 * Each escape sequence of the form "%XX", where "XX" is the hexadecimal
 * representation (in upper case) of a character, is transformed into a
 * character. That character will be part of the decoded string unless it's in
 * the reserved set.
 *
 * @param {String} string encoded string
 * @param {Array} reservedSet characters not to be decoded
 * @return a decoded copy of the given string
 * @type String
 */
function decode(string, reservedSet) {
    var decodedString = '';
    var reservedChars = /^$/;
    var hexadecimal = /^([\dA-F]+)$/;
    var messages = {
        decodingError: 'String.decode: Decoding error.',
        expectingSequence: 'String.decode: Expecting a "%XX" sequence.',
        invalidSequence: 'String.decode: Invalid "%XX" sequence.'
    };
    
    if (reservedSet.length > 0) {
        reservedChars = new RegExp('^[' + reservedSet.join('') + ']$');
    }
    
    for (var i = 0; i < string.length; ++i) {
        var c = string.charAt(i);
        if (c != '%') {
            decodedString += c;
            continue;
        }
        
        if ((i + 2) >= string.length) {
            throw new URIError(messages.expectingSequence);
        }
        if (!string.slice(i + 1, i + 3).match(hexadecimal)) {
            throw new URIError(messages.invalidSequence);
        }
        
        var byteValue = parseInt(RegExp.$1, 16) & 0xFF;
        var start = i;
        i += 2;
        
        if ((byteValue & 0x80) == 0) {
            c = String.fromCharCode(byteValue);
        }
        else {
            for (var n = 0; ((byteValue << n) & 0x80) != 0; ++n);
            if ((n == 1) || (n > 4)) {
                throw new URIError(messages.decodingError);
            }
            
            var encodedChars = new Array(n);
            encodedChars[0] = String.fromCharCode(byteValue);
            
            if ((i + 3 * (n - 1)) >= string.length) {
                throw new URIError(messages.decodingError);
            }
            
            for (var j = 1; j < n; ++j) {
                ++i;
                
                if (string.charAt(i) != '%') {
                    throw new URIError(messages.expectingSequence);
                }
                if (!string.slice(i + 1, i + 3).match(hexadecimal)) {
                    throw new URIError(messages.invalidSequence);
                }
                
                byteValue = parseInt(RegExp.$1, 16) & 0xFF;
                if ((byteValue & 0xC0) != 0x80) {
                    throw new URIError(messages.decodingError);
                }
                
                encodedChars[j] = String.fromCharCode(byteValue);
                i += 2;
            }
            
            var encodedChar = String.decodeUTF8(encodedChars.join(''));
            var charValue = encodedChar.charCodeAt(0);
            
            if (charValue < 0x10000) {
                c = String.fromCharCode(charValue);
            }
            else if (charValue > 0x10FFFF) {
                throw new URIError(messages.decodingError);
            }
            else {
                var low = ((charValue - 0x10000) & 0x3FF) + 0xDC00;
                var high = (((charValue - 0x10000) >> 10) & 0x3FF) + 0xD800;
                decodedString += String.fromCharCode(high, low);
                continue;
            }
        }
        
        c = !c.match(reservedChars) ? c : string.slice(start, i + 1);
        decodedString += c;
    }
    
    return decodedString;
}


/**
 * Encodes the given string.
 *
 * Each character in the string not in the unescaped set is first encoded in
 * UTF-8, and each octet in it is then transformed into a string with each one
 * represented by an escape sequence of the form "%XX", where "XX" is its
 * hexadecimal representation (in uppercase).
 *
 * @param {String} string unencoded string
 * @param {Array} unescapedSet characters not to be escaped
 * @return an encoded copy of the given string
 * @type String
 */
function encode(string, unescapedSet) {
    var unreservedChars = /^$/;
    var encodedString = '';
    
    if (unescapedSet.length > 0) {
        unreservedChars = new RegExp('^[' + unescapedSet.join('') + ']$');
    }
    
    for (var i = 0; i < string.length; ++i) {
        if (string.charAt(i).match(unreservedChars)) {
            encodedString += string.charAt(i);
            continue;
        }
        
        var newCharCode, charCode1 = string.charCodeAt(i);
        
        if ((charCode1 >= 0xDC00) && (charCode1 <= 0xDFFF)) {
            throw new URIError('String.encode: Encoding error.');
        }
        else if ((charCode1 < 0xD800) || (charCode1 > 0xDBFF)) {
            newCharCode = charCode1;
        }
        else {
            if (++i == string.length) {
                throw new URIError();
            }
            
            var charCode2 = string.charCodeAt(i);
            if ((charCode2 < 0xDC00) || (charCode2 > 0xDFFF)) {
                throw new URIError('String.encode: Encoding error.');
            }
            
            newCharCode = (charCode1 - 0xD800) * 0x400
                          + charCode2 - 0xDC00 + 0x10000;
        }
        
        var bytes = String.encodeUTF8(String.fromCharCode(newCharCode));
        
        for (var j = 0; j < bytes.length; ++j) {
            var code = bytes.charCodeAt(j).toString(16).toUpperCase();
            encodedString += '%' + (code.length == 1 ? '0' : '') + code;
        }
    }
    
    return encodedString;
}


// End of local scope.
})();
