/**
 * Computes the sign of the given number.
 *
 * @param {Number} number number for which to compute its sign
 * @return -1 if the number is negative or 1 otherwise
 * @type Number
 */
Math.sign = function(number) {
    return number < 0 ? -1 : 1;
};


Number.prototype.isInt32 = function() {
    return this === Object.toInt32(this);
};


Number.prototype.isInteger = function() {
    return this === Object.toInteger(this);
};


Number.prototype.isUInt32 = function() {
    return this === Object.toUInt32(this);
};


/**
 * Converts the number to the given base.
 *
 * @param {Number} base base in which to convert the number (1 < base < 37)
 * @param {Array} symbols optional array of symbols to use in the conversion
 * @return the number converted to the given base
 * @type String
 */
Number.prototype.toBase = function(base, symbols) {
    if (this == 0) {
        return '0';
    }
    
    symbols = symbols || '0123456789ABCDEFGHYJKLMNOPQRSTUVWXYZ'.split('');
    
    var string = '';
    var negative = (this < 0);
    var number = Math.floor(Math.abs(this));
    
    while (number != 0) {
        string = symbols[number % base] + string;
        number = Math.floor(number / base);
    }
    
    if (negative) {
        string = '-' + string;
    }
    return string;
};


Number.like = function(object) {
    return !isNaN(Number(object));
};


/**
 * Repeats the given function n times.
 *
 * @param {Function} f function to be called repeatedly (each invocation
 *                     receives the current iteration number and total)
 * @return an array with all the values returned by the function
 * @type Array
 */
Number.prototype.times = function(f) {
    var values = [];
    
    for (var n = this; n > 0; --n) {
        var value = f(this - n + 1, this);
        
        if (defined(value)) {
            values.push(value);
        }
    }
    
    return values;
};



/**
 * Converts the number to hexadecimal.
 *
 * @return the number converted to hexadecimal
 * @type String
 */
Number.prototype.toHex = function() {
    return this.toBase(16);
};