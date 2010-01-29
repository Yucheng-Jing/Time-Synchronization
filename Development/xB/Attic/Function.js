Function.prototype.attach = function(object) {
    var self = this;
    
    return function(/* ... */) {
        return self.apply(object, arguments);
    };
};


/**
 * Binds this function to an arguments list.
 *
 * @return new function bound to the given arguments
 * @type Function
 */
Function.prototype.bind = function(/* ... */) {
    var boundArguments = Array.from(arguments);
    var self = this;
    
    return function(/* ... */) {
        var all = [].concat(boundArguments, Array.from(arguments));
        return self.apply(this, all);
    };
};


Function.prototype.apply = function(thisArg, argArray) {
    var functionName = 'Function.prototype.apply';
    
    if (typeof this != 'function') {
        throw new TypeError(functionName + ' called on incompatible object.');
    }
    if (defined(argArray) && !(argArray instanceof Array)
        && (typeof(argArray.callee) != 'function'))
    {
        var msg = 'Second argument to ' + functionName + ' must be an array.';
        throw new TypeError(msg);
    }
    
    thisArg = defined(thisArg) ? Object.toObject(thisArg) : Object.global;
    
    if (!defined(argArray)) {
        argArray = [];
    }
    
    // Look for an unused property to temporarily store the apply method.
    var methodName = 'apply_';
    while (methodName in thisArg) {
        methodName += '_';
    }
    
    var parameters = [];
    var maxArgs = Object.toUInt32(argArray.length) - 1;
    
    for (var i = 0; (i < argArray.length) && (i <= maxArgs); ++i) {
        parameters.push('argArray[' + i + ']');
    }
    
    thisArg[methodName] = this;
    var value = eval('thisArg["' + methodName + '"](' + parameters + ')');
    
    try {
        delete thisArg[methodName];
    }
    catch (exception) {
        // No exception will be thrown.
    }
    
    return value;
};
