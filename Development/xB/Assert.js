/**
 * @fileoverview Implements assertions.
 * @author Márcio Faustino
 * @version 2009-07-05
 */


/**
 * Asserts a condition.
 *
 * @param value condition value to assert
 * @throws Error Assertion failed.
 */
function assert(value) {
    if (arguments.length != 1) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (!value) {
        throw new Error('Assertion failed');
    }
}


/**
 * Asserts that two values are equal.
 *
 * @param x first value to compare with
 * @param y second value to compare with
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertEquals(x, y) {
    if (arguments.length != 2) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (x !== y) {
        throw new Error('Assertion failed: Not equal: ' + x + ' !== ' + y);
    }
}


/**
 * Asserts that an exception is thrown.
 *
 * @param {Object} type expected exception type
 * @param {Function} code function to be called
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertException(type, code) {
    if (arguments.length != 2) {
        throw new SyntaxError('Wrong number of arguments');
    }
    
    try {
        code();
    }
    catch (exception) {
        if (exception instanceof type) {
            return;
        }
        else {
            throw new Error('Assertion failed: Unexpected exception thrown: '
                + exception);
        }
    }
    
    throw new Error('Assertion failed: Expected exception not thrown: '
        + type);
}


/**
 * Asserts that a value is false.
 *
 * @param value value to assert it's false
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertFalse(value) {
    if (arguments.length != 1) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (value !== false) {
        throw new Error('Assertion failed: Not false: ' + value);
    }
};


/**
 * Asserts that a value is NaN.
 *
 * @param value value to assert it's NaN
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertNaN(value) {
    if (arguments.length != 1) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (!isNaN(value)) {
        throw new Error('Assertion failed: Not NaN: ' + value);
    }
}


/**
 * Asserts that two values are not equal.
 *
 * @param x first value to compare with
 * @param y second value to compare with
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertNotEquals(x, y) {
    if (arguments.length != 2) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (x === y) {
        throw new Error('Assertion failed: Equal: ' + x + ' === ' + y);
    }
}


/**
 * Asserts that a value is null.
 *
 * @param value value to assert it's null
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertNull(value) {
    if (arguments.length != 1) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (value !== null) {
        throw new Error('Assertion failed: Not null: ' + value);
    }
}


/**
 * Asserts that a value is true.
 *
 * @param value value to assert it's true
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertTrue(value) {
    if (arguments.length != 1) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (value !== true) {
        throw new Error('Assertion failed: Not true: ' + value);
    }
}


/**
 * Asserts that a value is undefined.
 *
 * @param value value to assert it's undefined
 * @throws Error Assertion failed.
 * @throws SyntaxError Wrong number of arguments.
 */
function assertUndefined(value) {
    if (arguments.length != 1) {
        throw new SyntaxError('Wrong number of arguments');
    }
    if (value !== undefined) {
        throw new Error('Assertion failed: Not undefined: ' + value);
    }
}
