/**
 * @fileoverview Tests assertions.
 * @author Márcio Faustino
 * @version 2009-07-05
 */


test({
    testAssert: function() {
        assert(true);
        assert(1);
        assert('...');
        assert({});
        assert([]);
    },
    
    testAssertEquals: function() {
        assertEquals('', '');
        assertEquals(-0, +0);
        assertEquals(1, 1.0);
    },
    
    testAssertException: function() {
        assertException(Error, function() {
            throw Error();
        });
    },
    
    testAssertFalse: function() {
        assertFalse(false);
    },
    
    testAssertNull: function() {
        assertNull(null);
    },
    
    testAssertTrue: function() {
        assertTrue(true);
    },
    
    testAssertUndefined: function() {
        assertUndefined(undefined);
    },
    
    testCorrectNumberOfArguments: function() {
        assertException(SyntaxError, function() {assert()});
        assertException(SyntaxError, function() {assert(1, 2)});
        assertException(SyntaxError, function() {assertEquals(1)});
        assertException(SyntaxError, function() {assertEquals(1, 2, 3)});
        assertException(SyntaxError, function() {assertException(Error)});
        assertException(SyntaxError, function() {assertException(Error, 2, 3)});
        assertException(SyntaxError, function() {assertFalse()});
        assertException(SyntaxError, function() {assertFalse(1, 2)});
        assertException(SyntaxError, function() {assertNull()});
        assertException(SyntaxError, function() {assertNull(1, 2)});
        assertException(SyntaxError, function() {assertTrue()});
        assertException(SyntaxError, function() {assertTrue(1, 2)});
        assertException(SyntaxError, function() {assertUndefined()});
        assertException(SyntaxError, function() {assertUndefined(1, 2)});
    },
    
    testAssertionFailures: function() {
        assertException(Error, function() {assert('')});
        assertException(Error, function() {assertEquals(true, false)});
        assertException(Error, function() {assertEquals(null, undefined)});
        assertException(Error, function() {assertFalse(0)});
        assertException(Error, function() {assertNull(undefined)});
        assertException(Error, function() {assertTrue(1)});
        assertException(Error, function() {assertUndefined(null)});
        
        assertException(Error, function() {
            assertException(SyntaxError, function() {throw RangeError()});
        });
        
        assertException(Error, function() {
            assertException(Error, function() {});
        });
    }
});
