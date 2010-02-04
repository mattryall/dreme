package daydreme;

import junit.framework.TestCase;

import static daydreme.SchemeObjects.*;

public class TestCaptures extends TestCase {
    public void testSingleCapture() throws Exception {
        Captures captures = new Captures();
        captures.put(word("a"), num(1));
        assertEquals(num(1), captures.get(word("a")));
        assertEquals(list(num(1)), captures.getAll(word("a")));
    }

    public void testMultipleCaptures() throws Exception {
        Captures captures = new Captures();
        captures.put(word("a"), num(1));
        captures.put(word("a"), num(2));
        captures.put(word("b"), num(3));
        assertTrue(captures.containsKey(word("a")));
        assertEquals(num(1), captures.get(word("a")));
        assertEquals(num(1), captures.get(word("a")));
        assertTrue(captures.containsKey(word("a")));
        assertEquals(list(num(1), num(2)), captures.getAll(word("a")));
        assertTrue(captures.containsKey(word("b")));
        assertEquals(list(num(3)), captures.getAll(word("b")));
    }

    public void testShift() throws Exception {
        Captures captures = new Captures();
        captures.put(word("a"), num(1));
        captures.put(word("a"), num(2));
        captures.put(word("b"), num(3));

        Captures shifted = captures.shift();

        assertTrue(shifted.containsKey(word("a")));
        assertEquals(num(2), shifted.get(word("a")));
        assertEquals(list(num(2)), shifted.getAll(word("a")));
        assertTrue(shifted.containsKey(word("b")));
        assertNull(shifted.get(word("b")));
        assertEquals(list(), shifted.getAll(word("b")));

        Captures shiftedTwice = shifted.shift();

        assertTrue(shiftedTwice.containsKey(word("a")));
        assertNull(shiftedTwice.get(word("a")));
        assertEquals(list(), shiftedTwice.getAll(word("a")));
        assertTrue(shiftedTwice.containsKey(word("b")));
        assertNull(shiftedTwice.get(word("b")));
        assertEquals(list(), shiftedTwice.getAll(word("b")));
    }
}