package dreme;

import junit.framework.TestCase;
import static dreme.SchemeObjects.*;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class TestList extends TestCase {
    public void testListsArePairs() throws Exception {
        assertEquals("empty", pair(null, null), list());
        assertEquals("one item", pair(num(1), null), list(num(1)));
        assertEquals("two items", pair(num(1), pair(num(2), null)), list(num(1), num(2)));
        assertEquals("three items", pair(num(1), pair(num(2), pair(num(3), null))),
            list(num(1), num(2), num(3)));
        assertEquals("one pair improper", pair(num(1), num(2)), list(num(1)).addTerminal(num(2)));
        assertEquals("two pairs improper", pair(num(1), pair(num(2), num(3))),
            list(num(1), num(2)).addTerminal(num(3)));
    }

    public void testIteration() throws Exception {
        Iterator<SchemeObject> iter = list(word("a"), word("b"), word("c")).iterator();
        assertTrue(iter.hasNext());
        assertEquals(word("a"), iter.next());
        assertTrue(iter.hasNext());
        assertEquals(word("b"), iter.next());
        assertTrue(iter.hasNext());
        assertEquals(word("c"), iter.next());
        assertFalse(iter.hasNext());
        try {
            iter.next();
            fail("Expected exception not thrown");
        }
        catch (NoSuchElementException expected) {
        }
    }

    public void testIterationOnEmptyList() throws Exception {
        Iterator<SchemeObject> emptyIter = list().iterator();
        assertFalse(emptyIter.hasNext());
        try {
            emptyIter.next();
            fail("Expected exception not thrown");
        }
        catch (NoSuchElementException expected) {
        }
    }

    public void testConstructFromPair() throws Exception {
        Pair pair = pair(num(1), pair(num(2), null));
        final List list = List.toList(pair);
        assertEquals(num(1), list.get(0));
        assertEquals(num(2), list.get(1));
        assertEquals(2, list.size());
    }
}
