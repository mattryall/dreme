package daydreme;

import junit.framework.TestCase;
import static daydreme.SchemeObjects.*;

public class TestPatternMatcher extends TestCase {
    public void testEmptyMatches() throws Exception {
        PatternMatcher matcher = new PatternMatcher(list(), null);
        assertTrue(matcher.matches(list()));
        assertFalse(matcher.matches(list(SchemeBoolean.TRUE)));
    }

    public void testSingleArgument() throws Exception {
        PatternMatcher matcher = new PatternMatcher(list(word("e")), null);
        assertFalse(matcher.matches(list(word("a"), num(1))));
        assertTrue(matcher.matches(list(SchemeBoolean.TRUE)));
        assertFalse(matcher.matches(list()));
    }

    public void testMultipleArguments() throws Exception {
        PatternMatcher matcher = new PatternMatcher(list(word("e1"), word("e2")), null);
        assertTrue(matcher.matches(list(word("a"), num(1))));
        assertFalse(matcher.matches(list(word("a"))));
        assertFalse(matcher.matches(list()));
    }
}
