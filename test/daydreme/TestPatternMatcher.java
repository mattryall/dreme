package daydreme;

import junit.framework.TestCase;
import static daydreme.SchemeObjects.*;
import static daydreme.Parser.Instance.parse;

import java.util.Map;

public class TestPatternMatcher extends TestCase {
    private static boolean matches(String pattern, String input) {
        PatternMatcher matcher = new PatternMatcher(parse(pattern), null);
        return matcher.matches(parse(input));
    }

    private static void assertMatches(String pattern, String input) {
        assertTrue("Pattern: " + pattern + " should match input: " + input,
            matches(pattern, input));
    }

    private static void assertDoesNotMatch(String pattern, String input) {
        assertFalse("Pattern: " + pattern + " should not match input: " + input,
            matches(pattern, input));
    }

    public void testEmptyMatches() throws Exception {
        assertMatches("()", "()");
        assertDoesNotMatch("()", "(a)");
    }

    public void testSingleArgument() throws Exception {
        assertDoesNotMatch("(e)", "()");
        assertMatches("(e)", "(a)");
        assertDoesNotMatch("(e)", "(a 1)");
    }

    public void testMultipleArguments() throws Exception {
        assertDoesNotMatch("(e1 e2)", "()");
        assertDoesNotMatch("(e1 e2)", "(a)");
        assertMatches("(e1 e2)", "(a 1)");
    }

    public void testSimpleEllipsis() throws Exception {
        assertDoesNotMatch("(e1 e2 ...)", "()");
        assertMatches("(e1 e2 ...)", "(a)");
        assertMatches("(e1 e2 ...)", "(a 1)");
        assertMatches("(e1 e2 ...)", "(a b c)");
    }

    public void testSingleCapture() throws Exception {
        Map<Identifier, SchemeObject> captures = captures("(e)", "(a)");
        assertEquals(1, captures.size());
        assertEquals(word("a"), captures.get(word("e")));
    }

    public void testDoubleCapture() throws Exception {
        Map<Identifier, SchemeObject> captures = captures("(e1 e2)", "(a b)");
        assertEquals(2, captures.size());
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(word("b"), captures.get(word("e2")));
    }

    public void testEllipsisCapture() throws Exception {
        Map<Identifier, SchemeObject> captures = captures("(e1 e2 ...)", "(a)");
        assertEquals(2, captures.size());
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(list(), captures.get(word("e2")));

        captures = captures("(e1 e2 ...)", "(a b)");
        assertEquals(2, captures.size());
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(list(word("b")), captures.get(word("e2")));

        captures = captures("(e1 e2 ...)", "(a b c)");
        assertEquals(2, captures.size());
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(list(word("b"), word("c")), captures.get(word("e2")));
    }

    public void testIdentityApply() throws Exception {
        assertEquals(parse("(a)"), apply("(e1)", "(e1)", "(a)"));
    }

    public void testSingleItemApply() throws Exception {
        assertEquals(parse("(a a)"), apply("(e1)", "(e1 e1)", "(a)"));
    }

    public void testTwoItemSwap() throws Exception {
        assertEquals(parse("(b a)"), apply("(e1 e2)", "(e2 e1)", "(a b)"));
    }

    public void testNonIdentifiersInTemplate() throws Exception {
        assertEquals(parse("(a #t)"), apply("(e1 e2)", "(e1 #t)", "(a b)"));
    }

    public void testNonCapturedIdentifiersInTemplate() throws Exception {
        assertEquals(parse("(a foo)"), apply("(e1 e2)", "(e1 foo)", "(a b)"));
    }

    public void testTemplateWithLists() throws Exception {
        assertEquals(parse("(a (foo b))"), apply("(e1 e2)", "(e1 (foo e2))", "(a b)"));
    }

    public void testIdentityWithEllipsis() throws Exception {
        assertEquals(parse("(a)"), apply("(e1 e2 ...)", "(e1 e2 ...)", "(a)"));
        assertEquals(parse("(a b)"), apply("(e1 e2 ...)", "(e1 e2 ...)", "(a b)"));
        assertEquals(parse("(a b c)"), apply("(e1 e2 ...)", "(e1 e2 ...)", "(a b c)"));
    }

    public void testMoreComplexEllipsis() throws Exception {
        assertEquals(parse("(a b)"), apply("(e1 e2 ...)", "(e1 e2 ... b)", "(a)"));
    }

    public void testEllipsisInPatternNotInTemplate() throws Exception {
        assertEquals(parse("(a (b c))"), apply("(e1 e2 ...)", "(e1 e2)", "(a b c)"));
    }

    public void testDistributiveEllipsis() throws Exception {
        assertEquals(parse("((a c) (b d))"), apply("((x y) ...)", "((x ...) (y ...))", "((a b) (c d))"));
    }

    private Map<Identifier, SchemeObject> captures(String pattern, String input) {
        PatternMatcher matcher = new PatternMatcher(parse(pattern), null);
        return matcher.capture(parse(input));
    }

    private List apply(String pattern, String template, String input) {
        PatternMatcher matcher = new PatternMatcher(parse(pattern), parse(template));
        return matcher.apply(parse(input));
    }
}
