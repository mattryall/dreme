package dreme;

import junit.framework.Assert;
import junit.framework.TestCase;
import static dreme.SchemeObjects.*;
import static dreme.Parser.Instance.parse;

public class TestPatternMatcher extends TestCase {
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
        Captures captures = captures("(e)", "(a)");
        assertEquals(word("a"), captures.get(word("e")));
    }

    public void testDoubleCapture() throws Exception {
        Captures captures = captures("(e1 e2)", "(a b)");
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(word("b"), captures.get(word("e2")));
    }

    public void testEllipsisCapture() throws Exception {
        Captures captures = captures("(e1 e2 ...)", "(a)");
        assertEquals(word("a"), captures.get(word("e1")));
        try {
            captures.get(word("e2"));
            fail("Should not have captured second value");
        }
        catch (IllegalStateException expected) {
        }

        captures = captures("(e1 e2 ...)", "(a b)");
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(word("b"), captures.get(word("e2")));

        captures = captures("(e1 e2 ...)", "(a b c)");
        assertEquals(word("a"), captures.get(word("e1")));
        assertEquals(list(word("b"), word("c")), captures.getAll(word("e2")));
    }

    public void testCaptureDistributiveEllipsis() throws Exception {
        Captures captures = captures("((x y) ...)", "((a b) (c d))");
        assertEquals(list(word("a"), word("c")), captures.getAll(word("x")));
        assertEquals(list(word("b"), word("d")), captures.getAll(word("y")));
    }

    public void testIdentityApply() throws Exception {
        Assert.assertEquals(parse("(a)"), apply("(e1)", "(e1)", "(a)"));
    }

    public void testSingleItemApply() throws Exception {
        Assert.assertEquals(parse("(a a)"), apply("(e1)", "(e1 e1)", "(a)"));
    }

    public void testTwoItemSwap() throws Exception {
        Assert.assertEquals(parse("(b a)"), apply("(e1 e2)", "(e2 e1)", "(a b)"));
    }

    public void testNonIdentifiersInTemplate() throws Exception {
        Assert.assertEquals(parse("(a #t)"), apply("(e1 e2)", "(e1 #t)", "(a b)"));
    }

    public void testNonCapturedIdentifiersInTemplate() throws Exception {
        Assert.assertEquals(parse("(a foo)"), apply("(e1 e2)", "(e1 foo)", "(a b)"));
    }

    public void testTemplateWithLists() throws Exception {
        Assert.assertEquals(parse("(a (foo b))"), apply("(e1 e2)", "(e1 (foo e2))", "(a b)"));
    }

    public void testIdentityWithEllipsis() throws Exception {
        Assert.assertEquals(parse("(a)"), apply("(e1 e2 ...)", "(e1 e2 ...)", "(a)"));
        Assert.assertEquals(parse("(a b)"), apply("(e1 e2 ...)", "(e1 e2 ...)", "(a b)"));
        Assert.assertEquals(parse("(a b c)"), apply("(e1 e2 ...)", "(e1 e2 ...)", "(a b c)"));
    }

    public void testMoreComplexEllipsis() throws Exception {
        Assert.assertEquals(parse("(a b)"), apply("(e1 e2 ...)", "(e1 e2 ... b)", "(a)"));
    }

    public void testEllipsisInPatternNotInTemplate() throws Exception {
        // TODO - make this case throw an exception
        Assert.assertEquals(parse("(a b)"), apply("(e1 e2 ...)", "(e1 e2)", "(a b c)"));
    }

    public void testDistributiveEllipsis() throws Exception {
        Assert.assertEquals(parse("((a c) (b d))"), apply("((x y) ...)", "((x ...) (y ...))", "((a b) (c d))"));
        Assert.assertEquals(parse("((f a b) (f c d))"), apply("((x y) ...)", "((f x y) ...)", "((a b) (c d))"));
        Assert.assertEquals(parse("((f a c) (f b d))"), apply("((x ...) (y ...))", "((f x y) ...)", "((a b) (c d))"));
    }

    public void testTwoEllipsisInTemplateWithSameIdentifier() throws Exception {
        Assert.assertEquals(parse("((a b c) (a b c))"), apply("(x ...)", "((x ...) (x ...))", "(a b c)"));
    }

    private static boolean matches(String pattern, String input) {
        PatternMatcher matcher = new PatternMatcher(parse(pattern), new List());
        return matcher.matches(parse(input));
    }

    private static Captures captures(String pattern, String input) {
        PatternMatcher matcher = new PatternMatcher(parse(pattern), new List());
        return matcher.capture(parse(input));
    }

    private static List apply(String pattern, String template, String input) {
        PatternMatcher matcher = new PatternMatcher(parse(pattern), new List());
        return matcher.apply(parse(input), parse(template));
    }
}
