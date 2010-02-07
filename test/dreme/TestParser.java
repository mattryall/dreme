package dreme;

import junit.framework.TestCase;

import static dreme.SchemeObjects.*;

import java.io.IOException;

public class TestParser extends TestCase
{
    private Parser parser = new Parser();
    
    private List parse(String scheme) throws IOException {
        return parser.parse(scheme);
    }

    public void testOneToken() throws Exception
    {
        assertEquals(
            list(word("test")),
            parse("(test)"));
    }

    public void testTwoTokens() throws Exception
    {
        assertEquals(
            list(word("test"), num(1)),
            parse("(test 1)"));
    }

    public void testThreeTokens() throws Exception
    {
        assertEquals(
            list(word("test"), num(1), num(2)),
            parse("(test 1 2)"));
    }

    public void testNested() throws Exception
    {
        assertEquals(
            list(word("test"), list(word("foo"))),
            parse("(test (foo))"));
    }

    public void testHard() throws Exception
    {
        assertEquals(
            list(word("test"), list(word("foo")), word("bar")),
            parse("(test (foo) bar)"));
    }

    public void testMultiValueNestedList() throws Exception
    {
        assertEquals(
            list(word("first"), list(word("pass"), num(1), num(-345))),
            parse("(first (pass 1 -345))"));
    }

    public void testInitialList() throws Exception
    {
        assertEquals(
            list(list(word("first"), word("second")), word("third")),
            parse("((first second) third)"));
    }

    public void testFinalEmptyList() throws Exception
    {
        assertEquals(
            list(
                list(word("a"), word("b")),
                list(list(word("c")), word("d")),
                list()),
            parse("((a b) ((c) d) ())"));
    }

    public void testImproperList() throws Exception {
        List result = parse("(a b . c)");
        assertEquals(list(word("a"), word("b")).addTerminal(word("c")), result);
        assertEquals(pair(word("a"), pair(word("b"), word("c"))), result);
    }

    public void testDottedPair() throws Exception {
        List result = parse("(a . c)");
        assertEquals(pair(word("a"), word("c")), result);
    }

    public void testDottedList() throws Exception {
        List result = parse("(a . (b . (c)))");
        assertEquals(list(word("a"), word("b"), word("c")), result);
    }

    public void testHangingDot() throws Exception {
        try {
            parse("(a . )");
            fail("Expected parse exception");
        }
        catch (IllegalStateException expected) {
        }
    }

    public void testHangingDotAfterQuotedIdentifier() throws Exception {
        try {
            parse("('a . )");
            fail("Expected parse exception");
        }
        catch (IllegalStateException expected) {
        }
    }

    public void testLetStructure() throws Exception {
        assertEquals(
            list(
                word("let"),
                list(list(word("x"), num(5))),
                list(word("+"), word("x"), word("x"))),
            parse("(let ((x 5)) (+ x x))"));
    }

    public void testQuotedSymbol() throws Exception {
        assertEquals(
            list(
                list(word("quote"), word("a")),
                list(word("b"), word("c")),
                word("d")),
            parse("('a (b c) d)"));
    }

    public void testQuotedList() throws Exception {
        assertEquals(
            list(
                word("a"),
                list(word("quote"), list(word("b"), word("c"))),
                word("d")),
            parse("(a '(b c) d)"));
    }

    public void testQuasiQuotedList() throws Exception {
        assertEquals(
            list(
                word("a"),
                list(word("quasiquote"), list(word("b"), list(word("unquote"), word("c")), word("d"))),
                word("e")),
            parse("(a `(b ,c d) e)"));
    }

    public void testQuasiQuotedSymbol() throws Exception {
        assertEquals(
            list(
                list(word("quasiquote"), word("a")),
                list(word("unquote"), list(word("b"), word("c"))),
                word("d")),
            parse("(`a ,(b c) d)"));
    }
}