package daydreme;

import junit.framework.TestCase;
import static daydreme.SchemeObjects.*;

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

    public void testLetStructure() throws Exception {
        assertEquals(
            list(
                word("let"),
                list(list(word("x"), num(5))),
                list(word("+"), word("x"), word("x"))),
            parse("(let ((x 5)) (+ x x))"));
    }
}