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

    public void testListsArePairs() throws Exception {
        // so we don't have to worry about testing pairs *and* lists throughout this class
        assertEquals("empty", pair(null, null), list());
        assertEquals("one item", pair(num(1), null), list(num(1)));
        assertEquals("two items", pair(num(1), pair(num(2), null)), list(num(1), num(2)));
        assertEquals("three items", pair(num(1), pair(num(2), pair(num(3), null))),
            list(num(1), num(2), num(3)));
        assertEquals("one pair improper", pair(num(1), num(2)), list(num(1)).addTerminal(num(2)));
        assertEquals("two pairs improper", pair(num(1), pair(num(2), num(3))),
            list(num(1), num(2)).addTerminal(num(3)));
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
}