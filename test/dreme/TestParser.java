package dreme;

import junit.framework.TestCase;
import static dreme.ConsUtils.*;

public class TestParser extends TestCase
{
    public void testOneToken() throws Exception
    {
        Parser parser = new Parser(new TokenStream("(test)"));
        assertEquals(
            cons(word("test"), null),
            parser.parse());
    }

    public void testTwoTokens() throws Exception
    {
        Parser parser = new Parser(new TokenStream("(test 1)"));
        assertEquals(
            cons(word("test"), cons(num("1"), null)),
            parser.parse());
    }

    public void testThreeTokens() throws Exception
    {
        Parser parser = new Parser(new TokenStream("(test 1 2)"));
        assertEquals(
            cons(word("test"), cons(num("1"), cons(num("2"), null))),
            parser.parse());
    }

    public void testNested() throws Exception
    {
        Parser parser = new Parser(new TokenStream("(test (foo))"));
        assertEquals(
            cons(word("test"), list(cons(word("foo"), null))),
            parser.parse());
    }

    public void testHard() throws Exception
    {
        Parser parser = new Parser(new TokenStream("(test (foo) bar)"));
        assertEquals(
            cons(word("test"), cons(list(word("foo")), cons(word("bar"), null))),
            parser.parse());
    }

    public void testMultiValueNestedList() throws Exception
    {
        Parser parser = new Parser(new TokenStream("(first (pass 1 -345))"));
        assertEquals(
            cons(word("first"), list(cons(word("pass"), cons(num("1"), cons(num("-345"), null))))),
            parser.parse());
    }

    public void testInitialList() throws Exception
    {
        Parser parser = new Parser(new TokenStream("((first second) third)"));
        assertEquals(
            cons(cons(word("first"), cons(word("second"), null)),
                cons(word("third"), null)),
            parser.parse());
    }

    public void testFinalEmptyList() throws Exception
    {
        Parser parser = new Parser(new TokenStream("((a b) ((c) d) ())"));
        assertEquals(
            cons(
                list(cons(word("a"), cons(word("b"), null))),
                cons(
                    list(cons(cons(word("c"), null), cons(word("d"), null))),
                        cons(null, null))),
            parser.parse());
    }
}
