package dreme;

import junit.framework.TestCase;
import static dreme.Tokens.*;

public class TestTokenStream extends TestCase
{
    public void testFirstPass() throws Exception
    {
        TokenStream stream = new TokenStream("(first (pass 1 -345 34.5 0.345e-7))");
        assertStreamEquals(stream,
            OPEN_PARENS,
            new BareWord("first"),
            OPEN_PARENS,
            new BareWord("pass"),
            new Tokens.Integer("1"),
            new Tokens.Integer("-345"),
            new Tokens.Decimal("34.5"),
            new Tokens.Decimal("0.345e-7"),
            CLOSE_PARENS,
            CLOSE_PARENS,
            END_OF_STREAM);
    }

    public void testImproperList() throws Exception
    {
        TokenStream stream = new TokenStream("(a b . c)");
        assertStreamEquals(stream,
            OPEN_PARENS,
            new BareWord("a"),
            new BareWord("b"),
            DOT,
            new BareWord("c"),
            CLOSE_PARENS,
            END_OF_STREAM);
    }

    public void testQuotedIdentifier() throws Exception
    {
        TokenStream stream = new TokenStream("(a b 'c)");
        assertStreamEquals(stream,
            OPEN_PARENS,
            new BareWord("a"),
            new BareWord("b"),
            QUOTE,
            new BareWord("c"),
            CLOSE_PARENS,
            END_OF_STREAM);
    }

    public void testQuotedList() throws Exception
    {
        TokenStream stream = new TokenStream("(a b '(c d))");
        assertStreamEquals(stream,
            OPEN_PARENS,
            new BareWord("a"),
            new BareWord("b"),
            QUOTE,
            OPEN_PARENS,
            new BareWord("c"),
            new BareWord("d"),
            CLOSE_PARENS,
            CLOSE_PARENS,
            END_OF_STREAM);
    }

    public void testQuasiQuote() throws Exception {
        TokenStream stream = new TokenStream("(a `(b ,c (d e)))");
        assertStreamEquals(stream,
            OPEN_PARENS,
            new BareWord("a"),
            QUASIQUOTE,
            OPEN_PARENS,
            new BareWord("b"),
            UNQUOTE,
            new BareWord("c"),
            OPEN_PARENS,
            new BareWord("d"),
            new BareWord("e"),
            CLOSE_PARENS,
            CLOSE_PARENS,
            CLOSE_PARENS);
    }

    public void testInvalidNumbersAreBareWords() throws Exception
    {
        assertEquals(new Tokens.BareWord("1a"), new TokenStream("1a").getToken());
    }

    private void assertStreamEquals(TokenStream stream, Token... tokens) throws Exception
    {
        for (int i=0; i<tokens.length; i++)
        {
            Token expected = tokens[i];
            Token actual = stream.getToken();
            assertEquals("Unexpected token at index " + i, expected, actual);
        }
    }
}
