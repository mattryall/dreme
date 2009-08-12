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

    public void testInvalidToken() throws Exception
    {
        try
        {
            new TokenStream("1a").getToken();
            fail();
        }
        catch (IllegalStateException e)
        {
            // Expected
        }
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
