package dreme;

import junit.framework.TestCase;
import static dreme.Tokens.*;

public class TestTokenStream extends TestCase
{
    public void testFirstPass() throws Exception
    {
        TokenStream stream = new TokenStream("(first (pass 1))");
        assertStreamEquals(stream,
            OPEN_PARENS,
            new BareWord("first"),
            OPEN_PARENS,
            new BareWord("pass"),
            new BareWord("1"),
            CLOSE_PARENS,
            CLOSE_PARENS,
            END_OF_STREAM);
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
