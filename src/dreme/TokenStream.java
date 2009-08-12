package dreme;

import static dreme.Tokens.*;

import java.io.Reader;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;

public class TokenStream
{
    private PushbackReader reader;

    public TokenStream(String input)
    {
        this.reader = new PushbackReader(new StringReader(input));
    }

    public TokenStream(Reader reader)
    {
        this.reader = new PushbackReader(reader);
    }

    public Tokens.Token getToken() throws IOException
    {
        int c;
        while ((c = reader.read()) != -1)
        {
            switch ((char) c)
            {
                case '(':
                    return OPEN_PARENS;
                case ')':
                    return CLOSE_PARENS;
                case ' ':
                case '\t':
                case '\n':
                    break;
                default:
                    int next = c;
                    StringBuffer bareword = new StringBuffer();
                    do {
                        if ("() \t\n".contains(Character.toString((char) next)))
                        {
                            reader.unread(next);
                            break;
                        }
                        bareword.append((char) next);
                    } while ((next = reader.read()) != -1);
                    return new BareWord(bareword.toString());
            }
        }
        return END_OF_STREAM;
    }
}
