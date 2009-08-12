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
                    StringBuffer buffer = new StringBuffer();
                    do {
                        if ("() \t\n".contains(Character.toString((char) next)))
                        {
                            reader.unread(next);
                            break;
                        }
                        buffer.append((char) next);
                    } while ((next = reader.read()) != -1);

                    char first = buffer.charAt(0);

                    if (!Character.isDigit(first) && first != '-')
                        return new BareWord(buffer.toString());

                    final String numberString = buffer.toString();
                    if (numberString.matches("-?\\d+"))
                        return new Tokens.Integer(numberString);

                    if (numberString.matches("-?\\d+\\.\\d+(e-?\\d+)?"))
                        return new Tokens.Decimal(numberString);

                    throw new IllegalStateException("Invalid token detected: " + buffer);

            }
        }
        return END_OF_STREAM;
    }
}
