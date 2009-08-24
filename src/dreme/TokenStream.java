package dreme;

import java.io.Reader;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;

public class TokenStream {
    private PushbackReader reader;

    public TokenStream(String input) {
        this.reader = new PushbackReader(new StringReader(input));
    }

    public TokenStream(Reader reader) {
        this.reader = new PushbackReader(reader);
    }

    public Tokens.Token getToken() throws IOException {
        int c;
        while ((c = reader.read()) != -1) {
            switch ((char) c) {
                case '(':
                    return Tokens.OPEN_PARENS;
                case ')':
                    return Tokens.CLOSE_PARENS;
                case '.':
                    return Tokens.DOT;
                case ' ':
                case '\t':
                case '\n':
                    break;
                default:
                    int next = c;
                    StringBuffer buffer = new StringBuffer();
                    do {
                        if ("() \t\n".contains(Character.toString((char) next))) {
                            reader.unread(next);
                            break;
                        }
                        buffer.append((char) next);
                    } while ((next = reader.read()) != -1);

                    return toToken(buffer.toString());
            }
        }
        return Tokens.END_OF_STREAM;
    }

    private Tokens.Token toToken(String symbol) {
        char first = symbol.charAt(0);

        if (!Character.isDigit(first) && first != '-')
            return new Tokens.BareWord(symbol);

        if (symbol.matches("-?\\d+"))
            return new Tokens.Integer(symbol);

        if (symbol.matches("-?\\d+\\.\\d+(e-?\\d+)?"))
            return new Tokens.Decimal(symbol);

        throw new IllegalStateException("Invalid token detected: " + symbol);
    }
}
