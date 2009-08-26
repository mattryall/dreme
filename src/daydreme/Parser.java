package daydreme;

import dreme.TokenStream;
import dreme.Tokens;

import java.io.IOException;
import java.util.Stack;

public class Parser {
    public List parse(String scheme) throws IOException {
        return parse(new TokenStream(scheme));
    }

    public List parse(TokenStream tokens) throws IOException {
        Tokens.Token token;
        Stack<List> openLists = new Stack<List>();
        while ((token = tokens.getToken()) != Tokens.END_OF_STREAM) {
            if (token == Tokens.OPEN_PARENS) {
                List newList = new List();
                if (!openLists.isEmpty()) { // already in a list
                    List currentList = openLists.peek();
                    currentList.add(newList);
                }
                openLists.push(newList);
            }
            else if (token == Tokens.CLOSE_PARENS) {
                List lastList = openLists.pop();
                if (openLists.isEmpty())
                    return lastList;
            }
            else if (token == Tokens.DOT) {
                token = tokens.getToken();
                if (!(token instanceof Tokens.Value))
                    throw new IllegalStateException("Invalid improper list terminator: " + token + ", stack: " + openLists);
                openLists.peek().addTerminal(toSchemeObject(token));
            }
            else {
                if (openLists.isEmpty())
                    throw new IllegalStateException("Atom found outside list: " + token);
                openLists.peek().add(toSchemeObject(token));
            }
        }
        throw new IllegalStateException("Not enough closing braces. Stack: " + openLists);
    }

    private SchemeObject toSchemeObject(Tokens.Token token) {
        if (token instanceof Tokens.BareWord) {
            return new Identifier(((Tokens.BareWord) token).getValue());
        }
        else if (token instanceof Tokens.Integer) {
            return new Integer(((Tokens.Integer) token).getInt());
        }
        else if (token instanceof Tokens.Decimal) {
            return new Decimal(((Tokens.Decimal) token).getDouble());
        }
        else if (token instanceof Tokens.SString) {
            return new SchemeString(((Tokens.SString) token).getValue());
        }
        throw new IllegalArgumentException("Unknown token type: " + token);
    }
}
