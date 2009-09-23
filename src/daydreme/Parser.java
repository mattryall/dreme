package daydreme;

import dreme.TokenStream;
import dreme.Tokens;

import java.io.IOException;
import java.util.Stack;

public class Parser {
    public static class Instance {
        private static final Parser INSTANCE = new Parser();
        public static List parse(String scheme) {
            try {
                return INSTANCE.parse(scheme);
            }
            catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public List parse(String scheme) throws IOException {
        return parse(new TokenStream(scheme));
    }

    public List parse(TokenStream tokens) throws IOException {
        Tokens.Token token;
        ParseStack stack = new ParseStack();
        while ((token = tokens.getToken()) != Tokens.END_OF_STREAM) {
            if (token == Tokens.OPEN_PARENS) {
                stack.push(new List());
            }
            else if (token == Tokens.CLOSE_PARENS) {
                List lastList = stack.pop();
                if (stack.isEmpty())
                    return lastList;
            }
            else if (token == Tokens.DOT) {
                stack.dot();
            }
            else if (token == Tokens.QUOTE) {
                stack.quote();
            }
            else if (token == Tokens.QUASIQUOTE) {
                stack.quote();
            }
            else if (token == Tokens.UNQUOTE) {
                // ignore
            }
            else {
                stack.add(toSchemeObject(token));
            }
        }
        if (!stack.isEmpty())
            throw new IllegalStateException("Not enough closing braces. Stack: " + stack);

        // we've parsed the last form in the file
        return null;
    }

    private SchemeObject toSchemeObject(Tokens.Token token) {
        if (token instanceof Tokens.BareWord) {
            return new Identifier(((Tokens.BareWord) token).getValue());
        }
        else if (token instanceof Tokens.Integer) {
            return new Number(((Tokens.Integer) token).getValue());
        }
        else if (token instanceof Tokens.Decimal) {
            return new Number(((Tokens.Decimal) token).getValue());
        }
        else if (token instanceof Tokens.SString) {
            return new SchemeString(((Tokens.SString) token).getValue());
        }
        else if (token == Tokens.Boolean.TRUE) {
            return SchemeBoolean.TRUE;
        }
        else if (token == Tokens.Boolean.FALSE) {
            return SchemeBoolean.FALSE;
        }
        else if (token == Tokens.ELLIPSIS) {
            return Ellipsis.INSTANCE;
        }
        throw new IllegalArgumentException("Unknown token type: " + token);
    }

    private static class ParseStack
    {
        private Stack<List> stack = new Stack<List>();

        public void push(List newList) {
            if (!stack.isEmpty()) { // already in a list
                stack.peek().add(newList);
            }
            stack.push(newList);
        }

        public boolean isEmpty() {
            return stack.isEmpty();
        }

        private List current() {
            return stack.peek();
        }

        public void quote() {
            push(new QuoteList());
        }

        public List pop() {
            List result = stack.pop();
            result.close();
            return result;
        }

        public void add(SchemeObject o) {
            if (stack.isEmpty())
                throw new IllegalStateException("Atom found outside list: " + o);
            current().add(o);
        }

        public void dot() {
            current().dot();
        }

        private final class QuoteList extends List {
            private QuoteList() {
                super.add(new Identifier("quote"));
            }

            public List add(SchemeObject o) {
                if (!(o instanceof Pair))
                    stack.pop();
                return super.add(o);
            }

            @Override
            public void close() {
                super.close();
                stack.pop();
            }
        }
    }
}
