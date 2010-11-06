package dreme;

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
        return new ParsingVisitor().process(tokens);
    }

    private static class ParsingVisitor implements Tokens.Visitor {

        private final ParseStack stack = new ParseStack();
        private List lastList = null;
        private boolean endOfStream = false;

        public List process(TokenStream stream) throws IOException {
            do {
                stream.getToken().acceptVisitor(this);
            } while (!endOfStream && stack.hasFrames());

            if (endOfStream && stack.hasFrames())
                throw new IllegalStateException("Not enough closing braces. Stack: " + stack);

            return lastList;
        }

        public void openParens() {
            stack.push(new List());
        }

        public void closeParens() {
            lastList = stack.pop();
        }

        public void dot() {
            stack.dot();
        }

        public void quote() {
            stack.quote();
        }

        public void quasiquote() {
            stack.quasiquote();
        }

        public void unquote() {
            stack.unquote();
        }

        public void ellipsis() {
            stack.add(Ellipsis.INSTANCE);
        }

        public void endOfStream() {
            endOfStream = true;
        }

        public void t() {
            stack.add(SchemeBoolean.TRUE);
        }

        public void f() {
            stack.add(SchemeBoolean.FALSE);
        }

        public void visit(Tokens.Token token) {
            throw new IllegalArgumentException("Unknown token type: " + token);
        }

        public void visit(Tokens.BareWord word) {
            stack.add(new Identifier((word.getValue())));
        }

        public void visit(Tokens.SString string) {
            stack.add(new SchemeString(string.getValue()));
        }

        public void visit(Tokens.Decimal decimal) {
            stack.add(new Number(decimal.getValue()));
        }

        public void visit(Tokens.Integer integer) {
            stack.add(new Number(integer.getValue()));
        }
    }

    private static class ParseStack {
        private Stack<List> stack = new Stack<List>();

        public void push(List newList) {
            if (!stack.isEmpty()) { // already in a list
                stack.peek().add(newList);
            }
            stack.push(newList);
        }

        public boolean hasFrames() {
            return !stack.isEmpty();
        }

        private List current() {
            return stack.peek();
        }

        public void quote() {
            push(new QuotedList("quote"));
        }

        public void quasiquote() {
            push(new QuotedList("quasiquote"));
        }

        public void unquote() {
            push(new QuotedList("unquote"));
        }

        public List pop() {
            List result = stack.pop();
            result.close();
            while (!stack.isEmpty() && current() instanceof QuotedList) {
                result = stack.pop();
                result.close();
            }
            return result;
        }

        public void add(SchemeObject o) {
            if (stack.isEmpty())
                throw new IllegalStateException("Atom found outside list: " + o);
            current().add(o);
        }

        public void dot() {
            if (current().isEmpty()) {
                add(new Identifier("."));
            }
            else {
                current().dot();
            }
        }

        private final class QuotedList extends List {
            private QuotedList(String name) {
                super.add(new Identifier(name));
            }

            public List add(SchemeObject o) {
                if (!(o instanceof Pair))
                    stack.pop();
                return super.add(o);
            }
        }
    }
}

