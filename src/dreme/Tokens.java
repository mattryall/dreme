package dreme;

public class Tokens
{
    public static interface Token {
        void acceptVisitor(Visitor visitor);
    }

    public static interface Visitor {
        void openParens();
        void closeParens();
        void dot();
        void quote();
        void unquote();
        void ellipsis();
        void endOfStream();
        void t();
        void f();

        void visit(Token token);
        void visit(BareWord word);
        void visit(SString string);
        void visit(Decimal decimal);
        void visit(Integer integer);
    }

    public static final Token OPEN_PARENS = new Token() {
        public String toString()
        {
            return "OPEN_PARENS";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.openParens();
        }
    };

    public static final Token CLOSE_PARENS = new Token() {
        public String toString()
        {
            return "CLOSE_PARENS";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.closeParens();
        }
    };

    public static final Token DOT = new Token() {
        public String toString() {
            return "DOT";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.dot();
        }
    };

    public static final Token QUOTE = new Token() {
        public String toString() {
            return "QUOTE";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.quote();
        }
    };

    public static final Token QUASIQUOTE = new Token() {
        public String toString() {
            return "QUASIQUOTE";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.quote();
        }
    };

    public static final Token UNQUOTE = new Token() {
        public String toString() {
            return "UNQUOTE";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.unquote();
        }
    };

    public static final Token ELLIPSIS = new Token() {
        public String toString() {
            return "ELLIPSIS";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.ellipsis();
        }
    };

    public static abstract class Value implements Token {
        private final String value;

        public Value(String value)
        {
            if (value == null)
                throw new IllegalArgumentException("Value cannot be null");
            this.value = value;
        }

        public String toString()
        {
            return value;
        }

        public boolean equals(Object o)
        {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return value.equals(((Value) o).value);
        }

        public int hashCode()
        {
            return value.hashCode();
        }

        public String getValue() {
            return value;
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.visit(this);
        }
    }

    public static final Token END_OF_STREAM = new Token() {
        public String toString()
        {
            return "EOS";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.endOfStream();
        }
    };

    public static class BareWord extends Value
    {
        public BareWord(String value)
        {
            super(value);
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.visit(this);
        }
    }

    public static class SString extends Value
    {
        public SString(String value)
        {
            super(value);
        }

        public String toString() {
            return "\"" + super.toString() + "\"";
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.visit(this);
        }
    }

    public static class Integer extends Value
    {
        public Integer(String value)
        {
            super(value);
        }

        public long getLong()
        {
            return java.lang.Long.parseLong(getValue());
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.visit(this);
        }
    }

    public static class Decimal extends Value
    {
        public Decimal(String value)
        {
            super(value);
        }

        public double getDouble() {
            return java.lang.Double.parseDouble(getValue());
        }

        public void acceptVisitor(Visitor visitor) {
            visitor.visit(this);
        }
    }

    public static class Boolean implements Token {
        public static Boolean TRUE = new Boolean();
        public static Boolean FALSE = new Boolean();

        private Boolean() {}

        public void acceptVisitor(Visitor visitor) {
            if (this == TRUE)
                visitor.t();
            else
                visitor.f();
        }
    }
}
