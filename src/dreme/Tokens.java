package dreme;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

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
        void quasiquote();
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
            visitor.quasiquote();
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
            super(unescapeString(value));
        }

        private static String unescapeString(String value) {
            Writer writer = new StringWriter(value.length());
            try {
                unescapeString(writer, value);
            }
            catch (IOException e) {
                // should never happen because we're using a StringWriter
                throw new RuntimeException(e);
            }
            return writer.toString();
        }

        private static void unescapeString(Writer out, String str) throws IOException {
            if (out == null) {
                throw new IllegalArgumentException("The Writer must not be null");
            }
            if (str == null) {
                return;
            }
            int sz = str.length();
            StringBuffer unicode = new StringBuffer(4);
            boolean hadSlash = false;
            boolean inUnicode = false;
            for (int i = 0; i < sz; i++) {
                char ch = str.charAt(i);
                if (inUnicode) {
                    // if in unicode, then we're reading unicode
                    // values in somehow
                    unicode.append(ch);
                    if (unicode.length() == 4) {
                        // unicode now contains the four hex digits
                        // which represents our unicode chacater
                        try {
                            int value = java.lang.Integer.parseInt(unicode.toString(), 16);
                            out.write((char) value);
                            unicode.setLength(0);
                            inUnicode = false;
                            hadSlash = false;
                        } catch (NumberFormatException nfe) {
                            throw new RuntimeException("Unable to parse unicode value: " + unicode, nfe);
                        }
                    }
                    continue;
                }
                if (hadSlash) {
                    // handle an escaped value
                    hadSlash = false;
                    switch (ch) {
                        case '\\':
                            out.write('\\');
                            break;
                        case '\'':
                            out.write('\'');
                            break;
                        case '\"':
                            out.write('"');
                            break;
                        case 'r':
                            out.write('\r');
                            break;
                        case 'f':
                            out.write('\f');
                            break;
                        case 't':
                            out.write('\t');
                            break;
                        case 'n':
                            out.write('\n');
                            break;
                        case 'b':
                            out.write('\b');
                            break;
                        case 'u':
                            {
                                // uh-oh, we're in unicode country....
                                inUnicode = true;
                                break;
                            }
                        default :
                            out.write(ch);
                            break;
                    }
                    continue;
                } else if (ch == '\\') {
                    hadSlash = true;
                    continue;
                }
                out.write(ch);
            }
            if (hadSlash) {
                // then we're in the weird case of a \ at the end of the
                // string, let's output it anyway.
                out.write('\\');
            }
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
