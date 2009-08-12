package dreme;

public class Tokens
{
    public static interface Token {}

    public static final Token OPEN_PARENS = new Token() {
        public String toString()
        {
            return "OPEN_PARENS";
        }
    };

    public static final Token CLOSE_PARENS = new Token() {
        public String toString()
        {
            return "CLOSE_PARENS";
        }
    };

    public static class BareWord implements Token {
        private String value;

        public BareWord(String value)
        {
            if (value == null)
                throw new IllegalArgumentException("Value cannot be null");
            this.value = value;
        }

        public String toString()
        {
            return "BARE_WORD(\"" + value + "\")";
        }

        public boolean equals(Object o)
        {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            return value.equals(((BareWord) o).value);
        }

        public int hashCode()
        {
            return value.hashCode();
        }
    }

    public static final Token END_OF_STREAM = new Token() {
        public String toString()
        {
            return "EOS";
        }
    };
}
