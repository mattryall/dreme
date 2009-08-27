package daydreme;

public class SchemeBoolean implements SchemeObject {
    public static final SchemeBoolean TRUE = new SchemeBoolean("#t");
    public static final SchemeBoolean FALSE = new SchemeBoolean("#f");

    private final String value;

    private SchemeBoolean(String value) {
        this.value = value;
    }

    public SchemeObject evaluate(Environment environment) {
        return this;
    }

    public String toString() {
        return value;
    }
}
