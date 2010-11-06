package dreme;

public class SchemeBoolean implements SchemeObject {
    public static final SchemeBoolean TRUE = new SchemeBoolean("#t");
    public static final SchemeBoolean FALSE = new SchemeBoolean("#f");

    private final String value;

    private SchemeBoolean(String value) {
        this.value = value;
    }

    public String toString() {
        return value;
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.schemeBoolean(this);
    }
}
