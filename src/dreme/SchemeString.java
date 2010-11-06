package dreme;

public class SchemeString implements SchemeObject {
    private final String value;

    public SchemeString(String value) {
        this.value = value.intern();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return value.equals(((SchemeString) o).value);
    }

    public String getValue() {
        return value;
    }

    public int hashCode() {
        return value.hashCode();
    }

    public String toString() {
        return "\"" + value + "\"";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.string(this);
    }
}
