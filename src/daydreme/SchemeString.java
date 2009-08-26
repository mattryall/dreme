package daydreme;

public class SchemeString implements SchemeObject {
    private final String value;

    public SchemeString(String value) {
        this.value = value.intern();
    }

    public SchemeObject evaluate(Environment environment) {
        return this;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return value.equals(((SchemeString) o).value);
    }

    public int hashCode() {
        return value.hashCode();
    }
}
