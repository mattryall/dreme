package dreme;

public class SchemeString implements SchemeObject {
    private final String value;

    public SchemeString(String value) {
        this.value = value.intern();
    }

    public void evaluate(ExecutionContext context) {
        context.addResult(this);
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
