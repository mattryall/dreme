package daydreme;

class Integer implements SchemeObject {
    private final int value;

    public Integer(int value) {
        this.value = value;
    }

    public String toString() {
        return String.valueOf(value);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return value == ((Integer) o).value;
    }

    public int hashCode() {
        return value;
    }

    public int getValue() {
        return value;
    }

    public SchemeObject evaluate(Environment environment) {
        return this;
    }
}
