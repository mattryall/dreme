package dreme;

public class Number implements SchemeObject, Comparable<Number> {
    private final double value;

    public Number(double value) {
        this.value = value;
    }

    public Number(String value) {
        this.value = Double.valueOf(value);
    }

    public double getValue() {
        return value;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return Double.compare(((Number) o).value, value) == 0;
    }

    public int hashCode() {
        long temp = value != +0.0d ? Double.doubleToLongBits(value) : 0L;
        return (int) (temp ^ (temp >>> 32));
    }

    public String toString() {
        return String.valueOf(value);
    }

    public int compareTo(Number o) {
        return Double.compare(value, o.value);
    }
    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.number(this);
    }

    public boolean isInteger() {
        return Math.round(getValue()) == getValue();
    }

    public int intValue() {
        return (int) getValue();
    }
}
