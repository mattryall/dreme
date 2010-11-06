package dreme;

public final class Identifier implements SchemeObject, Comparable<Identifier> {
    private final String name;

    public Identifier(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public String toString() {
        return name;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return name.equals(((Identifier) o).name);
    }

    public int hashCode() {
        return name.hashCode();
    }

    public int compareTo(Identifier other) {
        return this.name.compareTo(other.name);
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.identifier(this);
    }
}
