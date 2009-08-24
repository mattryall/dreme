package daydreme;

class Identifier implements SchemeObject {
    private final String name;

    public Identifier(String name) {
        this.name = name;
    }

    public String toString() {
        return name;
    }

    public SchemeObject evaluate(Environment environment) {
        if (!environment.contains(this))
            throw new IllegalStateException("Unbound variable: " + name);
        return environment.get(this);
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return name.equals(((Identifier) o).name);
    }

    public int hashCode() {
        return name.hashCode();
    }
}
