package dreme;

public final class Identifier implements Evaluatable, Comparable<Identifier> {
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

	public void evaluate(ExecutionContext context) {
        if (!context.getEnvironment().contains(this))
            throw new IllegalStateException("Unbound variable: " + name);
        context.addResult(context.getEnvironment().get(this));
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
}
