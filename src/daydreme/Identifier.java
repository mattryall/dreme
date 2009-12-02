package daydreme;

public final class Identifier implements SchemeObject {
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
}
