package dreme;

/**
 * An operator that returns a fully-evaluated result.
 */
public abstract class Procedure implements Operator {
    private final String name;

    protected Procedure() {
        this(null);
    }

    protected Procedure(String name) {
        this.name = name;
    }

    public void apply(ExecutionContext context) {
        SchemeObject result = apply(context.evaluatedValues(), context.getEnvironment());
        context.returnValue(result);
    }

    protected abstract SchemeObject apply(List arguments, Environment environment);

    @Override
    public String toString() {
        return "#<procedure" + (name != null ? " " + name : "") + ">";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.procedure(this);
    }
}
