package dreme;

/**
 * Represents the result of a binding operation (which formally has an "unspecified" result), or
 * any other result that can be passed around but raises an assertion failure if evaluated.
 */
public class Unspecified implements Evaluatable {
    public static final Unspecified INSTANCE = new Unspecified();

    private Unspecified() {
    }

    public void evaluate(ExecutionContext ctx) {
        throw new IllegalArgumentException("Attempted to evaluate " + this);
    }

    public String toString() {
        return "#<unspecified>";
    }
}
