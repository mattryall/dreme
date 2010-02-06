package dreme;

/**
 * An operator that replaces itself with another SchemeObject to be executed instead.
 */
public abstract class Substitution implements Operator {
    public final void apply(ExecutionContext context) {
        context.executeInPlace(getSubstitute(context), getNewEnvironment(context));
    }

    /**
     * Default implementation returns the current context. Implementations should override
     * this to provide a new environment if necessary.
     */
    protected Environment getNewEnvironment(ExecutionContext context) {
        return context.getEnvironment();
    }

    protected abstract List getSubstitute(ExecutionContext context);
}
