package dreme;

/**
 * Objects that need to be reduced to primitive types before being passed as arguments, etc.
 */
public interface Evaluatable extends SchemeObject {
    void evaluate(ExecutionContext context);
}
