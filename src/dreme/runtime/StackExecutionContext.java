package dreme.runtime;

import dreme.*;
import org.apache.log4j.Logger;

class StackExecutionContext implements ExecutionContext {
    private static final Logger log = Logger.getLogger(StackExecutionContext.class);

    private final SchemeStack stack;

    public StackExecutionContext(SchemeStack stack) {
        this.stack = stack;
    }

    public boolean isHeadPosition() {
        return currentFrame().isNew();
    }

    private ActivationFrame currentFrame() {
        return stack.currentFrame();
    }

    public boolean isTailPosition() {
        return currentFrame().getEvaluatedValues().size() == currentFrame().getRawValues().size() - 1;
    }

    public List getRawBody() {
        return currentFrame().getRawValues().tail();
    }

    public Environment getEnvironment() {
        return currentFrame().getEnvironment();
    }

    public void returnLastResult() {
        returnValue(currentFrame().getResult());
    }

    public void returnValue(SchemeObject returnValue) {
        stack.returnValue(returnValue);
    }

    public ExecutionContext copy() {
        return new StackExecutionContext(new SchemeStack(stack));
    }

    public void replaceWith(ExecutionContext context) {
        if (!(context instanceof StackExecutionContext))
            throw new UnsupportedOperationException("Cannot replace context with unknown type");
        stack.replaceWith(((StackExecutionContext) context).stack);
    }

    public List evaluatedValues() {
        return currentFrame().getEvaluatedValues().tail();
    }

    public void addResult(SchemeObject result) {
        if (result instanceof Macro && isHeadPosition()) {
            ((Macro) result).process(getRawBody(), this);
        }
        else {
            currentFrame().addEvaluated(result);
        }
    }

    public void execute(List executable, Environment environment) {
        if (isTailPosition() && currentFrame().getEvaluatedValues().head() instanceof Container) {
	        if (log.isDebugEnabled())
		        log.debug("Tail call optimising");
            executeInPlace(executable, environment);
        }
        else
            stack.pushFrame(executable, environment);
    }

    public void executeInPlace(List executable, Environment environment) {
        stack.replaceFrame(executable, environment);
    }
}
