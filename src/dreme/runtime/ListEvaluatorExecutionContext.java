package dreme.runtime;

import dreme.*;

class ListEvaluatorExecutionContext implements ExecutionContext {
    private final SchemeStack stack;

    public ListEvaluatorExecutionContext(SchemeStack stack) {
        this.stack = stack;
    }

    public boolean isHeadPosition() {
        return currentFrame().isNew();
    }

    private ActivationFrame currentFrame() {
        return stack.currentFrame();
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

    public void skip() {
        addResult(currentFrame().next());
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
        stack.pushFrame(executable, environment);
    }

    public void executeInPlace(List executable, Environment environment) {
        stack.replaceFrame(executable, environment);
    }
}
