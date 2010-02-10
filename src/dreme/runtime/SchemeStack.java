package dreme.runtime;

import dreme.Environment;
import dreme.List;
import dreme.SchemeObject;

import java.util.Stack;

class SchemeStack {
    static final int UNLIMITED_STACK_SIZE = -1;

    private final Stack<ActivationFrame> stack;
    private final int sizeLimit;

    private SchemeObject lastResult;

    public SchemeStack(int sizeLimit, List executable, Environment environment) {
        stack = new Stack<ActivationFrame>();
        this.sizeLimit = sizeLimit;
        pushFrame(executable, environment);
    }

    public SchemeStack(SchemeStack otherStack) {
        this.stack = (Stack<ActivationFrame>) otherStack.stack.clone();
        this.sizeLimit = otherStack.sizeLimit;
    }

    public void pushFrame(List executable, Environment environment) {
        if (sizeLimit != UNLIMITED_STACK_SIZE && stack.size() >= sizeLimit)
            throw new StackOverflowException("You suck");
        ActivationFrame newFrame = new ActivationFrame(executable, environment);
        stack.push(newFrame);
    }

    public void replaceFrame(List executable, Environment environment) {
        stack.pop();
        pushFrame(executable, environment);
    }

    public void returnValue(SchemeObject result) {
        lastResult = result;
        do {
            stack.pop();
        } while (!isEmpty() && stack.peek().isComplete()); // pop the complete frames off

        if (isEmpty()) return;
        currentFrame().addEvaluated(result);
    }

    public boolean isEmpty() {
        return stack.isEmpty();
    }

    public SchemeObject getLastResult() {
        return lastResult;
    }

    public ActivationFrame currentFrame() {
        return stack.peek();
    }

    public String toString() {
        int indent = 0;
        StringBuffer result = new StringBuffer(500);
        for (int i=0; i<stack.size(); i++) {
            ActivationFrame frame = stack.get(i);
            result.append(String.format("%2d%" + (indent + 1) + "s", i, " ")).append(frame);
            if (i < stack.size() - 1) result.append("\n");
            if (!frame.isComplete()) indent++;
        }
        return result.toString();
    }

    public void replaceWith(SchemeStack stack) {
        this.stack.clear();
        this.stack.addAll(stack.stack);
    }
}
