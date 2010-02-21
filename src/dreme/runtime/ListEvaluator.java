package dreme.runtime;

import dreme.*;
import org.apache.log4j.Logger;

class ListEvaluator {
    private static final Logger log = Logger.getLogger(ListEvaluator.class);

    private final int stackSizeLimit;
    private final Environment environment;

    ListEvaluator(Environment environment) {
        this(SchemeStack.UNLIMITED_STACK_SIZE, environment);
    }

    ListEvaluator(int stackSizeLimit, Environment environment) {
        this.stackSizeLimit = stackSizeLimit;
        this.environment = environment;
    }

    SchemeObject evaluate(List executable) {
        if (log.isDebugEnabled())
    		log.debug("Evaluating list " + executable);

        SchemeStack callStack = new SchemeStack(stackSizeLimit, executable, environment);
        SchemeProcessor contextProcessor = new SchemeProcessor(new StackExecutionContext(callStack));
        return evaluate(callStack, contextProcessor);
    }

    private SchemeObject evaluate(SchemeStack stack, SchemeProcessor processor) {
        while (!stack.isEmpty()) {
            if (log.isDebugEnabled())
                log.debug("\nCurrent stack:\n" + stack);
            ActivationFrame frame = stack.currentFrame();
            if (frame.hasNext())
                processor.evaluate(frame.next());
            else
                processor.apply(frame.getOperator());
        }
        return stack.getLastResult();
    }

    public void bind(Identifier var, SchemeObject val) {
        environment.bind(var, val);
    }
}
