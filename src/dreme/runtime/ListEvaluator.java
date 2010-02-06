package dreme.runtime;

import dreme.*;
import org.apache.log4j.Logger;

class ListEvaluator {
    private static final Logger log = Logger.getLogger(ListEvaluator.class);
    
    SchemeObject evaluate(List executable, Environment environment) {
        if (log.isDebugEnabled())
    		log.debug("Evaluating list " + executable);

        SchemeStack callStack = new SchemeStack(executable, environment);
        evaluate(callStack);
        return callStack.getLastResult();
    }

    private static void evaluate(SchemeStack stack) {
        while (!stack.isEmpty()) {
            if (log.isDebugEnabled())
                log.debug("\nCurrent stack:\n" + stack);
            ActivationFrame frame = stack.currentFrame();
            ExecutionContext ctx = new ListEvaluatorExecutionContext(stack);

            if (frame.hasNext()) {
				SchemeObject nextObject = frame.next();
                if (nextObject instanceof Evaluatable) {
                    ((Evaluatable) nextObject).evaluate(ctx);
                }
                else {
                    ctx.addResult(nextObject);
                }
            }
			else {
				// all the arguments are evaluated, let's run the procedure
				frame.getOperator().apply(ctx);
			}
        }
    }

}
