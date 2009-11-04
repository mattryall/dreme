package daydreme;

import sun.jvm.hotspot.runtime.ThreadLocalAllocBuffer;

import java.util.Stack;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;

public class ListEvaluator {

    private final Stack<ActivationFrame> callStack = new Stack<ActivationFrame>();
    private static final Set<SchemeObject> SPECIAL_FORMS = new HashSet<SchemeObject>() {{
        add(new Identifier("lambda"));
    }};

    public SchemeObject evaluate(List list, Environment environment)
    {
        callStack.push(new ActivationFrame(list, environment));
        return evaluate(callStack);
    }

    private static SchemeObject evaluate(Stack<ActivationFrame> stack)
    {
        while (!stack.isEmpty()) {
            System.out.println("Current stack: " + stack);
            ActivationFrame frame = stack.peek();
            Environment environment = frame.environment;
            SchemeObject result = null;
            List list = frame.rawValues;

            if (frame.counter == 0 && SPECIAL_FORMS.contains(list.head())) { // special processing
                if (list.head().equals(new Identifier("lambda"))) {
                    result = new Lambda(list.tail().get(0), list.tail().tail(), environment);
                }
            }
            else if (frame.hasNext()) {
                SchemeObject toEval = frame.next();
                if (toEval instanceof List) {
                    stack.push(new ActivationFrame((List) toEval, environment));
                    continue;
                }
                else if (toEval instanceof Identifier) {
                    // TODO: Throw undefined identifier if null in environment
                    frame.addEvaluated(environment.get((Identifier) toEval));
                    continue;
                }
                else {
                    frame.addEvaluated(toEval);
                    continue;
                }
            }
            else {
                // all the arguments are evaluated, let's run the procedure
                List evaluated = frame.evaluatedValues;
                Procedure procedure = (Procedure) evaluated.head();
                
                System.out.println("Applying procedure " + procedure + " with args :" + evaluated.tail());
                result = procedure.apply(evaluated.tail(), environment);
            }

            stack.pop();
            if (stack.isEmpty())
                return result;

            stack.peek().evaluatedValues.add(result);
        }
        throw new RuntimeException("Stack underflow");
    }

    private static class ActivationFrame implements Iterator<SchemeObject>
    {
        private List rawValues;
        private List evaluatedValues = new List();
        private Environment environment;
        private int counter = 0;

        private ActivationFrame(List list, Environment environment) {
            this.rawValues = list;
            this.environment = environment;
        }

        public boolean hasNext() {
            return counter < rawValues.size();
        }

        public SchemeObject next() {
            return rawValues.get(counter++);
        }

        public void remove() {
            throw new UnsupportedOperationException("Remove not supported");
        }

        void addEvaluated(SchemeObject o) {
            if (o == null)
                throw new IllegalArgumentException("Null is not a valid evaluation result");
            evaluatedValues.add(o);
        }

        @Override
        public String toString() {
            return "ActivationFrame{" +
                "rawValues=" + rawValues +
                ", evaluatedValues=" + evaluatedValues +
                ", counter=" + counter +
                '}';
        }
    }

}
