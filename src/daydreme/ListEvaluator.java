package daydreme;

import java.util.Stack;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;

public class ListEvaluator {

    private static final Set<Identifier> SPECIAL_FORMS = new HashSet<Identifier>() {{
        add(new Identifier("lambda"));
        add(new Identifier("define"));
        add(new Identifier("begin"));
    }};

    public SchemeObject evaluate(List list, Environment environment)
    {
        SchemeStack callStack = new SchemeStack();
        callStack.push(new ActivationFrame(list, environment));
        evaluate(callStack);
        return callStack.getLastResult();
    }

    private static void evaluate(SchemeStack stack)
    {
        while (!stack.isEmpty()) {
            System.out.println("\nCurrent stack:\n" + stack);
            ActivationFrame frame = stack.peek();
            Environment environment = frame.environment;
            List list = frame.rawValues;

            if (frame.evaluatedValues.size() == 0 && SPECIAL_FORMS.contains(list.head())) { // special processing
                Identifier specialForm = (Identifier) list.head();
                if (specialForm.getName().equals("lambda")) {
                    frame.evaluatedValues = frame.rawValues;
                }
                else if (specialForm.getName().equals("define")) {
                    frame.addEvaluated(environment.get((Identifier) frame.next()));
                    frame.addEvaluated(frame.next()); // skip evaluation of identifier
                }
                else if (specialForm.getName().equals("begin")) {
                    frame.addEvaluated(frame.next());
                }
            }
            else if (frame.hasNext()) {
                SchemeObject toEval = frame.next();
                if (toEval instanceof List) {
                    stack.push(new ActivationFrame((List) toEval, environment));
                }
                else {
                    frame.addEvaluated(toEval.evaluate(environment));
                }
            }
            // all the arguments are evaluated, let's run the procedure
            else if (frame.getOperator().equals(new Identifier("lambda"))) { // result of a lambda expression is a lambda procedure
                Lambda lambda = new Lambda(frame.evaluatedValues.tail().head(), frame.evaluatedValues.tail().tail(), environment);
                stack.returnValue(lambda);
            }
            else if (frame.getOperator() instanceof Lambda) {
                Lambda lambda = (Lambda) frame.getOperator();
                stack.push(new ActivationFrame(lambda.getBody(), lambda.getArgumentsEnv(frame.evaluatedValues.tail())));
            }
            else if (frame.getOperator() instanceof Procedure) {
                stack.returnValue(((Procedure) frame.getOperator()).apply(frame.evaluatedValues.tail(), environment));
            }
            else {
                stack.returnValue(frame.getResult());
            }
        }
    }

    private static class ActivationFrame implements Iterator<SchemeObject>
    {
        private List rawValues;
        private List evaluatedValues = new List();
        private Environment environment;

        private ActivationFrame(List list, Environment environment) {
            this.rawValues = list;
            this.environment = environment;
        }

        public boolean hasNext() {
            return !isComplete();
        }

        public SchemeObject next() {
            return rawValues.get(evaluatedValues.size());
        }

        public void remove() {
            throw new UnsupportedOperationException("Remove not supported");
        }

        void addEvaluated(SchemeObject o) {
            if (o == null)
                throw new IllegalArgumentException("Null is not a valid evaluation result");
            if (isComplete())
                throw new IllegalStateException("Can't add evaluation result " + o + " to complete frame: " + this);
            evaluatedValues.add(o);
        }

        public boolean isComplete() {
            return evaluatedValues.size() == rawValues.size();
        }

        public SchemeObject getResult() {
            return evaluatedValues.get(evaluatedValues.size() - 1);
        }

        public SchemeObject getOperator() {
            if (!isComplete())
                throw new IllegalStateException("Can't get operator for frame which is not fully evaluated: " + this);
            return evaluatedValues.head();
        }

        @Override
        public String toString() {
            StringBuffer result = new StringBuffer(500);
            result.append("[");
            for (int i=0; i<evaluatedValues.size(); i++) {
                if (i > 0) result.append(" ");
                result.append(evaluatedValues.get(i));
            }
            if (evaluatedValues.size() != rawValues.size())
                result.append(" ...");
            else
                result.append("]");
            return result.toString();
        }
    }

    private class SchemeStack extends Stack<ActivationFrame> {
        private SchemeObject lastResult;

        public void returnValue(SchemeObject result) {
            lastResult = result;
            do {
                pop();
            } while (!isEmpty() && peek().isComplete()); // pop the complete frames off

            if (isEmpty()) return;
            peek().addEvaluated(result);
        }

        public SchemeObject getLastResult() {
            return lastResult;
        }

        public synchronized String toString() {
            int indent = 0;
            StringBuffer result = new StringBuffer(500);
            for (int i=0; i<this.size(); i++) {
                ActivationFrame frame = this.get(i);
                result.append(String.format("%2d%" + (indent + 1) + "s", i, " ")).append(frame);
                if (i < this.size() - 1) result.append("\n");
                if (!frame.isComplete()) indent++;
            }
            return result.toString();
        }
    }
}
