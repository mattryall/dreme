package daydreme;

import java.util.Stack;

public class ListEvaluator {

    private final Stack<ActivationFrame> callStack = new Stack<ActivationFrame>();

    public SchemeObject evaluate(List list, Environment environment)
    {
        callStack.push(new ActivationFrame(list, environment, null));
        return evaluate(callStack);
    }

    private static SchemeObject evaluate(Stack<ActivationFrame> stack)
    {
        while (!stack.isEmpty()) {
            ActivationFrame frame = stack.peek();
            SchemeObject object = frame.object;
            if (object instanceof Identifier) {
                frame.parent.evaluated.add(frame.environment.get((Identifier) object));
                stack.pop();
                continue;
            }
            if (!(object instanceof List)) {
                frame.parent.evaluated.add(object);
                stack.pop();
                continue;
            }

            List currentList = (List) object;
            Environment environment = frame.environment;
            if (frame.evaluated.isEmpty()) {
                SchemeObject head = currentList.head();
                if (head instanceof List) {
                    stack.push(new ActivationFrame(head, environment, frame));
                }
                else {
                    Procedure procedure = (Procedure) environment.get((Identifier) head);
                    frame.evaluated.add(procedure);
                }

                List arguments = currentList.tail();
                for (SchemeObject argument : arguments)
                {
                    stack.push(new ActivationFrame(argument, environment, frame));
                }
                continue;
            }

            Procedure procedure = (Procedure) frame.evaluated.head();
            List arguments = frame.evaluated.tail();
            SchemeObject result = procedure.apply(arguments, environment);

            if (frame.parent == null) {
                return result;
            }

            frame.parent.evaluated.add(result);
            stack.pop();
        }
        throw new RuntimeException("Stack underflow");
    }

    private static class ActivationFrame
    {
        public SchemeObject object;
        public Environment environment;
        public List evaluated = new List();
        public ActivationFrame parent;

        private ActivationFrame(SchemeObject object, Environment environment, ActivationFrame parent) {
            this.environment = environment;
            this.object = object;
            this.parent = parent;
        }
    }

}
