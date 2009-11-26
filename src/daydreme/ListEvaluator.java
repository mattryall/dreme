package daydreme;

import java.util.Stack;
import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;

public class ListEvaluator {

	static interface Macro extends SchemeObject {
		void process(List body, ExecutionContext ctx);
	}

	static abstract class PrimitiveMacro implements Macro {
		public String toString() {
			return "primitive-macro " + this.getClass().getName();
		}
		
		public SchemeObject evaluate(ExecutionContext ctx) {
			if (ctx.isHeadPosition()) {
				process(ctx.getRawBody(), ctx);
			}

			return this;
		}

		public SchemeObject evaluate(Environment env) {
			return this;
		}

		public SchemeObject apply(ExecutionContext ctx) {
			throw new RuntimeException("Cannot apply type");
		}
	}

	static class LambdaMacro extends PrimitiveMacro {
		public void process(List body, ExecutionContext ctx) {
			ctx.returnValue(new Lambda(body, ctx.getEnvironment()));
		}
	}

	static class DefineMacro extends Procedure implements Macro {
		public void process(List body, ExecutionContext ctx) {
			ctx.addResult(this);
			ctx.skip();
		}

		public SchemeObject apply(List arguments, Environment scope) { 
			if (!(arguments.head() instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + arguments.head());
            scope.define((Identifier) arguments.head(), arguments.tail().head());
            return SchemeObject.UNSPECIFIED;
		}
		
		public SchemeObject evaluate(ExecutionContext ctx) {
			if (ctx.isHeadPosition()) {
				process(ctx.getRawBody(), ctx);
			}

			return this;
		}

		public String toString() {
			return "primitive-macro " + this.getClass().getName();
		}
				
		public SchemeObject evaluate(Environment env) {
			return this;
		}
	}

	static class BeginMacro extends PrimitiveMacro {
		public void process(List body, ExecutionContext ctx) {
			// FIXME: This needs to turn into a lambda thunk so that the last value is returned
			ctx.skip();
		}
	}

    public SchemeObject evaluate(List list, Environment environment) {
        SchemeStack callStack = new SchemeStack();

		environment.define(new Identifier("lambda"), new LambdaMacro());
		environment.define(new Identifier("define"), new DefineMacro());
		environment.define(new Identifier("begin"), new BeginMacro());

		// System.out.println("Evaluating list " + list);
        callStack.push(new ActivationFrame(list, environment));
        evaluate(callStack);
        return callStack.getLastResult();
    }

    private static void evaluate(SchemeStack stack) {
        while (!stack.isEmpty()) {
            // System.out.println("\nCurrent stack:\n" + stack);
            ActivationFrame frame = stack.peek();
            Environment environment = frame.environment;

			ExecutionContext ctx = new ListEvaluatorExecutionContext(stack, environment);

            if (frame.hasNext()) {
				SchemeObject nextObject = frame.next();
				nextObject.evaluate(ctx);
            }
			else {
				// all the arguments are evaluated, let's run the procedure
				frame.getOperator().apply(ctx);
			}
        }
    }

	private static class ListEvaluatorExecutionContext implements ExecutionContext {
		private final SchemeStack stack;
		private final Environment environment;
		private boolean inMacroExpansion = false;

		public ListEvaluatorExecutionContext(SchemeStack stack, Environment environment) {
			this.stack = stack;
			this.environment = environment;
		}

		public boolean isHeadPosition() {
			return stack.peek().isNew();
		}

		public List getRawBody() {
			return stack.peek().rawValues.tail();
		}

		public Environment getEnvironment() {
			return environment;
		}

		public void returnLastResult() {
			returnValue(stack.getLastResult());
		}

		public void returnValue(SchemeObject returnValue) {
			stack.returnValue(returnValue);
		}

		public void skip() {
			addResult(stack.peek().next());
		}

		public List arguments() {
			return evaluatedValues();
		}

		public List evaluatedValues() {
			return stack.peek().evaluatedValues.tail();
		}

		public void addResult(SchemeObject result) {
			// DIRTY HACK!
			if (result instanceof Macro && isHeadPosition() &&!inMacroExpansion) {
				inMacroExpansion = true;
				result.evaluate(this);
				inMacroExpansion = false;
			}
			else {
				stack.peek().addEvaluated(result);
			}
		}

		public void execute(List executable, Environment environment) {
			// returnValue(executable.evaluate(environment)); NON-STACK BASED!
			stack.pushFrame(executable, environment);
		}

		public void executeInPlace(List executable, Environment environment) {
			stack.pop();
			execute(executable, environment);
		}
	}

    private static class ActivationFrame implements Iterator<SchemeObject> {
        private List rawValues;
        private List evaluatedValues = new List();
		private boolean complete;
        private Environment environment;

        private ActivationFrame(List list, Environment environment) {
            this.rawValues = list;
            this.environment = environment;
			this.complete = false;
        }

		public boolean isNew() {
			return evaluatedValues.isEmpty();
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
            return evaluatedValues.size() == rawValues.size() || complete;
        }

		public void setComplete() {
			this.complete = true;
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

		public void pushFrame(List executable, Environment environment) {
			push(new ActivationFrame(executable, environment));
		}

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
