package dreme.macros.interop;

import dreme.*;
import dreme.macros.AbstractMacro;

public final class MethodInvocationMacro extends AbstractMacro {
    public void process(List body, ExecutionContext ctx) {
        // avoid evaluation of method name
        SchemeObject instance = body.head();
        String methodName = toMethodName(body.tail().head());
        List arguments = body.tail().tail();

        List replacement = new List();
        replacement.add(new MethodInvocation(methodName));
        replacement.add(instance);
        replacement.addAll(arguments);
        ctx.executeInPlace(replacement, ctx.getEnvironment());
    }

    private String toMethodName(SchemeObject object) {
        NameResolvingVisitor visitor = new NameResolvingVisitor();
        object.acceptVisitor(visitor);
        return visitor.getName();
    }

    private static class MethodInvocation extends Procedure {
        private final String methodName;

        public MethodInvocation(String methodName) {
            super("." + methodName);
            this.methodName = methodName;
        }

        protected SchemeObject apply(List arguments, Environment environment) {
            SchemeObject instance = arguments.head();
            return ConversionUtils.invokeMethod(instance, methodName, arguments.tail());
        }
    }
}
