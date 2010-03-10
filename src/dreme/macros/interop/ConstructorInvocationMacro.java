package dreme.macros.interop;

import dreme.*;
import dreme.macros.AbstractMacro;

public final class ConstructorInvocationMacro extends AbstractMacro {
    public void process(List body, ExecutionContext ctx) {
        // avoid evaluation of class name
        String className = toClassName(body.head());
        List replacement = new List(new ConstructorInvocation(className), body.tail());
        ctx.executeInPlace(replacement, ctx.getEnvironment());
    }

    private String toClassName(SchemeObject object) {
        NameResolvingVisitor visitor = new NameResolvingVisitor();
        object.acceptVisitor(visitor);
        return visitor.getName();
    }

    private static class ConstructorInvocation extends Procedure {
        private final String className;

        public ConstructorInvocation(String className) {
            super(className + ".");
            this.className = className;
        }

        protected SchemeObject apply(List arguments, Environment environment) {
            return ConversionUtils.invokeConstructor(className, arguments);
        }
    }
}
