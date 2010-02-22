package dreme.macros;

import dreme.*;

abstract class BindingMacro extends PrimitiveMacro {
    public void process(List body, ExecutionContext ctx) {
        ctx.addResult(new Procedure() {
            protected SchemeObject apply(List arguments, Environment environment) {
                if (!(arguments.head() instanceof Identifier))
                    throw new IllegalArgumentException("Bad variable " + arguments.head());
                bind(environment, (Identifier) arguments.head(), arguments.tail().head());

                return Unspecified.INSTANCE;
            }
        });
        ctx.addResult(body.head()); // skip evaluation of the identifier
    }

    protected abstract void bind(Environment environment, Identifier identifier, SchemeObject value);
}
