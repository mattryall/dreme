package daydreme.macros;

import daydreme.*;

abstract class BindingMacro extends PrimitiveMacro {
    public void process(List body, ExecutionContext ctx) {
        ctx.addResult(new Procedure() {
            @Override
            public SchemeObject apply(List arguments, Environment scope) {
                if (!(arguments.head() instanceof Identifier))
                    throw new IllegalArgumentException("Bad variable " + arguments.head());
                bind(scope, (Identifier) arguments.head(), arguments.tail().head());
                return SchemeObject.UNSPECIFIED;
            }
        });
        ctx.skip(); // skip evaluation of the identifier
    }

    protected abstract void bind(Environment environment, Identifier identifier, SchemeObject value);
}
