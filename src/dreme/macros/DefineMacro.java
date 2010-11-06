package dreme.macros;

import dreme.*;

public class DefineMacro extends AbstractMacro {

    public void process(List body, ExecutionContext ctx) {
        if (body.head() instanceof List) {
            List arguments = (List) body.head();
            processFunctionDef((Identifier) arguments.head(), arguments.tail(), body.tail(), ctx);
        } else {
            processVariableDef(body, ctx);
        }
    }

    private void processFunctionDef(Identifier identifier, List lambdaFormals, List lambdaBody, ExecutionContext ctx) {
        Lambda lambda = new Lambda(identifier.getName(), lambdaFormals, lambdaBody, ctx.getEnvironment());
        ctx.getEnvironment().define(identifier, lambda);
        ctx.returnValue(Unspecified.INSTANCE);
    }

    private void processVariableDef(List body, ExecutionContext ctx) {
        new BindingMacro() {
            protected void bind(Environment environment, Identifier identifier, SchemeObject value) {
                environment.define(identifier, value);
            }
        }.process(body, ctx);
    }
}
