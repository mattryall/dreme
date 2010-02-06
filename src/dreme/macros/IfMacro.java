package dreme.macros;

import dreme.*;

public class IfMacro extends PrimitiveMacro {

    public void process(List body, ExecutionContext ctx) {
        List ifProcedureList = new List();
        ifProcedureList.add(new IfProcedure(body.tail()));
        ifProcedureList.add(body.head());
        ctx.executeInPlace(ifProcedureList, ctx.getEnvironment());
    }

    private class IfProcedure extends Substitution {
        private final List body;

        private IfProcedure(List body) {
            this.body = body;
        }

        @Override
        protected List getSubstitute(ExecutionContext context) {
            SchemeObject predicate = context.evaluatedValues().head();
            SchemeObject result = (predicate != SchemeBoolean.FALSE) ? body.get(0) : body.get(1);
            if (result instanceof List) {
                return (List) result;
            }
            return new List().add(new Identifier("begin")).add(result);
        }
    }
}
