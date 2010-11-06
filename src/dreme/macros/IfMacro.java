package dreme.macros;

import dreme.*;

public class IfMacro extends AbstractMacro {

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
            SchemeObject result;

            if (predicate != SchemeBoolean.FALSE) { // all values other than #f are considered true
                result = body.get(0);
            }
            else if (body.size() > 1) {
                result = body.get(1);
            }
            else {
                return null;
            }

            if (result instanceof List) {
                return (List) result;
            }
            return new List().add(new Identifier("begin")).add(result);
        }
    }
}
