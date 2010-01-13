package daydreme.macros;

import daydreme.*;

public class IfMacro extends PrimitiveMacro {

    public void process(List body, ExecutionContext ctx) {
        List ifProcedureList = new List();
        ifProcedureList.add(new IfProcedure(body.tail()));
        ifProcedureList.add(body.head());
        ctx.executeInPlace(ifProcedureList, ctx.getEnvironment());
    }

    private class IfProcedure extends Procedure {
        private final List body;

        private IfProcedure(List body) {
            this.body = body;
        }

        public void apply(ExecutionContext context) {
            SchemeObject predicate = context.evaluatedValues().head();
            SchemeObject result = (predicate != SchemeBoolean.FALSE) ? body.get(0) : body.get(1);
            if (result instanceof List)
                context.executeInPlace((List) result, context.getEnvironment());
            else
                context.returnValue(result);
        }
    }
}
