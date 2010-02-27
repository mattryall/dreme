package dreme.macros;

import dreme.List;
import dreme.ExecutionContext;

public class LambdaMacro extends AbstractMacro {
    public void process(List body, ExecutionContext ctx) {
        ctx.returnValue(new Lambda(body, ctx.getEnvironment()));
    }
}
