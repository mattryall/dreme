package daydreme.macros;

import daydreme.List;
import daydreme.ExecutionContext;

public class LambdaMacro extends PrimitiveMacro {
    public void process(List body, ExecutionContext ctx) {
        ctx.returnValue(new Lambda(body, ctx.getEnvironment()));
    }
}
