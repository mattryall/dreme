package daydreme.macros;

import daydreme.List;
import daydreme.ExecutionContext;

public class QuoteMacro extends PrimitiveMacro {
    public void process(List body, ExecutionContext ctx) {
        if (body.size() > 1)
            throw new IllegalStateException("Extra expression in quote: " + body);
        ctx.returnValue(body.head());
    }
}
