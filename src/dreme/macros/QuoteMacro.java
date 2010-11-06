package dreme.macros;

import dreme.List;
import dreme.ExecutionContext;

public class QuoteMacro extends AbstractMacro {
    public void process(List body, ExecutionContext ctx) {
        if (body.size() > 1)
            throw new IllegalStateException("Extra expression in quote: " + body);
        ctx.returnValue(body.head());
    }
}
