package dreme.macros;

import dreme.Procedure;
import dreme.ExecutionContext;

public class BeginMacro extends Procedure {
    public void apply(ExecutionContext context) {
        context.returnLastResult();
    }
}
