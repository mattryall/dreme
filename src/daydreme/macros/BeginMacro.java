package daydreme.macros;

import daydreme.Procedure;
import daydreme.ExecutionContext;

public class BeginMacro extends Procedure {
    public void apply(ExecutionContext context) {
        context.returnLastResult();
    }
}
