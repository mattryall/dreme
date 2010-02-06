package dreme.macros;

import dreme.Macro;
import dreme.ExecutionContext;

public abstract class PrimitiveMacro implements Macro {
    public String toString() {
        return "primitive-macro " + this.getClass().getName();
    }

    public void evaluate(ExecutionContext ctx) {
        ctx.addResult(this);
    }
}
