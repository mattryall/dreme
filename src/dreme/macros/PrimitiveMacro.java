package dreme.macros;

import dreme.Macro;

public abstract class PrimitiveMacro implements Macro {
    public String toString() {
        return "primitive-macro " + this.getClass().getName();
    }
}
