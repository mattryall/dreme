package dreme.macros;

import dreme.Macro;
import dreme.SchemeObjectVisitor;

public abstract class PrimitiveMacro implements Macro {
    public String toString() {
        return "#<primitive-builtin-macro! " + this.getClass().getSimpleName() + ">";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.macro(this);
    }
}
