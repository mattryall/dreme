package dreme.macros;

import dreme.Macro;
import dreme.SchemeObjectVisitor;

abstract class AbstractMacro implements Macro {
    public String toString() {
        return "#<primitive-builtin-macro! " + this.getClass().getSimpleName() + ">";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.macro(this);
    }
}
