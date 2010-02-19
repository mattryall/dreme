package dreme.runtime;

import dreme.Operator;

public abstract class PrimitiveOperator implements Operator {
    public String toString() {
        return "#<primitive-builtin-operator! " + this.getClass().getSimpleName() + ">";
    }
}
