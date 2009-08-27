package daydreme;

abstract class Procedure implements SchemeObject {
    public SchemeObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public String toString() {
        return "#<procedure>";
    }

    abstract SchemeObject apply(List arguments, Environment environment);
}
