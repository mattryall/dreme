package daydreme;

class Decimal implements SchemeObject {
    private final double value;

    public Decimal(double value) {
        this.value = value;
    }

    public double getValue() {
        return value;
    }

    public SchemeObject evaluate(Environment environment) {
        return this;
    }
}
