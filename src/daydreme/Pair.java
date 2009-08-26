package daydreme;

class Pair implements SchemeObject {
    private SchemeObject car;
    private SchemeObject cdr;

    protected Pair() {
    }

    public Pair(SchemeObject car) {
        this.car = car;
    }

    protected Pair(Pair pair) {
        if (pair != null) {
            this.car = pair.car;
            this.cdr = pair.cdr;
        }
    }

    public Pair(SchemeObject car, SchemeObject cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public SchemeObject car() {
        return car;
    }

    public SchemeObject cdr() {
        return cdr;
    }

    protected void car(SchemeObject car) {
        if (this.car != null)
            throw new IllegalStateException("car is already set, pair is currently: " + this);
        this.car = car;
    }

    public void cdr(SchemeObject cdr) {
        if (this.cdr != null)
            throw new IllegalStateException("cdr is already set, pair is currently: " + this);
        this.cdr = cdr;
    }

    public SchemeObject evaluate(Environment environment) {
        return this;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Pair)) return false;
        Pair pair = (Pair) o;
        if (car == null ? pair.car != null : !car.equals(pair.car)) return false;
        return cdr == null ? pair.cdr == null : cdr.equals(pair.cdr);
    }

    public int hashCode() {
        int result = car != null ? car.hashCode() : 0;
        result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
        return result;
    }

    public String toString() {
        return "(" + car + " . " + cdr + ")";
    }
}
