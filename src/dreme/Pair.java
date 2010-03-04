package dreme;

public class Pair implements SchemeObject {
    private SchemeObject car;
    private SchemeObject cdr;

    protected Pair() {
    }

    public Pair(SchemeObject car) {
        car(car);
    }

    protected Pair(Pair pair) {
        if (pair != null) {
            car(pair.car);
            cdr(pair.cdr);
        }
    }

    public Pair(SchemeObject car, SchemeObject cdr) {
        car(car);
        cdr(cdr);
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

    public boolean isEmpty() {
        return car == null && cdr == null;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Pair)) return false;
        Pair pair = (Pair) o;
        if (car == null ? pair.car != null : !car.equals(pair.car)) {
            return false;
        }

        // treat cdr null and cdr empty list as equivalent
        if (cdr == null || (cdr instanceof Pair && ((Pair) cdr).isEmpty())) {
            return pair.cdr == null ||
                (pair.cdr instanceof Pair && ((Pair) pair.cdr).isEmpty());
        }
        else {
            return cdr.equals(pair.cdr);
        }
    }

    public int hashCode() {
        int result = car != null ? car.hashCode() : 0;
        result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
        return result;
    }

    public String toString() {
        if (cdr == null || cdr instanceof Pair) {
            return List.toList(this).toString();
        }
        return "(" + car + " . " + cdr + ")";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.pair(this);
    }
}
