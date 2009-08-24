package dreme;

import java.util.Map;

class Cons
{
    Object car;
    Object cdr;

    public Cons()
    {
        this(null, null);
    }

    public Cons(Object car)
    {
        this(car, null);
    }

    public Cons(Object car, Object cdr)
    {
        this.car = car;
        this.cdr = cdr;
    }

    public void assign(Object o)
    {
        if (car == null)
            car = o;
        else
            cdr = o;
    }

    public Cons append(Object o)
    {
        if (this.car == null) { // we're the first entry in an empty list
            this.car = o;
            return this;
        }
        return (Cons) (this.cdr = new Cons(o));
    }

    public Cons evaluate(Map<Tokens.BareWord, Evaluator.Function> environment)
    {
        return environment.get((Tokens.BareWord) car).apply((Cons) cdr, environment);
    }

    public String toString()
    {
        return "(" + car + " . " + (cdr == null ? "()" : cdr) + ")";
    }

    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Cons cons = (Cons) o;

        if (car != null ? !car.equals(cons.car) : cons.car != null) return false;
        return !(cdr != null ? !cdr.equals(cons.cdr) : cons.cdr != null);
    }

    public int hashCode()
    {
        int result = car != null ? car.hashCode() : 0;
        result = 31 * result + (cdr != null ? cdr.hashCode() : 0);
        return result;
    }
}
