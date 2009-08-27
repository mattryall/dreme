package daydreme;

import java.util.Iterator;
import java.util.NoSuchElementException;

class List extends Pair implements Iterable<SchemeObject> {
    private Pair lastPair = null;

    List() {
    }

    List(Pair pair) {
        super(pair);
        lastPair = this;
        while (lastPair.cdr() instanceof Pair)
            lastPair = (Pair) lastPair.cdr();
    }

    public List add(SchemeObject o) {
        if (car() == null) {
            car(o);
            lastPair = this;
        }
        else {
            Pair newPair = new Pair(o);
            lastPair.cdr(newPair);
            lastPair = newPair;
        }
        return this;
    }

    /**
     * @return everything in this list except the first element
     */
    public List tail() {
        return new List((Pair) cdr());
    }

    public List addTerminal(SchemeObject o) {
        if (lastPair == null)
            throw new IllegalStateException("Cannot add terminal entry to an empty list");
        lastPair.cdr(o);
        return this;
    }

    public SchemeObject evaluate(Environment environment) {
        if (car().equals(new Identifier("lambda"))) {
            List formals = new List((Pair) tail().get(0));
            List body = new List((Pair) tail().get(1));
            return new Lambda(formals, body, environment);
        }

        SchemeObject proc = car().evaluate(environment);
        if (!(proc instanceof Procedure))
            throw new IllegalArgumentException("Wrong type to apply: " + proc);
        return ((Procedure) proc).apply(tail(), environment);
    }

    public String toString() {
        StringBuffer result = new StringBuffer();
        result.append("(");
        Pair current = this;
        while (current != null) {
            result.append(current.car());
            if (current.cdr() != null && !(current.cdr() instanceof Pair)) {
                result.append(" . ").append(current.cdr());
                break;
            }
            if (current.cdr() != null)
                result.append(" ");
            current = (Pair) current.cdr();
        }
        result.append(")");
        return result.toString();
    }

    public Iterator<SchemeObject> iterator() {
        return new Iterator<SchemeObject>() {
            private SchemeObject next = List.this;

            public boolean hasNext() {
                return next instanceof Pair && ((Pair) next).car() != null;
            }

            public SchemeObject next() {
                if (!hasNext())
                    throw new NoSuchElementException();
                Pair current = (Pair) next;
                next = current.cdr();
                return current.car();
            }

            public void remove() {
                throw new UnsupportedOperationException("Cannot remove items from list");
            }
        };
    }

    public int size() {
        int result = 0;
        for (Iterator iter = iterator(); iter.hasNext(); iter.next()) {
            result++;
        }
        return result;
    }

    public SchemeObject get(int index) {
        if (index < 0)
            throw new IndexOutOfBoundsException("Index cannot be negative: " + index);
        int i = 0;
        for (SchemeObject o : this) {
            if (i == index) return o;
            i++;
        }
        throw new IndexOutOfBoundsException("Index greater than length of list: " + index);
    }
}
