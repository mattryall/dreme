package dreme;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class List extends Pair implements Iterable<SchemeObject>, Evaluatable {
    private Pair lastPair = null;
    private boolean dotLast = false;

    public static List toList(SchemeObject list) {
        return new List((Pair) list);
    }

    public List() {
    }

    public List(SchemeObject head, List tail) {
        this(new Pair(head, tail));
    }

	public void evaluate(ExecutionContext context) {
		context.execute(this, context.getEnvironment());
	}

    private List(Pair pair) {
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
            if (dotLast) {
                addTerminal(o);
            }
            else {
                Pair newPair = new Pair(o);
                lastPair.cdr(newPair);
                lastPair = newPair;
            }
        }
        return this;
    }

    /**
     * @return everything in this list except the first element
     */
    public List tail() {
        return toList(cdr());
    }

    public SchemeObject head() {
        return car();
    }

    public List addTerminal(SchemeObject o) {
        if (lastPair == null)
            throw new IllegalStateException("Cannot add terminal entry to an empty list");
        lastPair.cdr(o);
        dotLast = false;
        return this;
    }

    public Pair lastPair() {
        return lastPair;
    }

    public String toString() {
        StringBuffer result = new StringBuffer();
        result.append("(");
        Pair current = this;
        while (current != null && current.car() != null) {
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

    public boolean isEmpty() {
        return size() == 0;
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

    public List addAll(List list) {
        for (SchemeObject item : list) {
            add(item);
        }
        return this;
    }
    
    public void dot() {
        if (dotLast)
            throw new IllegalStateException("Cannot have consecutive dot operators");
        dotLast = true;
    }

    public boolean isProper() {
        return lastPair == null || lastPair.cdr() == null;
    }

    public void close() {
        if (dotLast)
            throw new IllegalStateException("Unexpected close of list. Value should follow dot.");
    }
}
