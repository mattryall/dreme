package daydreme;

import java.util.Iterator;
import java.util.NoSuchElementException;

class List extends Pair implements Iterable<SchemeObject> {
    private Pair lastPair = null;

    static List toList(SchemeObject list) {
        return new List((Pair) list);
    }

    List() {
    }

    List(SchemeObject head, List tail)
    {
        this(new Pair(head, tail));
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
        return toList(cdr());
    }

    public SchemeObject head() {
        return car();
    }

    public List addTerminal(SchemeObject o) {
        if (lastPair == null)
            throw new IllegalStateException("Cannot add terminal entry to an empty list");
        lastPair.cdr(o);
        return this;
    }

    public Pair lastPair() {
        return lastPair;
    }

    public SchemeObject evaluate(Environment environment) {
        if (car().equals(new Identifier("define-syntax"))) {
            Identifier name = (Identifier) tail().get(0);
            List transformer = tail().tail();
            environment.addTransformer(name, transformer);
            return SchemeObject.UNSPECIFIED;
        }

        if (car().equals(new Identifier("syntax-rules"))) {
            List literals = toList(tail().get(0));
            List clauses = tail().tail();
            return new SyntaxRules(literals, clauses);
        }

        if (car().equals(new Identifier("lambda"))) {
            SchemeObject formals = tail().get(0);
            List body = tail().tail();
            return new Lambda(formals, body, environment);
        }

        if (car().equals(new Identifier("quote"))) {
            return tail().get(0); // unevaluated
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

    public boolean isEmpty()
    {
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

    public boolean isProper() {
        return lastPair == null || lastPair.cdr() == null;
    }
}
