package daydreme;

import dreme.Tokens;

import java.util.Iterator;
import java.util.NoSuchElementException;

class List extends Pair implements Iterable<SchemeObject> {
    private Pair tail = null;

    List() {
    }

    List(Pair pair) {
        super(pair);
        tail = this;
        while (tail.cdr() instanceof Pair)
            tail = (Pair) tail.cdr();
    }

    public List add(SchemeObject o) {
        if (car() == null) {
            car(o);
            tail = this;
        }
        else {
            Pair newPair = new Pair(o);
            tail.cdr(newPair);
            tail = newPair;
        }
        return this;
    }

    public List addTerminal(SchemeObject o) {
        if (tail == null)
            throw new IllegalStateException("Cannot add terminal entry to an empty list");
        tail.cdr(o);
        return this;
    }

    public List addTerminal(Tokens.Token token) {
        return addTerminal(toSchemeObject(token));
    }

    public List add(Tokens.Token token) {
        return add(toSchemeObject(token));
    }

    private SchemeObject toSchemeObject(Tokens.Token token) {
        if (token instanceof Tokens.BareWord) {
            return new Identifier(((Tokens.BareWord) token).getValue());
        }
        else if (token instanceof Tokens.Integer) {
            return new Integer(((Tokens.Integer) token).getInt());
        }
        else if (token instanceof Tokens.Decimal) {
            return new Decimal(((Tokens.Decimal) token).getDouble());
        }
        throw new IllegalArgumentException("Unknown token type: " + token);
    }

    public SchemeObject evaluate(Environment environment) {
        if (car().equals(new Identifier("lambda"))) {
            List everything = new List((Pair) cdr());
            List formals = new List((Pair) everything.get(0));
            List body = new List((Pair) everything.get(1));
            return new Lambda(formals, body, environment);
        }

        SchemeObject proc = car().evaluate(environment);
        if (!(proc instanceof Procedure))
            throw new IllegalArgumentException("Wrong type to apply: " + proc);
        return ((Procedure) proc).apply((Pair) cdr(), environment);
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
