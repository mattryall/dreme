package daydreme;

import dreme.Tokens;

class List extends Pair {
    private Pair tail = null;

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
}
