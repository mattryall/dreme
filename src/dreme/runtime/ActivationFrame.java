package dreme.runtime;

import dreme.Environment;
import dreme.List;
import dreme.Operator;
import dreme.SchemeObject;

import java.util.Iterator;

class ActivationFrame implements Iterator<SchemeObject> {
    private List rawValues;
    private List evaluatedValues = new List();
    private Environment environment;

    ActivationFrame(List list, Environment environment) {
        this.rawValues = list;
        this.environment = environment;
    }

    ActivationFrame(ActivationFrame other) {
        this.rawValues = other.rawValues;
        this.evaluatedValues = new List().addAll(other.evaluatedValues);
        this.environment = other.environment;
    }

    public boolean isNew() {
        return evaluatedValues.isEmpty();
    }

    public boolean hasNext() {
        return !isComplete();
    }

    public SchemeObject next() {
        return rawValues.get(evaluatedValues.size());
    }

    public Environment getEnvironment() {
        return environment;
    }

    public List getRawValues() {
        return rawValues;
    }

    public List getEvaluatedValues() {
        return evaluatedValues;
    }

    public void remove() {
        throw new UnsupportedOperationException("Remove not supported");
    }

    void addEvaluated(SchemeObject o) {
        if (o == null)
            throw new IllegalArgumentException("Null is not a valid evaluation result");
        if (isComplete())
            throw new IllegalStateException("Can't add evaluation result " + o + " to complete frame: " + this);
        evaluatedValues.add(o);
    }

    public boolean isComplete() {
        return evaluatedValues.size() == rawValues.size();
    }

    public SchemeObject getResult() {
        return evaluatedValues.get(evaluatedValues.size() - 1);
    }

    public Operator getOperator() {
        if (!isComplete())
            throw new IllegalStateException("Can't get operator for frame which is not fully evaluated: " + this);
        if (!(evaluatedValues.head() instanceof Operator))
            throw new IllegalStateException("Can't apply: " + evaluatedValues.head() + " in frame: " + this);
        return (Operator) evaluatedValues.head();
    }

    @Override
    public String toString() {
        StringBuffer result = new StringBuffer(500);
        result.append("[");
        for (int i=0; i<evaluatedValues.size(); i++) {
            if (i > 0) result.append(" ");
            result.append(evaluatedValues.get(i));
        }
        if (evaluatedValues.size() != rawValues.size())
            result.append(" ...");
        else
            result.append("]");
        // result.append("    ").append(environment);
        return result.toString();
    }
}
