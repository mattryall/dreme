package daydreme;

import java.util.Map;
import java.util.HashMap;

class Environment implements Cloneable {
    private Environment parent = null;
    private Map<Identifier, SchemeObject> bindings = new HashMap<Identifier, SchemeObject>();

    Environment() {
    }

    private Environment(Environment parent) {
        this.parent = parent;
    }

    Environment(NamedProcedure... procedures) {
        for (NamedProcedure procedure : procedures) {
            define(procedure.getName(), procedure);
        }
    }

    public void let(SchemeObject var, SchemeObject rawValue) {
        bind(var, rawValue.evaluate(parent));
    }

    public void letrec(SchemeObject var, SchemeObject rawValue) {
        bind(var, rawValue.evaluate(this));
    }

    private void bind(SchemeObject var, SchemeObject val) {
        if (!(var instanceof Identifier))
            throw new IllegalArgumentException("Invalid binding: " + var);
        bindings.put((Identifier) var, val);
    }

    public void define(Identifier name, SchemeObject value) {
        Environment topLevel = this;
        while (topLevel.parent != null)
            topLevel = topLevel.parent;
        topLevel.bindings.put(name, value);
    }

    public SchemeObject get(Identifier name) {
        if (bindings.containsKey(name))
            return bindings.get(name);
        return parent != null ? parent.get(name) : null;
    }

    public Environment copy() {
        return new Environment(this);
    }

    public boolean contains(Identifier identifier) {
        return bindings.containsKey(identifier) ||
            (parent != null && parent.contains(identifier));
    }

    public void addTransformer(Identifier name, SchemeObject transformer) {
        bindings.put(name, transformer.evaluate(this));
    }
}
