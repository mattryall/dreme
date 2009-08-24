package daydreme;

import java.util.Map;
import java.util.HashMap;

class Environment implements Cloneable {
    private Map<Identifier, SchemeObject> bindings = new HashMap<Identifier, SchemeObject>();

    Environment() {
    }

    Environment(NamedProcedure... procedures) {
        for (NamedProcedure procedure : procedures) {
            define(procedure.getName(), procedure);
        }
    }

    public void let(Identifier name, SchemeObject value) {
        bindings.put(name, value);
    }

    public void define(Identifier name, SchemeObject value) {
        bindings.put(name, value);
    }

    public SchemeObject get(Identifier name) {
        return bindings.get(name);
    }

    public Environment copy() {
        Environment result = new Environment();
        result.bindings = new HashMap<Identifier, SchemeObject>(bindings);
        return result;
    }

    public boolean contains(Identifier identifier) {
        return bindings.containsKey(identifier);
    }
}
