package daydreme;

import java.util.HashMap;
import java.util.Map;

/**
 * A multimap that you have to shift() to get to successive elements of a particular key.
 */
public class Captures {
    final Map<Identifier, List> data;

    public Captures() {
        this.data = new HashMap<Identifier, List>();
    }

    private Captures(Map<Identifier, List> data) {
        this.data = data;
    }

    public void put(Identifier identifier, SchemeObject object)
    {
        if (!data.containsKey(identifier)) {
            data.put(identifier, new List());
        }
        data.get(identifier).add(object);
    }

    public SchemeObject get(Identifier identifier)
    {
        if (!data.containsKey(identifier))
            throw new IllegalStateException("Identifier not captured: " + identifier);

        return data.get(identifier).head();
    }

    public List getAll(Identifier identifier) {
        if (!data.containsKey(identifier))
            throw new IllegalStateException("Identifier not captured: " + identifier);

        return data.get(identifier);
    }

    public Captures shift() {
        Map<Identifier, List> shiftedData = new HashMap<Identifier, List>(data.size());
        for (Map.Entry<Identifier, List> entry : data.entrySet()) {
            shiftedData.put(entry.getKey(), entry.getValue().tail());
        }
        return new Captures(shiftedData);
    }

    public boolean containsKey(Identifier identifier) {
        return data.containsKey(identifier);
    }

    public String toString() {
        return data.toString();
    }

    public Captures copy() {
        return new Captures(data);
    }
}
