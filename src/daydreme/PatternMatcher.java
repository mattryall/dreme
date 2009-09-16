package daydreme;

import static daydreme.List.toList;

import java.util.Map;
import java.util.HashMap;

public class PatternMatcher {
    private final List pattern;
    private final List template;

    public PatternMatcher(List pattern, List template) {
        this.pattern = pattern;
        this.template = template;
    }

    public boolean matches(List input) {
        return matches(pattern, input, new HashMap<Identifier, SchemeObject>());
    }

    private boolean matches(List pattern, List input, Map<Identifier, SchemeObject> captures) {
        if (pattern.isEmpty())
            return input.isEmpty();
        if (pattern.size() >= 2 && pattern.get(1) == Ellipsis.INSTANCE) {
            captures.put((Identifier) pattern.get(0), input);
            return true;
        }
        if (input.isEmpty())
            return false;
        return matches(pattern.get(0), input.get(0), captures) &&
            matches(pattern.tail(), input.tail(), captures);
    }

    public Map<Identifier, SchemeObject> capture(List input)
    {
        HashMap<Identifier, SchemeObject> captures = new HashMap<Identifier, SchemeObject>();
        if (!matches(pattern, input, captures))
            return null;
        return captures;
    }

    public List apply(List input) {
        HashMap<Identifier, SchemeObject> captures = new HashMap<Identifier, SchemeObject>();
        if (!matches(pattern, input, captures))
            throw new IllegalArgumentException("Trying to apply non-matching syntax transformer to: " + input);

        return doApply(captures, template);
    }

    private List doApply(HashMap<Identifier, SchemeObject> captures, List template)
    {
        SchemeObject head = template.head();
        List result = new List();
        List rest = template.tail();

        if (head instanceof Identifier && captures.containsKey((Identifier) head))
        {
            if (template.size() > 1 && template.get(1) == Ellipsis.INSTANCE) {
                result.addAll((List) captures.get((Identifier) head));
                rest = toList(((Pair) template.cdr()).cdr());
            }
            else {
                result.add(captures.get((Identifier) head));
            }
        }
        else if (head instanceof List)
            result.add(doApply(captures, (List) head));
        else
            result.add(head);

        if (rest.isEmpty())
            return result;

        result.addAll(doApply(captures, rest));
        return result;
    }

    private boolean matches(SchemeObject patternComponent, SchemeObject input, Map<Identifier, SchemeObject> captures) {
        if (patternComponent instanceof Pair)
            return matches(toList(patternComponent), toList(input), captures);

        // if corresponding input doesn't exist, the pattern doesn't match
        if (input == null)
            return false;

        captures.put((Identifier) patternComponent, input);
        return true;
    }
}
