package daydreme;

import static daydreme.List.toList;

public class PatternMatcher {
    private final List pattern;
    private final List template;

    public PatternMatcher(List pattern, List template) {
        this.pattern = pattern;
        this.template = template;
    }

    public boolean matches(List input) {
        return matches(pattern, input);
    }

    private boolean matches(List pattern, List input) {
        if (pattern.isEmpty())
            return input.isEmpty();
        if (input.isEmpty())
            return false;
        return matches(pattern.get(0), input.get(0)) &&
            matches(pattern.tail(), input.tail());
    }

    public List apply(List input) {
        return null;
    }

    private boolean matches(SchemeObject patternComponent, SchemeObject input) {
        if (patternComponent instanceof Pair)
            return matches(toList(patternComponent), toList(input));
        // we just have an identifier, so check that input isn't null
        return input != null;
    }
}
