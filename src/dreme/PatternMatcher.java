package dreme;

import org.apache.log4j.Logger;

import java.util.HashSet;
import java.util.Set;

import static dreme.List.toList;

public class PatternMatcher {
    private static final Logger log = Logger.getLogger(PatternMatcher.class);

    private final List pattern;
    private final Set<Identifier> literals = new HashSet<Identifier>();

    public PatternMatcher(List pattern, List literals) {
        this.pattern = pattern;
        for (SchemeObject literal : literals) {
            if (!(literal instanceof Identifier))
                throw new IllegalArgumentException("Literal must be identifier: " + literal);
            this.literals.add((Identifier) literal);
        }
    }

    public boolean matches(List input) {
        return matches(pattern, input, new Captures());
    }

    private boolean matches(List pattern, List input, Captures captures) {
        if (log.isDebugEnabled())
            log.debug("Matching pattern: " + pattern + " to input " + input);
        if (pattern.isEmpty())
            return input.isEmpty();
        if (pattern.size() == 2 && pattern.get(1) == Ellipsis.INSTANCE) {
            if (input.isEmpty()) return true;

            if (log.isDebugEnabled())
                log.debug("Ellipsis on pattern " + pattern.head() + " attempting to match against: " + input.get(0));
            return subPatternMatches(pattern.head(), input.head(), captures) &&
                matches(pattern, input.tail(), captures);
        }
        if (input.isEmpty())
            return false;
        if (pattern.size() >= 2 && pattern.head() instanceof Identifier && pattern.get(1) == Ellipsis.INSTANCE) {
            captures.put((Identifier) pattern.head(), input);
            return true;
        }
        return subPatternMatches(pattern.head(), input.head(), captures) &&
            matches(pattern.tail(), input.tail(), captures);
    }

    public Captures capture(List input)
    {
        Captures captures = new Captures();
        if (!matches(pattern, input, captures))
            return null;
        return captures;
    }

    public List apply(List input, List template) {
        Captures captures = new Captures();
        if (!matches(pattern, input, captures))
            throw new IllegalArgumentException("Trying to apply non-matching syntax transformer to: " + input);

        return doApply(captures, template);
    }

    private List doApply(Captures captures, List template)
    {
        if (log.isDebugEnabled())
            log.debug("Applying template " + template + " with captures: " + captures);
        SchemeObject head = template.head();
        List result = new List();
        List rest = template.tail();

        if (rest.head() == Ellipsis.INSTANCE) {
            result.addAll(applyEllipsis(head, captures));
            rest = rest.tail();
        }
        else if (head instanceof Identifier && captures.containsKey((Identifier) head)) {
            result.add(captures.get((Identifier) head));
        }
        else if (head instanceof List) {
            result.add(doApply(captures, (List) head));
        }
        else {
            result.add(head);
        }

        if (!rest.isEmpty()) {
            result.addAll(doApply(captures, rest)); // recurse on tail
        }

        if (log.isDebugEnabled())
            log.debug("Application result: " + result);
        return result;
    }

    private List applyEllipsis(SchemeObject subPattern, Captures captures) {
        List result = new List();
        Captures tempCaptures = captures.copy();
        if (subPattern instanceof Identifier) {
            Identifier identifier = (Identifier) subPattern;
            while (tempCaptures.containsKey(identifier) && tempCaptures.get(identifier) != null) {
                result.add(tempCaptures.get(identifier));
                tempCaptures = tempCaptures.shift();
            }
        }
        else if (subPattern instanceof List) {
            Set<Identifier> variables = new HashSet<Identifier>();
            collectVariables((List) subPattern, captures, variables);
            ELLIPSIS: while (true) {
                result.add(doApply(tempCaptures, (List) subPattern));
                tempCaptures = tempCaptures.shift();
                for (Identifier variable : variables) {
                    if (tempCaptures.get(variable) == null)
                        break ELLIPSIS;
                }
            }
        }
        return result;
    }

    private void collectVariables(List template, Captures captures, Set<Identifier> variables) {
        if (template.isEmpty()) return;

        if (template.head() instanceof Identifier) {
            Identifier identifier = (Identifier) template.head();
            if (captures.containsKey(identifier)) {
                variables.add(identifier);
            }
        }
        else if (template.head() instanceof List) {
            collectVariables((List) template.head(), captures, variables);
        }

        collectVariables(template.tail(), captures, variables);
    }

    private boolean subPatternMatches(SchemeObject subPattern, SchemeObject input, Captures captures) {
        if (log.isDebugEnabled())
            log.debug("Matching sub-pattern: " + subPattern + " to input: " + input);

        if (subPattern instanceof Pair) {
            if (!(input instanceof Pair)) {
                return false; // pattern was a pair but the input wasn't
            }
            return matches(toList(subPattern), toList(input), captures);
        }

        // if corresponding input doesn't exist, the pattern doesn't match
        if (input == null)
            return false;

        // if the pattern is a literal or string, the input must be the same or it is not a match
        if (literals.contains(subPattern) || subPattern instanceof SchemeString)
            return input.equals(subPattern);

        captures.put((Identifier) subPattern, input);
        if (log.isDebugEnabled())
            log.debug("Bound '" + subPattern + "' to input value: " + input);

        return true;
    }
}
