package daydreme;

import static daydreme.List.toList;

class SyntaxRules extends Procedure {
    private final List literals;
    private final List clauses;

    public SyntaxRules(List literals, List clauses) {
        if (!literals.isEmpty())
            throw new IllegalArgumentException("Literals must be empty at the moment sorry");

        this.literals = literals;
        this.clauses = clauses;
    }

    SchemeObject apply(List /* unused */ arguments, Environment /* unused */ environment) {
        return new Procedure() {
            SchemeObject apply(List arguments, Environment environment) {
                for (SchemeObject t : clauses) {
                    List clause = toList(t);
                    List pattern = toList(clause.get(0)).tail();
                    List template = clause.tail();
                    final PatternMatcher matcher = new PatternMatcher(pattern, template);
                    if (matcher.matches(arguments)) {
                        SchemeObject transformed = matcher.apply(arguments).get(0);
                        return transformed.evaluate(environment);
                    }
                }
                throw new IllegalArgumentException("Malformed syntax-rules: " + arguments);
            }
        };
    }

}
