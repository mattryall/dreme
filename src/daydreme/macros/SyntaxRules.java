package daydreme.macros;

import static daydreme.List.toList;
import daydreme.*;

class SyntaxRules extends PrimitiveMacro {
    private final List literals;
    private final List clauses;

    public SyntaxRules(List literals, List clauses) {
        if (!literals.isEmpty())
            throw new IllegalArgumentException("Literals must be empty at the moment sorry");

        this.literals = literals;
        this.clauses = clauses;
    }

    public void process(List body, ExecutionContext ctx) {
        for (SchemeObject t : clauses) {
            List clause = toList(t);
            List pattern = toList(clause.get(0)).tail();
            List template = clause.tail();
            final PatternMatcher matcher = new PatternMatcher(pattern, template);
            if (matcher.matches(body)) {
                SchemeObject transformed = matcher.apply(body).get(0);
                if (transformed instanceof List) {
                    ctx.executeInPlace(List.toList(transformed), ctx.getEnvironment());
                }
                else {
                    ctx.returnValue(transformed);
                }
                return;
            }
        }
        throw new IllegalArgumentException("Malformed syntax-rules: " + body);
    }
}
