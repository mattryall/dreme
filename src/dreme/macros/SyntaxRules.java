package dreme.macros;

import static dreme.List.toList;

import dreme.*;
import org.apache.log4j.Logger;

class SyntaxRules extends PrimitiveMacro {
    private static final Logger log = Logger.getLogger(SyntaxRules.class);
    private final List literals;
    private final List clauses;

    public SyntaxRules(List literals, List clauses) {
        this.literals = literals;
        this.clauses = clauses;
    }

    public void process(List body, ExecutionContext ctx) {
        for (SchemeObject t : clauses) {
            List clause = toList(t);
            List pattern = toList(clause.get(0)).tail();
            List template = clause.tail();
            final PatternMatcher matcher = new PatternMatcher(pattern);
            if (matcher.matches(body)) {
                SchemeObject transformed = matcher.apply(body, template).get(0);
                if (log.isDebugEnabled())
                    log.debug("Macro transformation result: " + transformed);
                if (transformed instanceof List) {
                    ctx.executeInPlace(List.toList(transformed), ctx.getEnvironment());
                }
                else if (transformed instanceof Identifier) {
                    ctx.returnValue(ctx.getEnvironment().get((Identifier) transformed));
                }
                else {
                    ctx.returnValue(transformed);
                }
                return;
            }
        }
        throw new IllegalArgumentException("Malformed syntax-rules: " + body + "\n  clauses: " + clauses);
    }
}
