package dreme.runtime;

import dreme.*;

/**
* Created by IntelliJ IDEA.
* User: oysta
* Date: 21/02/2010
* Time: 8:58:15 PM
* To change this template use File | Settings | File Templates.
*/
class SchemeProcessor extends AbstractSchemeObjectVisitor {
    private final ExecutionContext ctx;

    public SchemeProcessor(ExecutionContext ctx) {
        this.ctx = ctx;
    }

    public void object(SchemeObject obj) {
        ctx.addResult(obj);
    }

    public void identifier(Identifier identifier) {
        if (!ctx.getEnvironment().contains(identifier))
            throw new IllegalStateException("Unbound variable: " + identifier.getName());
        object(ctx.getEnvironment().get(identifier));
    }

    public void list(List list) {
        ctx.execute(list, ctx.getEnvironment());
    }

    public void unspecified(Unspecified unspecified) {
        throw new IllegalArgumentException("Attempted to evaluate " + unspecified);
    }

    public void evaluate(SchemeObject schemeObject) {
        schemeObject.acceptVisitor(this);
    }

    public void apply(Operator operator) {
        operator.apply(ctx);
    }
}
