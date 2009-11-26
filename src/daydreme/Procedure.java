package daydreme;

abstract class Procedure implements SchemeObject, Applyable {

    public SchemeObject evaluate(ExecutionContext ctx) {
		return evaluate(ctx.getEnvironment());
	}

    public SchemeObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public String toString() {
        return "#<procedure>";
    }

    SchemeObject apply(List arguments, Environment environment) { 
		return SchemeObject.UNSPECIFIED;
	}

    public SchemeObject apply(ExecutionContext context) {
		SchemeObject o = apply(context.evaluatedValues(), context.getEnvironment());
		context.returnValue(o);
		return o;
	}
}
