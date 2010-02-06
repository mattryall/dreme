package dreme;

public abstract class Procedure implements SchemeObject, Operator {

    public void evaluate(ExecutionContext ctx) {
		ctx.addResult(this);
	}

    @Override
    public String toString() {
        return "#<procedure>";
    }

    public SchemeObject apply(List arguments, Environment environment) { 
		return SchemeObject.UNSPECIFIED;
	}

    public void apply(ExecutionContext context) {
		SchemeObject result = apply(context.evaluatedValues(), context.getEnvironment());
		context.returnValue(result);
	}
}
