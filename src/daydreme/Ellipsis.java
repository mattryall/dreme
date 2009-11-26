package daydreme;

public class Ellipsis implements SchemeObject {
    public static Ellipsis INSTANCE = new Ellipsis(); 

    private Ellipsis() {
    }

	public SchemeObject apply(ExecutionContext context) {
		throw new RuntimeException("Cannot apply type");
	}

	public SchemeObject evaluate(ExecutionContext context) {
		return evaluate(context.getEnvironment());
	}

    public SchemeObject evaluate(Environment environment) {
        throw new IllegalStateException("Cannot evaluate ellipsis");
    }

    @Override
    public String toString() {
        return "...";
    }
}
