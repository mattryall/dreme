package daydreme;

public class SchemeBoolean implements SchemeObject {
    public static final SchemeBoolean TRUE = new SchemeBoolean("#t");
    public static final SchemeBoolean FALSE = new SchemeBoolean("#f");

    private final String value;

    private SchemeBoolean(String value) {
        this.value = value;
    }

	public SchemeObject apply(ExecutionContext context) {
		throw new RuntimeException("Cannot apply type");
	}

	public void evaluate(ExecutionContext context) {
        context.addResult(this);
	}

    public String toString() {
        return value;
    }
}
