package dreme;

public class Ellipsis implements SchemeObject {
    public static Ellipsis INSTANCE = new Ellipsis(); 

    private Ellipsis() {
    }

	public void evaluate(ExecutionContext context) {
        throw new IllegalStateException("Cannot evaluate ellipsis");
	}

    @Override
    public String toString() {
        return "...";
    }
}
