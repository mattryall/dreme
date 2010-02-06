package dreme;

public interface SchemeObject {
    Evaluatable UNSPECIFIED = new Evaluatable() {
        public void evaluate(ExecutionContext ctx) {
            throw new IllegalArgumentException("Attempted to evaluate " + this);
		}

        public String toString() {
            return "#<unspecified>";
        }
    };
}
