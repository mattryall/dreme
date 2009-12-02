package daydreme;

interface SchemeObject {
    SchemeObject UNSPECIFIED = new SchemeObject() {
        public void evaluate(ExecutionContext ctx) {
            throw new IllegalArgumentException("Attempted to evaluate " + this);
		}

        public String toString() {
            return "#<unspecified>";
        }
    };

	void evaluate(ExecutionContext context);
}
