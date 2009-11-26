package daydreme;

interface SchemeObject extends Applyable {
    SchemeObject UNSPECIFIED = new SchemeObject() {

        public SchemeObject evaluate(ExecutionContext ctx) {
			return evaluate(ctx.getEnvironment());
		}

        public SchemeObject evaluate(Environment environment) {
            throw new IllegalArgumentException("Attempted to evaluate " + this);
        }

		public SchemeObject apply(ExecutionContext context) {
			return evaluate(context.getEnvironment());
		}

        public String toString() {
            return "#<unspecified>";
        }
    };

	SchemeObject evaluate(ExecutionContext context);

    /**
     * Evaluate the scheme object in some environment, and return the result. For
     * constant objects, this should return the object itself. For lists,
     * this will evaluate the list in the given environment.
     *
     * @param environment the environment which includes all currently defined
     * identifiers and locally bound variables.
     * @return the result of evaluating this object 
     */
    SchemeObject evaluate(Environment environment);
}
