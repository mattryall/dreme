package daydreme;

class Lambda extends Procedure {
    private final List formals;
    private final List body;
    private final Environment scope;

    public Lambda(List formals, List body, Environment scope) {
        for (SchemeObject formal : formals) {
            if (!(formal instanceof Identifier))
                throw new IllegalArgumentException("Bad formal " + formal);
        }
        this.formals = formals;
        this.body = body;
        this.scope = scope;
    }

    SchemeObject apply(List arguments, Environment environment) {
        if (arguments.size() != formals.size()) {
            throw new IllegalArgumentException("Wrong number of arguments: " +
                arguments.size() + ", expected: " + formals.size());
        }
        Environment bodyEnv = scope.copy();
        for (int i=0; i<arguments.size(); i++) {
            SchemeObject arg = arguments.get(i);
            Identifier name = (Identifier) formals.get(i);
            bodyEnv.let(name, arg.evaluate(environment));
        }
        return body.evaluate(bodyEnv);
    }
}
