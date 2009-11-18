package daydreme;

import static daydreme.List.toList;

class Lambda extends Procedure {
    private final SchemeObject formals;
    private final List body;
    private final Environment scope;

    public Lambda(SchemeObject formals, List body, Environment scope) {
        if (formals instanceof Pair) {
            for (SchemeObject formal : toList(formals)) {
                if (!(formal instanceof Identifier))
                    throw new IllegalArgumentException("Bad formal " + formal);
            }
        }
        this.formals = formals;
        this.body = body;
        this.scope = scope;
    }

    SchemeObject apply(List arguments, Environment environment) {
        Environment bodyEnv = getArgumentsEnv(arguments);
        SchemeObject result = SchemeObject.UNSPECIFIED;
        for (SchemeObject object : body) {
            result = object.evaluate(bodyEnv);
        }
        return result;
    }

    public Environment getArgumentsEnv(List actualArguments) {
        Environment env = scope.copy();
        env.bindAll(getArgumentsEnv(formals, actualArguments));
        return env;
    }

    private static Environment getArgumentsEnv(SchemeObject formals, List actuals) {
        Environment environment = new Environment();
        if (!(formals instanceof Pair)) {
            Identifier name = (Identifier) formals;
            environment.bind(name, actuals);
            return environment;
        }

        List formalsList = toList(formals);
        if (formalsList.isProper() && actuals.size() != formalsList.size()) {
            throw new IllegalArgumentException("Wrong number of arguments: " +
                actuals.size() + ", expected: " + formalsList.size());
        }
        for (SchemeObject formal : formalsList) {
            environment.bind(formal, actuals.head());
            actuals = actuals.tail();
        }
        if (!formalsList.isProper()) {
            Identifier restArg = (Identifier) formalsList.lastPair().cdr();
            environment.bind(restArg, actuals);
        }
        return environment;
    }

    public String toString()
    {
        return "#<procedure #f " + formals + ">";
    }

    public List getBody() {
        return body;
    }
}
