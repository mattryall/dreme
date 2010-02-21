package dreme.macros;

import static dreme.List.toList;

import dreme.*;

public class Lambda extends Substitution {
    private final SchemeObject formals;
    private final List body;
    private final Environment scope;

	public Lambda(List arguments, Environment scope) {
		this(arguments.head(), arguments.tail(), scope);
	}

    private Lambda(SchemeObject formals, List body, Environment scope) {
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

    protected List getSubstitute(ExecutionContext context) {
        List substitute = new List();
        substitute.add(new Container());
        substitute.addAll(getBody());
        return substitute;
    }

    protected Environment getNewEnvironment(ExecutionContext context) {
        Environment env = scope.copy();
        env.bindAll(getArgumentsEnv(formals, context.evaluatedValues()));
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

    public String toString() {
        return "#<procedure 0x" + Integer.toString(System.identityHashCode(this), 16) + " " + formals + ">";
    }

    public List getBody() {
        return body;
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.lambda(this);
    }
}
