package daydreme;

class Procedures {
    static final NamedProcedure PLUS = new NamedProcedure("+") {
        SchemeObject apply(Pair arguments, Environment environment)
        {
            int result = 0;
            for (SchemeObject arg : new List(arguments)) {
                result += ((Integer) arg.evaluate(environment)).getValue();
            }
            return new Integer(result);
        }
    };

    static final NamedProcedure MULTIPLY = new NamedProcedure("*") {
        SchemeObject apply(Pair arguments, Environment environment)
        {
            int result = 1;
            for (SchemeObject arg : new List(arguments)) {
                result *= ((Integer) arg.evaluate(environment)).getValue();
            }
            return new Integer(result);
        }
    };

    static final NamedProcedure LET = new NamedProcedure("let") {
        SchemeObject apply(Pair arguments, Environment environment) {
            List variables = new List((Pair) arguments.car());
            List expressions = new List((Pair) arguments.cdr());
            Environment newEnv = environment.copy();
            for (SchemeObject var : variables) {
                if (!(var instanceof Pair))
                    throw new IllegalArgumentException("Invalid binding: " + var);
                List decl = new List((Pair) var);
                SchemeObject name = decl.get(0);
                if (!(name instanceof Identifier))
                    throw new IllegalArgumentException("Invalid binding: " + name);
                SchemeObject value = decl.get(1).evaluate(environment);
                newEnv.let((Identifier) name, value);
            }
            SchemeObject result = null;
            for (SchemeObject expression : expressions) {
                result = expression.evaluate(newEnv);
            }
            return result;
        }
    };

    static final NamedProcedure BEGIN = new NamedProcedure("begin") {
        SchemeObject apply(Pair arguments, Environment environment) {
            SchemeObject result = null;
            for (SchemeObject expression : new List(arguments)) {
                result = expression.evaluate(environment);
            }
            return result;
        }
    };

    static final NamedProcedure DEFINE = new NamedProcedure("define") {
        SchemeObject apply(Pair arguments, Environment environment) {
            List args = new List(arguments);
            if (!(args.get(0) instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + args.get(0));
            environment.define((Identifier) args.get(0), args.get(1).evaluate(environment));
            return SchemeObject.UNSPECIFIED;
        }
    };
}
