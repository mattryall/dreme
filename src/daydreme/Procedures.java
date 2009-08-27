package daydreme;

class Procedures {
    static final NamedProcedure PLUS = new NamedProcedure("+") {
        SchemeObject apply(List arguments, Environment environment) {
            double result = 0;
            for (SchemeObject arg : arguments) {
                result += ((Number) arg.evaluate(environment)).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure MINUS = new NamedProcedure("-") {
        SchemeObject apply(List arguments, Environment environment) {
            double result = ((Number) arguments.car().evaluate(environment)).getValue();
            for (SchemeObject arg : arguments.tail()) {
                result -= ((Number) arg.evaluate(environment)).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure MULTIPLY = new NamedProcedure("*") {
        SchemeObject apply(List arguments, Environment environment) {
            double result = 1;
            for (SchemeObject arg : arguments) {
                result *= ((Number) arg.evaluate(environment)).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure DIVIDE = new NamedProcedure("/") {
        SchemeObject apply(List arguments, Environment environment) {
            double result = ((Number) arguments.car().evaluate(environment)).getValue();
            for (SchemeObject arg : arguments.tail()) {
                result /= ((Number) arg.evaluate(environment)).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure LET = new NamedProcedure("let") {
        SchemeObject apply(List arguments, Environment environment) {
            List declarations = new List((Pair) arguments.car());
            List expressions = arguments.tail();
            Environment newEnv = environment.copy();
            for (SchemeObject declaration : declarations) {
                if (!(declaration instanceof Pair))
                    throw new IllegalArgumentException("Invalid binding: " + declaration);
                List decl = new List((Pair) declaration);
                newEnv.let(decl.get(0), decl.get(1));
            }
            return BEGIN.apply(expressions, newEnv);
        }
    };

    static final NamedProcedure LETREC = new NamedProcedure("letrec") {
        SchemeObject apply(List arguments, Environment environment) {
            List declarations = new List((Pair) arguments.car());
            List expressions = arguments.tail();
            Environment newEnv = environment.copy();
            for (SchemeObject declaration : declarations) {
                if (!(declaration instanceof Pair))
                    throw new IllegalArgumentException("Invalid binding: " + declaration);
                List decl = new List((Pair) declaration);
                newEnv.letrec(decl.get(0), decl.get(1));
            }
            return BEGIN.apply(expressions, newEnv);
        }
    };

    static final NamedProcedure BEGIN = new NamedProcedure("begin") {
        SchemeObject apply(List arguments, Environment environment) {
            SchemeObject result = null;
            for (SchemeObject expression : arguments) {
                result = expression.evaluate(environment);
            }
            return result;
        }
    };

    static final NamedProcedure DEFINE = new NamedProcedure("define") {
        SchemeObject apply(List arguments, Environment environment) {
            if (!(arguments.get(0) instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + arguments.get(0));
            environment.define((Identifier) arguments.get(0), arguments.get(1).evaluate(environment));
            return SchemeObject.UNSPECIFIED;
        }
    };

    static final NamedProcedure IF = new NamedProcedure("if") {
        SchemeObject apply(List arguments, Environment environment) {
            SchemeObject condition = arguments.get(0);
            if (!condition.evaluate(environment).equals(SchemeBoolean.FALSE)) {
                // condition is true
                return arguments.get(1).evaluate(environment);
            }
            else if (arguments.size() >= 3) {
                // condition is false
                return arguments.get(2).evaluate(environment);
            }
            else {
                // condition is false, but we have no third argument
                return SchemeObject.UNSPECIFIED;
            }
        }
    };

    static final NamedProcedure GT = new NumericComparator(">") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) > 0;
        }
    };

    static final NamedProcedure LT = new NumericComparator("<") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) < 0;
        }
    };

    static final NamedProcedure GE = new NumericComparator(">=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) >= 0;
        }
    };

    static final NamedProcedure LE = new NumericComparator("<=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) <= 0;
        }
    };

    private static abstract class NumericComparator extends NamedProcedure {
        public NumericComparator(String name) {
            super(name);
        }

        SchemeObject apply(List arguments, Environment environment) {
            Number last = (Number) arguments.get(0).evaluate(environment);
            for (SchemeObject arg : arguments.tail()) {
                Number current = (Number) arg.evaluate(environment);
                if (!isConsistent(last, current))
                    return SchemeBoolean.FALSE;
                last = current;
            }
            return SchemeBoolean.TRUE;
        }

        abstract boolean isConsistent(Number n1, Number n2);
    }
}
