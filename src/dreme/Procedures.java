package dreme;

import static dreme.List.toList;

class Procedures {
    static final NamedProcedure PLUS = new NamedProcedure("+") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = 0;
            for (SchemeObject arg : arguments) {
                result += ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure MINUS = new NamedProcedure("-") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = ((Number) arguments.car()).getValue();
            for (SchemeObject arg : arguments.tail()) {
                result -= ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure MULTIPLY = new NamedProcedure("*") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = 1;
            for (SchemeObject arg : arguments) {
                result *= ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure DIVIDE = new NamedProcedure("/") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = ((Number) arguments.car()).getValue();
            for (SchemeObject arg : arguments.tail()) {
                result /= ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final NamedProcedure LET = new NamedProcedure("let") {
        public SchemeObject apply(List arguments, Environment environment) {
            List declarations = toList(arguments.car());
            List expressions = arguments.tail();
            Environment bodyEnv = environment.copy();
            for (SchemeObject declaration : declarations) {
                if (!(declaration instanceof Pair))
                    throw new IllegalArgumentException("Invalid binding: " + declaration);
                List decl = toList(declaration);
                bodyEnv.bind(decl.get(0), decl.get(1));
            }
            return BEGIN.apply(expressions, bodyEnv);
        }
    };

    static final NamedProcedure LETREC = new NamedProcedure("letrec") {
        public SchemeObject apply(List arguments, Environment environment) {
            List declarations = toList(arguments.car());
            List expressions = arguments.tail();
            Environment bodyEnv = environment.copy();
            for (SchemeObject declaration : declarations) {
                if (!(declaration instanceof Pair))
                    throw new IllegalArgumentException("Invalid binding: " + declaration);
                List decl = toList(declaration);
                bodyEnv.bind(decl.get(0), decl.get(1));
            }
            return BEGIN.apply(expressions, bodyEnv);
        }
    };

    static final NamedProcedure CONS = new NamedProcedure("cons") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 2)
                throw new IllegalArgumentException("Expected 2 arguments, but got " + arguments.size());
            return new Pair(arguments.get(0), arguments.get(1));
        }
    };

    static final NamedProcedure CAR = new NamedProcedure("car") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 1)
                throw new IllegalArgumentException("Expected 1 argument, but got " + arguments.size());
            return ((Pair) arguments.get(0)).car();
        }
    };

    static final NamedProcedure CDR = new NamedProcedure("cdr") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 1)
                throw new IllegalArgumentException("Expected 1 argument, but got " + arguments.size());
            final SchemeObject cdr = ((Pair) arguments.get(0)).cdr();
            return cdr == null ? new List() : cdr;
        }
    };

    static final NamedProcedure EQV = new NamedProcedure("eqv?") {
        public SchemeObject apply(List arguments, Environment environment) {
            SchemeObject first = arguments.head();
            for (SchemeObject argument : arguments.tail()) {
                if (!first.equals(argument))
                    return SchemeBoolean.FALSE;
            }
            return SchemeBoolean.TRUE;
        }
    };

    static final NamedProcedure BEGIN = new NamedProcedure("begin") {
        public SchemeObject apply(List arguments, Environment environment) {
            SchemeObject result = null;
            for (SchemeObject expression : arguments) {
                result = expression;
            }
            return result;
        }
    };

    static final NamedProcedure DEFINE = new NamedProcedure("define") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (!(arguments.head() instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + arguments.head());
            environment.define((Identifier) arguments.head(), arguments.tail().head());
            return SchemeObject.UNSPECIFIED;
        }
    };

    static final NamedProcedure SET = new NamedProcedure("set!") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (!(arguments.get(0) instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + arguments.get(0));
            environment.set((Identifier) arguments.get(0), arguments.get(1));
            return SchemeObject.UNSPECIFIED;
        }
    };

    static final NamedProcedure IF = new NamedProcedure("if") {
        public SchemeObject apply(List arguments, Environment environment) {
            SchemeObject condition = arguments.get(0);
            if (!condition.equals(SchemeBoolean.FALSE)) {
                // condition is true
                return arguments.get(1);
            }
            else if (arguments.size() >= 3) {
                // condition is false
                return arguments.get(2);
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

    static final NamedProcedure EQ = new NumericComparator("=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) == 0;
        }
    };

    private static abstract class NumericComparator extends NamedProcedure {

        public NumericComparator(String name) {
            super(name);
        }

        public SchemeObject apply(List arguments, Environment environment) {
            Number last = (Number) arguments.get(0);
            for (SchemeObject arg : arguments.tail()) {
                Number current = (Number) arg;
                if (!isConsistent(last, current))
                    return SchemeBoolean.FALSE;
                last = current;
            }
            return SchemeBoolean.TRUE;
        }

        abstract boolean isConsistent(Number n1, Number n2);
    }

    static NamedProcedure CALL_CC = new NamedProcedure("call-with-current-continuation") {
        public SchemeObject apply(List arguments, Environment environment) {
            return null;
        }
    };
}
