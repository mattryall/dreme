package dreme;

import java.util.HashMap;
import java.util.Map;

class Procedures {
    static final Procedure PLUS = new Procedure("+") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = 0;
            for (SchemeObject arg : arguments) {
                result += ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final Procedure MINUS = new Procedure("-") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = ((Number) arguments.car()).getValue();
            for (SchemeObject arg : arguments.tail()) {
                result -= ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final Procedure MULTIPLY = new Procedure("*") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = 1;
            for (SchemeObject arg : arguments) {
                result *= ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final Procedure DIVIDE = new Procedure("/") {
        public SchemeObject apply(List arguments, Environment environment) {
            double result = ((Number) arguments.car()).getValue();
            for (SchemeObject arg : arguments.tail()) {
                result /= ((Number) arg).getValue();
            }
            return new Number(result);
        }
    };

    static final Procedure CONS = new Procedure("cons") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 2)
                throw new IllegalArgumentException("Expected 2 arguments, but got " + arguments.size());
            return new Pair(arguments.get(0), arguments.get(1));
        }
    };

    static final Procedure CAR = new Procedure("car") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 1)
                throw new IllegalArgumentException("Expected 1 argument, but got " + arguments.size());
            return ((Pair) arguments.get(0)).car();
        }
    };

    static final Procedure CDR = new Procedure("cdr") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 1)
                throw new IllegalArgumentException("Expected 1 argument, but got " + arguments.size());
            final SchemeObject cdr = ((Pair) arguments.get(0)).cdr();
            return cdr == null ? new List() : cdr;
        }
    };

    static final Procedure EQV = new Procedure("eqv?") {
        public SchemeObject apply(List arguments, Environment environment) {
            SchemeObject first = arguments.head();
            for (SchemeObject argument : arguments.tail()) {
                if (!first.equals(argument))
                    return SchemeBoolean.FALSE;
            }
            return SchemeBoolean.TRUE;
        }
    };

    static final Procedure DEFINE = new Procedure("define") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (!(arguments.head() instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + arguments.head());
            environment.define((Identifier) arguments.head(), arguments.tail().head());
            return SchemeObject.UNSPECIFIED;
        }
    };

    static final Procedure SET = new Procedure("set!") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (!(arguments.get(0) instanceof Identifier))
                throw new IllegalArgumentException("Bad variable " + arguments.get(0));
            environment.set((Identifier) arguments.get(0), arguments.get(1));
            return SchemeObject.UNSPECIFIED;
        }
    };

    static final Procedure IF = new Procedure("if") {
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

    static final Procedure GT = new NumericComparator(">") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) > 0;
        }
    };

    static final Procedure LT = new NumericComparator("<") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) < 0;
        }
    };

    static final Procedure GE = new NumericComparator(">=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) >= 0;
        }
    };

    static final Procedure LE = new NumericComparator("<=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) <= 0;
        }
    };

    static final Procedure EQ = new NumericComparator("=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) == 0;
        }
    };

    private static abstract class NumericComparator extends Procedure {

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

    static Procedure CALL_CC = new Procedure("call-with-current-continuation") {
        public SchemeObject apply(List arguments, Environment environment) {
            return null;
        }
    };
    
    static final Map<String, Procedure> BUILT_IN_PROCEDURES = new HashMap<String, Procedure>();
    
    static {
        BUILT_IN_PROCEDURES.put("define", DEFINE);
        BUILT_IN_PROCEDURES.put("set!", SET);
        BUILT_IN_PROCEDURES.put("if", IF);
        BUILT_IN_PROCEDURES.put("cons", CONS);
        BUILT_IN_PROCEDURES.put("car", CAR);
        BUILT_IN_PROCEDURES.put("cdr", CDR);
        BUILT_IN_PROCEDURES.put("eqv?", EQV);
        BUILT_IN_PROCEDURES.put("call-with-current-continuation", CALL_CC);
        BUILT_IN_PROCEDURES.put("+", PLUS);
        BUILT_IN_PROCEDURES.put("-", MINUS);
        BUILT_IN_PROCEDURES.put("*", MULTIPLY);
        BUILT_IN_PROCEDURES.put("/", DIVIDE);
        BUILT_IN_PROCEDURES.put(">", GT);
        BUILT_IN_PROCEDURES.put("<", LT);
        BUILT_IN_PROCEDURES.put(">=", GE);
        BUILT_IN_PROCEDURES.put("<=", LE);
        BUILT_IN_PROCEDURES.put("=", EQ);
    }
}
