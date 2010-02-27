package dreme.runtime;

import dreme.*;
import dreme.Number;

import java.util.Map;

class Procedures {
    public static final Procedure PLUS = new Procedure("+") {
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
            if (!(arguments.get(0) instanceof Pair))
                throw new IllegalArgumentException("Expected pair, but was " + arguments.get(0));
            return ((Pair) arguments.get(0)).car();
        }
    };

    static final Procedure CDR = new Procedure("cdr") {
        public SchemeObject apply(List arguments, Environment environment) {
            if (arguments.size() != 1)
                throw new IllegalArgumentException("Expected 1 argument, but got " + arguments.size());
            if (!(arguments.get(0) instanceof Pair))
                throw new IllegalArgumentException("Expected pair, but was " + arguments.get(0));
            final SchemeObject cdr = ((Pair) arguments.get(0)).cdr();
            return cdr == null ? new List() : cdr;
        }
    };

    static final Procedure EQUAL = new Procedure("eqv?") {
        public SchemeObject apply(List arguments, Environment environment) {
            SchemeObject first = arguments.head();
            for (SchemeObject argument : arguments.tail()) {
                if (!first.equals(argument))
                    return SchemeBoolean.FALSE;
            }
            return SchemeBoolean.TRUE;
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

    static final Procedure ENV = new Procedure() {
        protected SchemeObject apply(List arguments, Environment environment) {
            for (Map.Entry<Identifier, SchemeObject> entry : environment) {
                System.out.println(entry.getKey() + " => " + entry.getValue());
            }
            return Unspecified.INSTANCE;
        }
    };
}
