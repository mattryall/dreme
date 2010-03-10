package dreme.runtime;

import dreme.*;
import dreme.Number;
import org.apache.log4j.Logger;

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

    static final Procedure EQUAL = new Predicate("eqv?") {
        protected boolean evaluate(List arguments, Environment environment) {
            SchemeObject first = arguments.head();
            for (SchemeObject argument : arguments.tail()) {
                if (!first.equals(argument))
                    return false;
            }
            return true;
        }
    };

    private static final Logger log = Logger.getLogger(Procedures.class);

    static final Procedure EQ = new Predicate("eq?") {
        protected boolean evaluate(List arguments, Environment environment) {
            SchemeObject first = arguments.head();
            for (SchemeObject argument : arguments.tail()) {
                if (!areEqual(first, argument))
                    return false;
            }
            return true;
        }

        private boolean areEqual(SchemeObject first, SchemeObject argument) {
            if (first instanceof Pair) {
                if (!(argument instanceof Pair))
                    return false;
                if (((Pair) first).isEmpty() && ((Pair) argument).isEmpty())
                    return true;
                if (first != argument)
                    return false;
            }
            else if (!first.equals(argument)) {
                return false;
            }
            return true;
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

    static final Procedure NUMERIC_EQUALS = new NumericComparator("=") {
        boolean isConsistent(Number n1, Number n2) {
            return n1.compareTo(n2) == 0;
        }
    };

    private static abstract class NumericComparator extends Predicate {

        public NumericComparator(String name) {
            super(name);
        }

        protected boolean evaluate(List arguments, Environment environment) {
            Number last = (Number) arguments.get(0);
            for (SchemeObject arg : arguments.tail()) {
                Number current = (Number) arg;
                if (!isConsistent(last, current))
                    return false;
                last = current;
            }
            return true;
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

    static final Procedure PAIR = new SingleArgumentPredicate<Pair>("pair?", Pair.class) {
        protected boolean evaluate(Pair argument) {
            return !argument.isEmpty();
        }
    };

    static final Procedure NULL = new SingleArgumentPredicate<Pair>("null?", Pair.class) {
        protected boolean evaluate(Pair argument) {
            return argument.isEmpty();
        }
    };

    static final Procedure INTEGER = new SingleArgumentPredicate<Number>("integer?", Number.class) {
        protected boolean evaluate(Number argument) {
            return argument.isInteger();
        }
    };

    private static abstract class SingleArgumentPredicate<T extends SchemeObject> extends Predicate {
        private final Class<T> requiredType;

        protected SingleArgumentPredicate(String name, Class<T> requiredType) {
            super(name);
            this.requiredType = requiredType;
        }

        protected boolean evaluate(List arguments, Environment environment) {
            if (!requiredType.isInstance(arguments.head()))
                return false;
            return evaluate(requiredType.cast(arguments.head()));
        }

        protected abstract boolean evaluate(T argument);
    }

    private static abstract class Predicate extends Procedure {
        private final String name;

        protected Predicate(String name) {
            super(name);
            this.name = name;
        }

        protected SchemeObject apply(List arguments, Environment environment) {
            boolean result = evaluate(arguments, environment);
            if (log.isDebugEnabled()) {
                List list = new List(new Identifier(name), arguments);
                log.debug("Evaluated predicate " + list + ") to " + result);
            }
            return result ? SchemeBoolean.TRUE : SchemeBoolean.FALSE;
        }

        protected abstract boolean evaluate(List arguments, Environment environment);
    }
}
