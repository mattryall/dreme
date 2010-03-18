package dreme.runtime;

import dreme.*;

public final class Coercions {

    public static Procedure TO_STRING = new Procedure("->string") {
        protected SchemeObject apply(List arguments, Environment environment) {
            return new SchemeString(arguments.head().toString());
        }
    };

}
