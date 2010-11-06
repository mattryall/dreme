package dreme.macros.interop;

import dreme.*;

/**
 * Gets the name of an identifier or the value of a SchemeString as a String.
 * Used by the Java interop macros to accept either an identifier or a string
 * when specifying the name of a method or class.
 */
final class NameResolvingVisitor extends AbstractSchemeObjectVisitor {
    private String name;

    public void identifier(Identifier identifier) {
        name = identifier.getName();
    }

    public void string(SchemeString string) {
        name = string.getValue();
    }

    public void object(SchemeObject object) {
        throw new IllegalArgumentException("Cannot use name of: " + object);
    }

    public void unspecified(Unspecified unspecified) {
        throw new IllegalArgumentException("Cannot use name of: " + unspecified);
    }

    public String getName() {
        return name;
    }

}
