package dreme;

/**
 * Represents the result of a binding operation (which formally has an "unspecified" result), or
 * any other result that can be passed around but raises an assertion failure if evaluated.
 */
public class Unspecified extends BasicSchemeObject {
    public static final Unspecified INSTANCE = new Unspecified();

    private Unspecified() {
    }

    public String toString() {
        return "#<unspecified>";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.unspecified(this);
    }
}
