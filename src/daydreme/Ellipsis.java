package daydreme;

public class Ellipsis implements SchemeObject {
    public static Ellipsis INSTANCE = new Ellipsis(); 

    private Ellipsis() {
    }

    public SchemeObject evaluate(Environment environment) {
        throw new IllegalStateException("Cannot evaluate ellipsis");
    }

    @Override
    public String toString() {
        return "#<...>";
    }
}
