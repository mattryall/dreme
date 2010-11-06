package dreme;

public class Ellipsis implements SchemeObject {
    public static Ellipsis INSTANCE = new Ellipsis(); 

    private Ellipsis() {
    }

    @Override
    public String toString() {
        return "...";
    }

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.ellipsis(this);
    }
}
