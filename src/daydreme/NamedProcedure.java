package daydreme;

abstract class NamedProcedure extends Procedure {
    private final Identifier name;

    protected NamedProcedure(Identifier name) {
        this.name = name;
    }

    public NamedProcedure(String name) {
        this(new Identifier(name));
    }

    @Override
    public String toString() {
        return "#<procedure " + name + ">";
    }

    public Identifier getName() {
        return name;
    }
}