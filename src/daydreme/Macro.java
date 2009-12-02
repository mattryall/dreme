package daydreme;

public interface Macro extends SchemeObject {
    void process(List body, ExecutionContext ctx);
}
