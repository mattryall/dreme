package dreme;

final class ConsUtils
{
    private ConsUtils() {} // DENIED

    static Cons list(Object head)
    {
        return new Cons(head);
    }

    static Tokens.Integer num(String value)
    {
        return new Tokens.Integer(value);
    }

    static Cons cons(Object car, Object cdr)
    {
        return new Cons(car, cdr);
    }

    static Tokens.BareWord word(String value)
    {
        return new Tokens.BareWord(value);
    }
}
