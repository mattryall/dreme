package dreme;

public final class SchemeObjects
{
    private SchemeObjects() {} // DENIED

    public static List list(SchemeObject... items)
    {
        List list = new List();
        for (SchemeObject item : items) {
            list.add(item);
        }
        return list;
    }

    public static Number num(double value)
    {
        return new Number(value);
    }

    public static Pair pair(SchemeObject car, SchemeObject cdr)
    {
        return new Pair(car, cdr);
    }

    public static Identifier word(String value)
    {
        return new Identifier(value);
    }
}