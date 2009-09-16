package daydreme;

final class SchemeObjects
{
    private SchemeObjects() {} // DENIED

    static List list(SchemeObject... items)
    {
        List list = new List();
        for (SchemeObject item : items) {
            list.add(item);
        }
        return list;
    }

    static Number num(double value)
    {
        return new Number(value);
    }

    static Pair pair(SchemeObject car, SchemeObject cdr)
    {
        return new Pair(car, cdr);
    }

    static Identifier word(String value)
    {
        return new Identifier(value);
    }
}