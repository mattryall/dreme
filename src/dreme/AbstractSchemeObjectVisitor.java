package dreme;

import dreme.macros.Lambda;

/**
 * Created by IntelliJ IDEA.
 * User: oysta
 * Date: 09/02/2010
 * Time: 9:47:52 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class AbstractSchemeObjectVisitor implements SchemeObjectVisitor {

    @Override
    public void identifier(Identifier identifier) {
        object(identifier);
    }

    @Override
    public void list(List list) {
        object(list);
    }

    @Override
    public void procedure(Procedure procedure) {
        object(procedure);
    }

    @Override
    public void lambda(Lambda lambda) {
        object(lambda);
    }

    @Override
    public void string(SchemeString string) {
        object(string);
    }

    @Override
    public void macro(Macro macro) {
        object(macro);
    }

    @Override
    public void number(Number number) {
        object(number);
    }

    @Override
    public void pair(Pair pair) {
        object(pair);
    }

    @Override
    public void schemeBoolean(SchemeBoolean schemeBoolean) {
        object(schemeBoolean);
    }

    @Override
    public void ellipsis(Ellipsis ellipsis) {
        object(ellipsis);
    }
}
