package dreme;

import dreme.macros.Lambda;

/**
 * Created by IntelliJ IDEA.
 * User: oysta
 * Date: 09/02/2010
 * Time: 9:30:54 PM
 * To change this template use File | Settings | File Templates.
 */
public interface SchemeObjectVisitor {

    void object(SchemeObject object);
    void identifier(Identifier identifier);
    void list(List list);
    void procedure(Procedure procedure);
    void lambda(Lambda lambda);
    void string(SchemeString string);
    void macro(Macro macro);
    void number(Number number);
    void pair(Pair pair);
    void schemeBoolean(SchemeBoolean schemeBoolean);
    void ellipsis(Ellipsis ellipsis);
    void unspecified(Unspecified unspecified);

}
