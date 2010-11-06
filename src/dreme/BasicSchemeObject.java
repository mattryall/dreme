package dreme;

/**
 * Created by IntelliJ IDEA.
 * User: oysta
 * Date: 09/02/2010
 * Time: 9:43:53 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BasicSchemeObject implements SchemeObject {

    public void acceptVisitor(SchemeObjectVisitor visitor) {
        visitor.object(this);
    }

}
