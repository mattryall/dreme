package daydreme;

import junit.framework.TestCase;
import static daydreme.SchemeObjects.*;
import static daydreme.Parser.Instance.parse;

public class TestListEvaluator extends TestCase {

    public void testSimpleEval() throws Exception {
        Environment environment = new Environment(Procedures.PLUS);
        ListEvaluator listEvaluator = new ListEvaluator();
        assertEquals(num(10), listEvaluator.evaluate(parse("(+ 4 6)"), environment));
        assertEquals(num(15), listEvaluator.evaluate(parse("(+ (+ 4 6) 5)"), environment));

        environment.define(word("a"), num(10));
        environment.define(word("b"), num(6));
        assertEquals(num(16), listEvaluator.evaluate(parse("(+ a b)"), environment));
        assertEquals(num(38), listEvaluator.evaluate(parse("(+ a b (+ b a b))"), environment));

        environment.define(word("omega"), new Lambda(list(word("x")), list(parse("(x x)")), environment));
        assertEquals(null, listEvaluator.evaluate(parse("(omega omega)"), environment));
    }
}
