package daydreme;

import junit.framework.TestCase;
import static daydreme.SchemeObjects.*;
import static daydreme.Parser.Instance.parse;

public class TestListEvaluator extends TestCase {
    private final Environment environment = new Environment(Procedures.PLUS);
    private final ListEvaluator listEvaluator = new ListEvaluator();

    public void testSimpleEval() throws Exception {
        assertEquals(num(10), listEvaluator.evaluate(parse("(+ 4 6)"), environment));
        assertEquals(num(15), listEvaluator.evaluate(parse("(+ (+ 4 6) 5)"), environment));

        environment.define(word("a"), num(10));
        environment.define(word("b"), num(6));
        assertEquals(num(16), listEvaluator.evaluate(parse("(+ a b)"), environment));
        assertEquals(num(38), listEvaluator.evaluate(parse("(+ a b (+ b a b))"), environment));

//        environment.define(word("omega"), new Lambda(list(word("x")), list(parse("(x x)")), environment));
//        assertEquals(null, listEvaluator.evaluate(parse("(omega omega)"), environment));
    }

    public void testLambdaParsing() throws Exception {
        assertEquals(num(5), listEvaluator.evaluate(parse("((lambda (x y) (+ x y)) 2 3)"), environment));
        assertEquals(num(7), listEvaluator.evaluate(parse("((lambda (x y) (+ x y) (+ x x y)) 2 3)"), environment));
        // (define add (lambda (x y) (+ x y))

    }
}
