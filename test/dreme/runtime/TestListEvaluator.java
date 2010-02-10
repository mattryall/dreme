package dreme.runtime;

import dreme.Environment;
import dreme.Procedures;
import dreme.macros.LambdaMacro;
import junit.framework.TestCase;

import static dreme.SchemeObjects.*;
import static dreme.Parser.Instance.parse;

public class TestListEvaluator extends TestCase {
    private Environment environment;
    private ListEvaluator listEvaluator;

    protected void setUp() throws Exception {
        super.setUp();
        environment = new Environment();
        environment.define(word("+"), Procedures.PLUS);
        environment.define(word("lambda"), new LambdaMacro());
        listEvaluator = new ListEvaluator(environment);
    }

    public void testSimpleEval() throws Exception {
        assertEquals(num(10), listEvaluator.evaluate(parse("(+ 4 6)")));
        assertEquals(num(15), listEvaluator.evaluate(parse("(+ (+ 4 6) 5)")));

        environment.define(word("a"), num(10));
        environment.define(word("b"), num(6));
        assertEquals(num(16), listEvaluator.evaluate(parse("(+ a b)")));
        assertEquals(num(38), listEvaluator.evaluate(parse("(+ a b (+ b a b))")));

//        environment.define(word("omega"), new Lambda(list(word("x")), list(parse("(x x)")), environment));
//        assertEquals(null, listEvaluator.evaluate(parse("(omega omega)"), environment));
    }

    public void testLambdaParsing() throws Exception {
        assertEquals(num(5), listEvaluator.evaluate(parse("((lambda (x y) (+ x y)) 2 3)")));
        assertEquals(num(7), listEvaluator.evaluate(parse("((lambda (x y) (+ x y) (+ x x y)) 2 3)")));
        // (define add (lambda (x y) (+ x y))

    }
}
