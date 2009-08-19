package dreme;

import junit.framework.TestCase;
import static dreme.ConsUtils.*;

import java.util.Map;

public class TestEvaluator extends TestCase
{
    public void testSimpleAddition() throws Exception
    {
        Evaluator evaluator = new Evaluator();
        evaluator.define("+", new Evaluator.Function() {
            public Cons apply(Cons arguments, Map<Tokens.BareWord, Evaluator.Function> environment)
            {
                int result = 0;
                Cons currentCons = arguments;
                while (currentCons != null) {
                    Cons evaluated = new Cons(currentCons.car);
                    if (currentCons.car instanceof Cons)
                        evaluated = ((Cons) currentCons.car).evaluate(environment);
                    result += ((Tokens.Integer) evaluated.car).getInt();
                    currentCons = (Cons) currentCons.cdr;
                }

                return new Cons(new Tokens.Integer(String.valueOf(result)));
            }
        });
        assertEquals(list(num("25")), evaluator.eval("(+ 15 10)"));
        assertEquals(list(num("16")), evaluator.eval("(+ 5 (+ 5 2 3) 1)"));
        assertEquals(list(num("0")), evaluator.eval("(+)"));
    }
}
