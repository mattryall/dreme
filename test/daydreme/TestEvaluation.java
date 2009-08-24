package daydreme;

import junit.framework.TestCase;
import static daydreme.SchemeObjects.*;
import static daydreme.Procedures.*;

import java.io.IOException;

public class TestEvaluation extends TestCase
{
    private Parser parser = new Parser();
    private Environment environment = new Environment(LET, PLUS, MULTIPLY);

    private SchemeObject eval(String scheme) throws IOException {
        return parser.parse(scheme).evaluate(environment);
    }

    public void testSimpleAddition() throws Exception
    {
        assertEquals(num(25), eval("(+ 15 10)"));
        assertEquals(num(16), eval("(+ 5 (+ 5 2 3) 1)"));
        assertEquals(num(0), eval("(+)"));
    }

    public void testMultiplication() throws Exception {
        assertEquals(num(10), eval("(* 2 5)"));
        assertEquals(num(22), eval("(* 2 (+ (+ 4 1) (* 2 3)))"));
    }

    public void testLet() throws Exception {
        assertEquals(num(10), eval("(let ((x 5)) (+ x x))"));
        assertEquals(num(11), eval("(let ((x 5) (y 6)) (+ x y))"));
        assertEquals("last expression is returned", num(41),
            eval("(let ((x 5) (y 6)) (+ x y) (+ (* x y) (+ x y)))"));
        assertEquals("nested", num(50), eval(
            "(let ((x 5))" +
                "(let ((y 10)) (* x y)))"));
        assertEquals("shadowing", num(100), eval(
            "(let ((x 5))" +
                "(let ((y (* 2 x)))" +
                    "(let ((x 10)) (* x y))))"));
    }
}