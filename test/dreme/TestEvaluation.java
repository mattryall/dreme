package dreme;

import junit.framework.TestCase;
import static dreme.SchemeObjects.*;

import java.io.IOException;
import dreme.runtime.Runtime;

public class TestEvaluation extends TestCase
{
    private Parser parser = new Parser();
    private Runtime runtime = new Runtime();

    private SchemeObject eval(String scheme) throws IOException {
		return runtime.run(parser.parse(scheme));
    }

    public void testSimpleAddition() throws Exception
    {
        assertEquals(num(25), eval("(+ 15 10)"));
        assertEquals(num(16), eval("(+ 5 (+ 5 2 3) 1)"));
        assertEquals(num(0), eval("(+)"));
        assertEquals(num(13), eval("(+ 15 -2)"));
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

    public void testLambda() throws Exception {
        assertEquals(num(10), eval("((lambda () (+ 5 5)))"));
    }

    public void testLambdaWithFormals() throws Exception {
        assertEquals(num(49), eval("((lambda (x) (* x x)) 7)"));
    }

    public void testLambdaEnvironment() throws Exception {
        assertEquals(num(35), eval("(let ((x 5)) ((lambda (y) (* y x)) 7))"));
    }
}
