package dreme.runtime;

import junit.framework.TestCase;

import static dreme.Parser.Instance.parse;

public class TestTailCallOptimisation extends TestCase {

    public void testLengthTailCallsAreOptimised() throws Exception {
        Runtime runtime = new Runtime(5);
        runtime.run(parse("(length '(1 2 3 4 5 6 7 8 9))"));
    }
}
