package dreme;

import static dreme.List.toList;

import dreme.runtime.Runtime;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.*;

public class TestScheme {
    static final java.util.List<String> SUITES = Arrays.asList(
        "builtin", "lambda", "define", "if", "define-syntax", "cons", "quote", "call-cc");

    public static Test suite() throws Exception {
        TestSuite result = new TestSuite();
        for (String suiteName : SUITES) {
            Reader reader = getReader(suiteName + "-tests.scm");
            List tests;
            try {
                tests = new Parser().parse(new TokenStream(reader));
            }
            catch (RuntimeException e) {
                result.addTest(new ErrorTestCase(suiteName, e));
                continue;
            }
            TestSuite suite = new TestSuite(suiteName);
            for (SchemeObject testObj : tests) {
                List test = toList(testObj);
                String name = ((Identifier) test.get(0)).getName();
                try {
                    SchemeObject actual = toList(test.get(1));
                    SchemeObject expected = test.get(2);
                    suite.addTest(constructTest(name, expected, actual));
                }
                catch (RuntimeException e) {
                    suite.addTest(new ErrorTestCase(name, e));
                }
            }
            result.addTest(suite);
        }
        return result;
    }

    private static TestCase constructTest(final String name, final SchemeObject expected,
        final SchemeObject actual)
    {
        return new TestCase() {
            public String getName() {
                return name;
            }

            protected void runTest() throws Throwable {
                assertEquals(mapExpected(expected), new Runtime().run((List) actual));
            }

            private SchemeObject mapExpected(SchemeObject expected) {
                if (expected.equals(new Identifier("#<unspecified>")))
                    return Unspecified.INSTANCE;
                return expected;
            }
        };
    }

    private static Reader getReader(String fileName) {
        return new InputStreamReader(TestScheme.class.getResourceAsStream(fileName));
    }

    private static class ErrorTestCase extends TestCase {
        private final String suiteName;
        private final RuntimeException exception;

        public ErrorTestCase(String suiteName, RuntimeException exception) {
            this.suiteName = suiteName;
            this.exception = exception;
        }

        public String getName() {
            return suiteName;
        }

        protected void runTest() throws Throwable {
            throw exception;
        }
    }
}
