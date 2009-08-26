package daydreme;

import static daydreme.SchemeObjects.ENVIRONMENT;
import dreme.TokenStream;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;

public class TestScheme {
    public static Test suite() throws Exception {
        TestSuite result = new TestSuite();
        for (String suiteName : Arrays.asList("lambda-tests", "define-tests")) {
            List tests = new Parser().parse(new TokenStream(getReader(suiteName + ".txt")));
            TestSuite suite = new TestSuite(suiteName);
            for (SchemeObject testObj : tests) {
                List test = new List((Pair) testObj);
                String name = ((Identifier) test.get(0)).getName();
                SchemeObject actual = new List((Pair) test.get(1));
                SchemeObject expected = test.get(2);
                suite.addTest(constructTest(name, expected, actual));
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
                assertEquals(expected, actual.evaluate(ENVIRONMENT));
            }
        };
    }

    private static Reader getReader(String fileName) {
        return new InputStreamReader(TestScheme.class.getResourceAsStream(fileName));
    }
}
