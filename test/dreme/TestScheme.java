package dreme;

import static dreme.List.toList;

import dreme.runtime.Runtime;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.InputStreamReader;
import java.io.Reader;

public class TestScheme {
    public static Test suite() throws Exception {
        Classpath classpath = new Classpath(TestScheme.class.getClassLoader());
        TestSuite result = new TestSuite();
        for (String resourceName : classpath.findResourcesMatching("dreme/", "tests.scm")) {
            Reader reader = classpath.getResource(resourceName);
            String suiteName = resourceName.replaceFirst("^.*/", "").replaceFirst("\\.scm$", "");
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
                String name = suiteName + ".(untitled)";
                try {
                    List test = toList(testObj);
                    name = ((Identifier) test.get(0)).getName();
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
        private final String name;
        private final RuntimeException exception;

        public ErrorTestCase(String name, RuntimeException exception) {
            this.name = name;
            this.exception = exception;
        }

        public String getName() {
            return name;
        }

        protected void runTest() throws Throwable {
            throw new RuntimeException("Could not construct test: " + name, exception);
        }
    }
}
