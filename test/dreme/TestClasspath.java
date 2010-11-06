package dreme;

import junit.framework.TestCase;

import java.util.List;

public class TestClasspath extends TestCase {
    public void testScanForTests() throws Exception {
        Classpath scanner = new Classpath(getClass().getClassLoader());
        List<String> resources = scanner.findResourcesMatching("dreme", ".scm");
        assertTrue(resources.contains("dreme/builtin-tests.scm"));
        assertTrue(resources.contains("dreme/list-operations-tests.scm"));
    }
}
