package dreme;

import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;
import java.util.List;

public class Classpath {
    private final ClassLoader classLoader;

    public Classpath(ClassLoader classLoader) {
        this.classLoader = classLoader;
    }

    public Reader getResource(String name) {
        return new InputStreamReader(classLoader.getResourceAsStream(name));
    }

    /**
     * Finds resource files on the classpath which are directly under the specified path
     * and have a file name matching the provided suffix. If suffix is null, all such files
     * are returned.
     */
    public List<String> findResourcesMatching(String pathPrefix, String suffix) {
        List<String> result = new ArrayList<String>();
        if (!pathPrefix.endsWith("/")) pathPrefix = pathPrefix + "/";
        FilenameFilter filenameFilter = getFileFilter(suffix);
        try {
            List<URL> paths = Collections.list(classLoader.getResources(pathPrefix));
            for (URL url : paths) {
                if (!url.getProtocol().equals("file")) continue;
                File directory = new File(url.toURI());
                if (!directory.exists() || !directory.isDirectory()) continue;
                for (String filename : directory.list(filenameFilter)) {
                    if (classLoader.getResource(pathPrefix + filename) != null)
                        result.add(pathPrefix + filename);
                }
            }
        }
        catch (IOException e) {
            throw new RuntimeException(e);
        }
        catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
        return result;
    }

    private FilenameFilter getFileFilter(final String suffix) {
        if (suffix == null)
            return null;
        return new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return name.endsWith(suffix);
            }
        };
    }
}
