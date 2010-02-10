package dreme;

import java.io.*;
import dreme.runtime.Runtime;

/**
 * REPL for the Dreme runtime
 */
public class REPL implements Runnable {

    private final Parser parser = new Parser();
    private final InputStream is;
    private final PrintStream os;
    private final PrintStream errs;

    public REPL(InputStream in, PrintStream out, PrintStream err) {
        is = in;
        os = out;
        errs = err;
    }

    public static void main(String[] args) {
        REPL repl = new REPL(System.in, System.out, System.err);
        repl.run();
    }

    public void run() {
        Reader inReader = new InputStreamReader(is);
        TokenStream inTokens = new TokenStream(inReader);
        Runtime runtime = new Runtime();
        while (true) {
            try {
                os.print("dreme> ");
                List inputForm = parser.parse(inTokens);
                os.println(runtime.run(inputForm));
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}
