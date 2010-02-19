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
    private final boolean showPrompt;

    public REPL(InputStream in, PrintStream out, PrintStream err, boolean showPrompt) {
        is = in;
        os = out;
        errs = err;
        this.showPrompt = showPrompt;
    }

    public static void main(String[] args) {
        boolean quiet = args.length >= 1 && args[0].equals("-q");
        REPL repl = new REPL(System.in, System.out, System.err, !quiet);
        repl.run();
    }

    public void run() {
        Reader inReader = new InputStreamReader(is);
        TokenStream inTokens = new TokenStream(inReader);
        Runtime runtime = new Runtime();
        runtime.bind("display", getDisplay(os));
        while (true) {
            try {
                if (showPrompt) {
                    os.print("dreme> ");
                    os.flush();
                }
                List inputForm = parser.parse(inTokens);
                if (inputForm == null) {
                    os.println();
                    os.flush();
                    return;
                }
                SchemeObject result = runtime.run(inputForm);
                if (result != Unspecified.INSTANCE)
                    os.println(result);
            }
            catch (RuntimeException e) {
                os.println("ERROR: " + e.getClass().getSimpleName() + ": " + e.getMessage());
            }
            catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private static Procedure getDisplay(final PrintStream out) {
        return new Procedure() {
            protected SchemeObject apply(List arguments, Environment environment) {
                out.println(arguments.head());
                return Unspecified.INSTANCE;
            }
        };
    }
}
